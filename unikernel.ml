open Qubes

let ( let* ) = Lwt.bind

module Main
    (R : Mirage_random.S)
    (M : Mirage_clock.MCLOCK)
    (P : Mirage_clock.PCLOCK)
    (T : Mirage_time.S)
    (S : Tcpip.Stack.V4V6)
    (KV : Mirage_kv.RO) =
struct
  module O = Miragevpn_mirage.Make (R) (M) (P) (T) (S)

  type t = { ovpn : O.t; table : Mirage_nat_lru.t; clients : Clients.t }

  let log = Logs.Src.create "nat"

  module Log = (val Logs.src_log log : Logs.LOG)
  module Private_routing = Routing.Make (Log) (A)

  let add_rule t packet =
    let public_ip = O.get_ip t.ovpn in
    match
      Mirage_nat_lru.add t.table packet public_ip
        (fun () -> Some (Randomconv.int16 R.generate))
        `NAT
    with
    | Error err ->
        Logs.debug (fun m ->
            m "Failed to add a NAT rule: %a" Mirage_nat.pp_error err)
    | Ok () -> ()

  let output_tunnel t packet =
    match Nat_packet.to_cstruct ~mtu:(O.mtu t.ovpn) packet with
    | Ok pkts ->
        let* res =
          Lwt_list.fold_left_s
            (fun r p -> if r then O.write t.ovpn p else Lwt.return r)
            true pkts
        in
        if not res then Logs.err (fun m -> m "Failed to write data via tunnel");
        Lwt.return_unit
    | Error err ->
        Logs.err (fun m ->
            m "Nat_packet.to_cstruct failed: %a" Nat_packet.pp_error err);
        Lwt.return_unit

  let rec ingest_private ~ipaddr t packet =
    let (`IPv4 ({ Ipv4_packet.dst; _ }, _) : Nat_packet.t) = packet in
    if Ipaddr.V4.compare dst ipaddr = 0 then Lwt.return_unit
    else
      match Mirage_nat_lru.translate t.table packet with
      | Ok packet -> output_tunnel t packet
      | Error `Untranslated ->
          add_rule t packet;
          ingest_private ~ipaddr t packet
      | Error `TTL_exceeded ->
          Logs.warn (fun m -> m "TTL exceeded");
          Lwt.return_unit
  (* TODO(dinosaure): should report ICMP error message to src. *)

  let add_vif ~finalisers t ({ Dao.Client_vif.domid; device_id } as client_vif)
      ipaddr () =
    let open Lwt.Infix in
    let* backend = Vif.Netbackend.make ~domid ~device_id in
    let ip = Clients.default_gateway t.clients in
    let* vif = Vif.make backend client_vif ~ip ~client_ip:ipaddr in
    let* () = Clients.add_client t.clients vif in
    Finaliser.add
      ~finaliser:(fun () -> Clients.rem_client t.clients vif)
      finalisers;
    let cache = ref (Fragments.Cache.empty (256 * 1024)) in
    let listener =
      let fn () =
        let arpv4 = Vif.Client_arp.input vif.Vif.arp in
        let ipv4 p =
          let cache', res =
            Nat_packet.of_ipv4_packet !cache ~now:(M.elapsed_ns ()) p
          in
          cache := cache';
          match res with
          | Error err ->
              Logs.err (fun m ->
                  m "Listen for %a failed: %a" Vif.pp vif Nat_packet.pp_error
                    err);
              Lwt.return_unit
          | Ok None -> Lwt.return_unit
          | Ok (Some pkt) -> ingest_private ~ipaddr t pkt
        in
        let header_size = Ethernet.Packet.sizeof_ethernet
        and input =
          Vif.Client_ethernet.input ~arpv4 ~ipv4
            ~ipv6:(fun _ -> Lwt.return_unit)
            vif.Vif.ethernet
        in
        Vif.Netbackend.listen backend ~header_size input >>= function
        | Error err ->
            Logs.err (fun m ->
                m "Private interface %a stopped: %a" Vif.Netbackend.pp_error err
                  Vif.pp vif);
            Lwt.return_unit
        | Ok () ->
            Logs.debug (fun m ->
                m "Private interface %a terminated normally" Vif.pp vif);
            Lwt.return_unit
      in
      Lwt.catch fn @@ function
      | Lwt.Canceled -> Lwt.return_unit
      | exn -> Lwt.fail exn
    in
    Finaliser.add ~finaliser:(fun () -> Lwt.cancel listener) finalisers;
    Lwt.async (fun () -> Lwt.pick [ listener ]);
    Lwt.return finalisers

  let add_client t client_vif ipaddr =
    let finalisers = Finaliser.create () in
    Lwt.catch (add_vif t ~finalisers client_vif ipaddr) @@ function
    | exn ->
        Logs.warn (fun f ->
            f "Error with client %a: %s" Dao.Client_vif.pp client_vif
              (Printexc.to_string exn));
        Lwt.return finalisers

  let wait_clients t =
    let clients : Finaliser.t Dao.Vif_map.t ref = ref Dao.Vif_map.empty in
    Dao.watch_clients @@ fun m ->
    let clean_up_clients client_vif finalisers =
      if not (Dao.Vif_map.mem client_vif m) then (
        clients := Dao.Vif_map.remove client_vif !clients;
        Logs.info (fun f -> f "client %a has gone" Dao.Client_vif.pp client_vif);
        Finaliser.finalise finalisers)
    in
    let rec add_new_clients seq =
      match Seq.uncons seq with
      | Some ((client_vif, ipaddr), seq)
        when not (Dao.Vif_map.mem client_vif !clients) ->
          let* finalisers = add_client t client_vif ipaddr in
          Logs.debug (fun f ->
              f "client %a arrived" Dao.Client_vif.pp client_vif);
          clients := Dao.Vif_map.add client_vif finalisers !clients;
          add_new_clients seq
      | Some _ -> add_new_clients seq
      | None -> Lwt.return_unit
    in
    Dao.Vif_map.iter clean_up_clients !clients;
    add_new_clients (Dao.Vif_map.to_seq m)

  let output_private ~ipaddr_cidr eth arp packet =
    let (`IPv4 ({ Ipv4_packet.dst; _ }, _) : Nat_packet.t) = packet in
    let* res = Private_routing.destination_mac ipaddr_cidr None arp dst in
    match res with
    | Error dst ->
        let dst = match dst with `Local -> "local" | `Gateway -> "gateway" in
        Logs.err (fun m -> m "Could not send packet, error: %s" dst);
        Lwt.return_unit
    | Ok dst -> (
        let more = ref [] in
        let write buf =
          match Nat_packet.into_cstruct packet buf with
          | Ok (n, adds) ->
              more := adds;
              n
          | Error err ->
              Logs.err (fun m ->
                  m "Error %a while Nat_packet.into_cstruct" Nat_packet.pp_error
                    err);
              (* Vif.Client_ethernet.write takes a fill function
                 [Cstruct.t -> int], which can not result in an error. Now, if
                 Nat_packet results in an error (e.g. need to fragment, but
                 fragmentation is not allowed (don't fragment bit is set)), we
                 can't pass this information up the stack. Instead we log an
                 error and return 0 -- thus an empty Ethernet header will be
                 transmitted on the wire. *)
              (* TODO(dinosaure): an ICMP error should be sent to the packet
                 origin. *)
              0
        in
        let* res = Vif.Client_ethernet.write eth dst `IPv4 write in
        match res with
        | Ok () ->
            let write_more pkt =
              let size = Cstruct.length pkt in
              let write buf =
                Cstruct.blit pkt 0 buf 0 size;
                size
              in
              let* res = Vif.Client_ethernet.write eth dst `IPv4 ~size write in
              Lwt.return_unit
            in
            Lwt_list.iter_s write_more !more
        | Error _ -> Lwt.return_unit)

  let ingest_public cache now table cs =
    let cache', res = Nat_packet.of_ipv4_packet !cache ~now cs in
    cache := cache';
    match res with
    | Error err ->
        Logs.err (fun m -> m "%a" Nat_packet.pp_error err);
        Lwt.return_unit
    | Ok None -> Lwt.return_unit
    | Ok (Some packet) -> (
        match Mirage_nat_lru.translate table packet with
        | Ok packet ->
            let eth = assert false in
            (* TODO *)
            let arp = assert false in
            let ipaddr_cidr = assert false in
            output_private ~ipaddr_cidr eth arp packet
        | Error err -> Lwt.return_unit)

  let rec listen_ovpn t cache =
    let* datas = O.read t.ovpn in
    let fn = ingest_public cache (M.elapsed_ns ()) t.table in
    let* () = Lwt_list.iter_p fn datas in
    listen_ovpn t cache

  let listen_ovpn t =
    let cache = ref (Fragments.Cache.empty (256 * 1024)) in
    listen_ovpn t cache

  let openvpn_configuration disk =
    let* contents = KV.get disk (Mirage_kv.Key.v "/config.ovpn") in
    match contents with
    | Error _ -> Fmt.failwith "No OpenVPN configuration found"
    | Ok contents -> (
        let string_of_file _ = Error (`Msg "Impossible to load extra files") in
        match Miragevpn.Config.parse_client ~string_of_file contents with
        | Ok cfg -> Lwt.return cfg
        | Error _ -> Fmt.failwith "Invalid OpenVPN configuration")

  let start _random _mclock _pclock _time vif0 disk =
    let* _qrexec = RExec.connect ~domid:0 () in
    let* qubesDB = DB.connect ~domid:0 () in
    let* cfg = Dao.read_network_config qubesDB in
    let clients = Clients.create cfg in
    let* config = openvpn_configuration disk in
    let* ovpn = O.connect config vif0 in
    match ovpn with
    | Error (`Msg msg) -> failwith msg
    | Ok ovpn ->
        let table =
          Mirage_nat_lru.empty ~tcp_size:1024 ~udp_size:1024 ~icmp_size:20
        in
        let t = { ovpn; table; clients } in
        Lwt.pick [ wait_clients t; listen_ovpn t ]
end
