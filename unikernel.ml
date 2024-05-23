open Qubes

let ( let* ) = Lwt.bind

module Main
  (R:Mirage_random.S)
  (M:Mirage_clock.MCLOCK)
  (P:Mirage_clock.PCLOCK)
  (T:Mirage_time.S)
  (S:Tcpip.Stack.V4V6)
  (KV:Mirage_kv.RO) = struct
  type t =
    { ovpn : O.t
    ; mutable cache : Fragments.Cache.t
    ; table : Mirage_nat_lru.t
    ; clients : Clients.t }

  module A =  Arp.Make (Vif.Client_ethernet) (T)

  let add_rule t =
    let public_ip = O.get_ip t.ovpn in
    match Mirage_nat_lru.add t.table packet public_ip
      (fun () -> Some (Randomconv.int16 R.generate)) `NAT with
    | Error _err -> ()
    | Ok () -> ()

  let output_tunnel t packet =
    match Nat_packet.to_cstruct ~mtu:(O.mtu t.ovpn) packet with
    | Ok pkts ->
      let* res = Lwt_lit.fold_left_s (fun r p -> if r then O.write t.ovpn p else Lwt.return r) true pkts in
      if not res then Logs.err (fun m -> m "Failed to write data via tunnel");
      Lwt.return_unit
    | Error _err ->
      Lwt.return_unit

  let ingest_private t pkt =
    let `IPv4 ({ Ipv4_packet.dst; _ }, _) = dst in
    if Ipaddr.V4.compare dst (Ipaddr.V4.Prefix.address private_ip_cidr) = 0
    then Lwt.return_unit
    else match Mirage_nat_lru.translate t.table packet with
      | Ok packet -> output_tunnel t packet
      | Error `Untranslated -> add_rule t packet; ingest_private t pkt
      | Error `TTL_exceeded -> Lwt.return_unit (* TODO *)

  let add_vif ~finalisers t ({ Dao.Client_vif.domid; device_id; } as client_vif) ipaddr =
    let* backend = Vif.Netbackend.make ~domid ~device_id in
    let* vif = Vif.make backend client_vif ~ip ~client_ip in
    let* () = Clients.add_client t vif in
    Finaliser.add ~finaliser:(fun () -> Clients.rem_client t vif) finalisers;
    let listener () =
      let fn () =
        let* arp = A.connect vif.Vif.ethernet in
        let arpv4 = A.input arp in
        let ipv4 p =
          let cache', res = Nat_packet.of_ipv4_packet t.cache ~now:(M.elapsed_ns ()) p in
          t.cache <- cache';
          match res with
          | Error _err -> Lwt.return_unit
          | Ok None -> Lwt.return_unit
          | Ok (Some pkt) -> ingest_private t.table pkt in
        let header_size = Ethernet.Packet.sizeof_ethernet
        and input = Vif.Client_ethernet.input ~arpv4 ~ipv4
          ~ipv6:(fun _ -> Lwt.return_unit) vif.Vif.ethernet in
        Vif.Netbackend.listen backend ~header_size input >>= function
        | Error err ->
          Logs.err (fun m -> m "Private interface %a stopped: %a" Vif.Netbackend.pp err Vif.pp vif);
          Lwt.return_unit
        | Ok () ->
          Logs.debug (fun m -> m "Private interface %a terminated normally" Vif.pp vif);
          Lwt.return_unit in
        (*
        match Ethernet.Packet.of_cstruct frame with
        | Error err ->
          Logs.warn (fun f -> f "Invalid Ethernet frame: %s" err);
          Lwt.return_unit
        | Ok (eth, payload) ->
          match eth.Ethernet.Packet.ethertype with
          | `ARP -> Lwt.return_unit (* TODO *)
          | `IPv4 -> client_handle_ipv4 ovpn payload
          | `IPv6 -> Lwt.return_unit in
        *)
      Lwt.catch fn @@ function
      | Lwt.Cancelled -> Lwt.return_unit
      | exn -> Lwt.fail exn in
    Finaliser.add finalisers (fun () -> Lwt.cancel listener);
    Lwt.async listener;
    Lwt.return finalisers

  let add_client t client_vif ipaddr =
    let finalisers = Finaliser.create () in
    let* () =
      Lwt.catch (add_vif t ~finalisers client_vif ipaddr)
      @@ function
      | exn ->
        Logs.warn (fun f -> f "Error with client %a: %s" Dao.Client_vif.pp vif
          (Printexc.to_string exn));
        Lwt.return_unit in
    Lwt.return finalisers

  let wait_clients t =
    let clients = ref Finaliser.t Dao.Vif_map.t = ref Dao.Vif_map.empty in
    Dao.watch_clients @@ fun m ->
    let clean_up_clients client_vif finalisers =
      if not (Dao.Vif_map.mem client_vif m) then begin
        clients := Dao.Vif_map.remove client_vif !clients;
        Logs.info (fun f -> f "client %a has gone" Dao.Client_vif.pp client_vif);
        Finaliser.finalise finalisers
      end in
    let add_new_client seq = match Seq.uncons seq with
      | Some ((client_vif, ipaddr), seq) ->
        if not (Dao.Vif_map.mem client_vif !clients) then begin
          let* finalisers = add_client t client_vif ipaddr in
          Logs.debug (fun f -> f "client %a arrived" Dao.Client_vif.pp client_vif);
          clients := Dao.Vif_map.add client_vif finalisers clients;
          add_new_clients seq
        end add_new_clients seq
      | None -> add_new_clients seq in
    Dao.Vif_map.iter clean_up_clients !clients;
    add_new_clients (Dao.Vif_map.to_seq m)

  let openvpn_configuration disk =
    let* contents = KV.get disk (Mirage_kv.Key.v "/config.ovpn") in
    match contents with
    | Error _ -> Fmt.failwith "No OpenVPN configuration found"
    | Ok contents ->
      let string_of_file _ = Error (`Msg "Impossible to load extra files") in
      match Miragevpn.Config.parse_client ~string_of_file contents with
      | Ok cfg -> Lwt.return cfg
      | Error _ -> Fmt.failwith "Invalid OpenVPN configuration"

  let start _random _mclock _pclock _time vif0 disk =
    let* _qrexec = RExec.connect ~domid:0 () in
    let* qubesDB = DB.connect ~domid:0 () in
    let* cfg = Dao.read_network_config qubesDB in
    let* clients = Clients.create cfg in
    let* config = openvpn_configuration disk in
    let* ovpn = O.connect config vif0 in
    match ovpn with
    | Error (`Msg msg) -> failwith msg
    | Ok ovpn ->
      let cache = Fragments.Cache.empty (256 * 1024) in
      let table = Mirage_nat_lru.empty ~tcp_size:1024 ~udp_size:1024 ~icmp_size:20 in
      let t = { ovpn; cache; table; clients } in
      let* () = Dispatcher.wait_clients Clock.elapsed_ns dns_client dns in
      Lwt.return_unit
end
