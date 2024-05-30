open Qubes

let ( let* ) = Lwt.bind
let ( % ) f g = fun x -> f (g x)

module Main
    (R : Mirage_random.S)
    (M : Mirage_clock.MCLOCK)
    (P : Mirage_clock.PCLOCK)
    (T : Mirage_time.S)
    (DB : Qubes.S.DB)
    (S : Tcpip.Stack.V4V6)
    (KV : Mirage_kv.RO) =
struct
  module O = Miragevpn_mirage.Make (R) (M) (P) (T) (S)

  type t =
    { ovpn : O.t
    ; table : Mirage_nat_lru.t
    ; mutable oc_fragments : Fragments.Cache.t
    ; oc : Nat_packet.t Lwt_stream.t * (Nat_packet.t option -> unit)
    ; ic : (Vif.t * Nat_packet.t) Lwt_stream.t * ((Vif.t * Nat_packet.t) option -> unit)
    ; clients : Clients.t }

  module Nat = struct
    let fail_to_parse ~protocol ~payload =
      Result.iter_error @@ fun msg ->
      Logs.debug (fun m -> m "Failed to parse %s packet: %s@.%a" protocol msg Cstruct.hexdump_pp payload)

    let of_ipv4 hdr payload = match Ipv4_packet.(Unmarshal.int_to_protocol hdr.proto) with
      | Some `TCP ->
        let value = Tcp.Tcp_packet.Unmarshal.of_cstruct payload in
        let fn (tcp, payload) = `IPv4 (hdr, `TCP (tcp, payload)) in
        fail_to_parse ~protocol:"TCP" ~payload value;
        Option.map fn (Result.to_option value)
      | Some `UDP ->
        let value = Udp_packet.Unmarshal.of_cstruct payload in
        let fn (udp, payload) = `IPv4 (hdr, `UDP (udp, payload)) in
        fail_to_parse ~protocol:"UDP" ~payload value;
        Option.map fn (Result.to_option value)
      | Some `ICMP ->
        let value = Icmpv4_packet.Unmarshal.of_cstruct payload in
        let fn (hdr', payload) = `IPv4 (hdr, `ICMP (hdr', payload)) in
        fail_to_parse ~protocol:"ICMP" ~payload value;
        Option.map fn (Result.to_option value)
      | _ ->
        Logs.debug (fun m -> m "Ignoring non-TCP/UDP/ICMP packet: %a" Ipv4_packet.pp hdr);
        None

    let payload_to_buffer pkt =
      let `IPv4 (hdr, protocol) = pkt in
      let src = hdr.Ipv4_packet.src and dst = hdr.Ipv4_packet.dst in
      Logs.debug (fun m -> m "Packet src:%a => dst:%a" Ipaddr.V4.pp src Ipaddr.V4.pp dst);
      match protocol with
      | `ICMP (hdr', payload) ->
        let buf = Cstruct.create (Icmpv4_wire.sizeof_icmpv4 + Cstruct.length payload) in
        Cstruct.blit payload 0 buf Icmpv4_wire.sizeof_icmpv4 (Cstruct.length payload);
        let res = Icmpv4_packet.Marshal.into_cstruct hdr' ~payload buf in
        if Result.is_ok res then Some (buf, `ICMP, hdr) else None
      | `UDP (hdr', payload) ->
        let buf = Cstruct.create (Udp_wire.sizeof_udp + Cstruct.length payload) in
        Cstruct.blit payload 0 buf Udp_wire.sizeof_udp (Cstruct.length payload);
        let pseudoheader= Ipv4_packet.Marshal.pseudoheader ~src ~dst ~proto:`UDP
          (Cstruct.length payload + Udp_wire.sizeof_udp) in
        let res = Udp_packet.Marshal.into_cstruct ~pseudoheader ~payload hdr' buf in
        if Result.is_ok res then Some (buf, `UDP, hdr) else None
      | `TCP (hdr', payload) ->
        let opt = Tcp.Options.lenv hdr'.Tcp.Tcp_packet.options in
        let dst_off = Tcp.Tcp_wire.sizeof_tcp + opt in
        let buf = Cstruct.create (dst_off + Cstruct.length payload) in
        Cstruct.blit payload 0 buf dst_off (Cstruct.length payload);
        let pseudoheader = Ipv4_packet.Marshal.pseudoheader ~src ~dst ~proto:`TCP
          (Cstruct.length payload + dst_off) in
        let res = Tcp.Tcp_packet.Marshal.into_cstruct ~pseudoheader hdr' ~payload buf in
        if Result.is_ok res then Some (buf, `TCP, hdr) else None

    let output_private t packet =
      match payload_to_buffer packet with
      | None ->
        Logs.warn (fun m -> m "couldn't encode packet");
        Lwt.return_unit
      | Some (buf, proto, hdr) ->
        let ipaddr = hdr.Ipv4_packet.dst in
        match Clients.lookup t.clients ipaddr with
        | Some vif ->
          Logs.debug (fun m -> m "Sending a packet to %a" Ipaddr.V4.pp ipaddr);
          let* r = Vif.Client_ip.write vif.Vif.ip ~ttl:hdr.Ipv4_packet.ttl
            ~src:hdr.Ipv4_packet.src hdr.Ipv4_packet.dst proto (fun _ -> 0) [ buf ] in
          (match r with
          | Ok _ -> ();
          | Error e -> Logs.warn (fun m -> m "error while sending: %a" Vif.Client_ip.pp_error e));
          Lwt.return_unit
        | None -> Logs.warn (fun m -> m "%a does not exist as a client" Ipaddr.V4.pp ipaddr); Lwt.return_unit
  end

  let local_network a b = Ipaddr.V4.compare a b = 0

  let add_vif ~finalisers t ({ Dao.Client_vif.domid; device_id } as client_vif)
      ipaddr () =
    let open Lwt.Infix in
    let* backend = Vif.Netbackend.make ~domid ~device_id in
    let ic_fragments = ref (Fragments.Cache.empty (256 * 1024)) in
    let ic = Lwt_stream.create () in
    let gateway = Clients.default_gateway t.clients in
    let* vif = Vif.make backend client_vif ~gateway ipaddr in
    let* () = Clients.add_client t.clients vif in
    let should_be_routed hdr =
      local_network ipaddr hdr.Ipv4_packet.src
      && not (local_network ipaddr hdr.Ipv4_packet.dst) in
    Finaliser.add
      ~finaliser:(fun () -> Clients.rem_client t.clients vif)
      finalisers;
    let listener =
      let fn () =
        let arpv4 = Vif.Client_arp.input vif.Vif.arp in
        let ipv4 payload =
          match Ipv4_packet.Unmarshal.of_cstruct payload with
          | Error msg ->
            Logs.err (fun m -> m "Couldn't decode IPv4 packet %s: %a" msg Cstruct.hexdump_pp payload)
          | Ok (hdr, payload) when should_be_routed hdr ->
            let now = M.elapsed_ns () in
            let fragments, packet = Fragments.process !ic_fragments now hdr payload in
            let packet = Option.bind packet (fun (hdr, payload) -> Nat.of_ipv4 hdr payload) in
            ic_fragments := fragments;
            Fun.flip Option.iter packet (snd ic % Option.some)
          | Ok (hdr, _) ->
            Logs.warn (fun m -> m "Ignoring IPv4 packet which should not be routed (IP header: %a)" Ipv4_packet.pp hdr)
        in
        let ipv4 payload = ipv4 payload; Lwt.return_unit in
        let header_size = Ethernet.Packet.sizeof_ethernet
        and input =
          Vif.Client_ethernet.input ~arpv4 ~ipv4
            ~ipv6:(fun _ -> Lwt.return_unit)
            vif.Vif.ethernet
        in
        Logs.debug (fun m -> m "%a starts to listen packets" Vif.pp vif);
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
    let rec transmit =
      let rec fn () =
        let open Lwt.Syntax in
        Lwt_stream.get (fst ic) >>= function
        | Some packet -> (snd t.ic) (Some (vif, packet)); fn ()
        | None -> Lwt.return_unit in
      fn () in
    Finaliser.add ~finaliser:(fun () -> Lwt.cancel transmit) finalisers;
    Lwt.async (fun () -> Lwt.pick [ listener; transmit ]);
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
    Logs.debug (fun m -> m "The network topology was updated");
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
      | Some (_, seq) -> add_new_clients seq
      | None -> Lwt.return_unit
    in
    Logs.debug (fun m -> m "Clean-up clients");
    Dao.Vif_map.iter clean_up_clients !clients;
    let open Lwt.Infix in
    Logs.debug (fun m -> m "Add new clients");
    add_new_clients (Dao.Vif_map.to_seq m) >|= fun () ->
    Logs.debug (fun m -> m "The unikernel is in-sync with the network topology")

  let rec packets_to_clients t =
    let* packet = Lwt_stream.get (fst t.oc) in
    let packet = Option.get packet in
    let* () = Nat.output_private t packet in
    packets_to_clients t

  (* OpenVPN packets to clients ([t.oc]) *)
  let ingest_public push table fragments css =
    let now = M.elapsed_ns () in
    let fold fragments cs = match Ipv4_packet.Unmarshal.of_cstruct cs with
      | Error msg ->
        Logs.err (fun m -> m "Failed to decode IPv4 packet from OpenVPN: %s: %a"
          msg Cstruct.hexdump_pp cs);
        Lwt.return fragments
      | Ok (hdr, payload) ->
        let fragments, packet = Fragments.process fragments now hdr payload in
        let packet = Nat.of_ipv4 hdr payload in
        let packet = Option.map (Mirage_nat_lru.translate table) packet in
        let packet = Option.map Result.to_option packet in
        Option.iter (push % Option.some) (Option.join packet);
        Lwt.return fragments in
    Lwt_list.fold_left_s fold fragments css
        
  let rec ovpn_loop t =
    let* css = O.read t.ovpn in
    Logs.debug (fun m -> m "Got %d packet(s) from OpenVPN" (List.length css));
    List.iter (fun cs -> Logs.debug (fun m -> m "%a" Cstruct.hexdump_pp cs)) css;
    let* fragments = ingest_public (snd t.oc) t.table t.oc_fragments css in
    t.oc_fragments <- fragments;
    ovpn_loop t

  let output_tunnel t vif packet =
    match Nat_packet.to_cstruct ~mtu:(O.mtu t.ovpn - Ipv4_wire.sizeof_ipv4) packet with
    | Ok pkts ->
        let* res =
          Lwt_list.fold_left_s
            (fun r p -> if r then O.write t.ovpn p else Lwt.return r)
            true pkts
        in
        if not res then Logs.err (fun m -> m "Failed to write data via tunnel")
        else Logs.debug (fun m -> m "Packet from %a sent" Vif.pp vif);
        Lwt.return_unit
    | Error err ->
        Logs.err (fun m -> m "Nat_packet.to_cstruct failed: %a" Nat_packet.pp_error err);
        Lwt.return_unit

  (* clients packets ([t.ic]) to OpenVPN server *)
  let rec ingest_private t =
    let* packet = Lwt_stream.get (fst t.ic) in
    let vif, packet = Option.get packet in
    match Mirage_nat_lru.translate t.table packet with
    | Ok packet -> let* () = output_tunnel t vif packet in ingest_private t
    | Error `TTL_exceeded ->
      Logs.warn (fun m -> m "TTL exceeded");
      ingest_private t
    | Error `Untranslated ->
      begin match Mirage_nat_lru.add t.table packet (O.get_ip t.ovpn)
        (fun () -> Some (Randomconv.int16 R.generate)) `NAT with
      | Error err ->
        Logs.debug (fun m -> m "Failed to add a NAT rule: %a" Mirage_nat.pp_error err);
        ingest_private t
      | Ok () -> match Mirage_nat_lru.translate t.table packet with
        | Ok packet -> let* () = output_tunnel t vif packet in ingest_private t
        | Error `Untranslated ->
          Logs.warn (fun m -> m "Can't translate packet, giving up");
          ingest_private t
        | Error `TTL_exceeded ->
          Logs.warn (fun m -> m "TTL exceeded");
          (* TODO(dinosaure): should report ICMP error message to src. *)
          ingest_private t end

  let openvpn_configuration disk =
    let* contents = KV.get disk (Mirage_kv.Key.v "/config.ovpn") in
    match contents with
    | Error _ -> Fmt.failwith "No OpenVPN configuration found"
    | Ok contents -> (
        let string_of_file _ = Error (`Msg "Impossible to load extra files") in
        match Miragevpn.Config.parse_client ~string_of_file contents with
        | Ok cfg -> Lwt.return cfg
        | Error _ -> Fmt.failwith "Invalid OpenVPN configuration")

  let start _random _mclock _pclock _time qubesDB vif0 disk =
    Logs.debug (fun m -> m "Start the unikernel");
    let shutdown =
      let* value = Xen_os.Lifecycle.await_shutdown_request () in
      match value with `Poweroff | `Reboot -> Lwt.return_unit in
    let* cfg = Dao.read_network_config qubesDB in
    Logs.debug (fun m -> m "ip:%a, gateway:%a, dns: %a & %a"
      Ipaddr.V4.pp cfg.Dao.ip
      Ipaddr.V4.pp cfg.Dao.gateway
      Ipaddr.V4.pp (fst cfg.Dao.dns)
      Ipaddr.V4.pp (snd cfg.Dao.dns));
    let clients = Clients.create cfg in
    let* config = openvpn_configuration disk in
    Logs.debug (fun m -> m "OpenVPN configuration loaded");
    let* ovpn = O.connect config vif0 in
    match ovpn with
    | Error (`Msg msg) -> failwith msg
    | Ok ovpn ->
        Logs.debug (fun m -> m "Connected to the OpenVPN server: %a" Ipaddr.V4.pp (O.get_ip ovpn));
        let table = Mirage_nat_lru.empty ~tcp_size:1024 ~udp_size:1024 ~icmp_size:20 in
        let t =
          { ovpn
          ; table
          ; oc_fragments= Fragments.Cache.empty (256 * 1024)
          ; oc= Lwt_stream.create ()
          ; ic= Lwt_stream.create ()
          ; clients } in
      Lwt.pick [ shutdown; wait_clients t; ovpn_loop t; ingest_private t; packets_to_clients t ]
end
