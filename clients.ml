(* Copyright (C) 2016, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

open Lwt.Infix

let src = Logs.Src.create "client_eth" ~doc:"Ethernet networks for NetVM clients"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  mutable vif_of_ip : Vif.t Ipaddr.V4.Map.t;
  update : unit Lwt_condition.t; (* Fires when [iface_of_ip] changes. *)
  default_gateway : Ipaddr.V4.t; (* The IP that clients are given as their default gateway. *)
}

type host =
  [ `Client of Vif.t
  | `Firewall
  | `External of Ipaddr.t ]

let create config =
  let update = Lwt_condition.create () in
  let default_gateway = config.Dao.ip in
  { vif_of_ip = Ipaddr.V4.Map.empty; default_gateway; update }

let default_gateway t = t.default_gateway

let add_client t vif =
  let (_, ip) = vif.Vif.ipaddr in
  let rec go () =
    match Ipaddr.V4.Map.find_opt ip t.vif_of_ip with
    | Some old ->
      (* Wait for old client to disappear before adding one with the same IP address.
         Otherwise, its [rem_client] call will remove the new client instead. *)
      Log.info (fun m -> m "Waiting for old client %a to go away \
        before accepting new one" Vif.pp old);
      Lwt_condition.wait t.update >>= go
    | None ->
      t.vif_of_ip <- Ipaddr.V4.Map.add ip vif t.vif_of_ip;
      Lwt_condition.broadcast t.update ();
      Lwt.return_unit
  in
  go ()

let rem_client t vif =
  let (_, ip) = vif.Vif.ipaddr in
  assert (Ipaddr.V4.Map.mem ip t.vif_of_ip);
  t.vif_of_ip <- Ipaddr.V4.Map.remove ip t.vif_of_ip;
  Lwt_condition.broadcast t.update ()

let lookup t ip = Ipaddr.V4.Map.find_opt ip t.vif_of_ip

let classify t ip =
  match ip with
  | Ipaddr.V6 _ -> `External ip
  | Ipaddr.V4 ipv4 ->
    if ipv4 = t.default_gateway then `Firewall
    else match lookup t ipv4 with
      | Some vif -> `Client vif
      | None -> `External ip

let resolve t : host -> Ipaddr.t = function
  | `Client vif -> Ipaddr.V4 (snd vif.Vif.ipaddr)
  | `Firewall -> Ipaddr.V4 t.default_gateway
  | `External addr -> addr

module ARP = struct
  type arp =
    { net : t
    ; vif : Vif.t }

  let lookup t ip =
    if ip = t.net.default_gateway then Some (fst t.vif.Vif.mac)
    else if (Ipaddr.V4.to_octets ip).[3] = '\x01' then begin
      Log.info (fun f -> f "Request for %a is invalid, but pretending it's me \
        (see Qubes issue #5022)" Ipaddr.V4.pp ip);
      Some (fst t.vif.Vif.mac)
    end else None
  (* We're now treating client networks as point-to-point links,
     so we no longer respond on behalf of other clients. *)
    (*
    else match Ipaddr.Map.find ip t.net.iface_of_ip with
    | Some client_iface -> Some client_iface#other_mac
    | None -> None
     *)

  let create ~net vif = { net; vif; }

  let input_query t arp =
    let req_ipv4 = arp.Arp_packet.target_ip in
    let pf (f : ?header:string -> ?tags:_ -> _) fmt =
      f ("who-has %a? " ^^ fmt) Ipaddr.V4.pp req_ipv4
    in
    if req_ipv4 = snd t.vif.Vif.ipaddr then begin
      Log.info (fun f -> pf f "ignoring request for client's own IP");
      None
    end else match lookup t req_ipv4 with
      | None ->
        Log.info (fun f -> pf f "unknown address; not responding");
        None
      | Some req_mac ->
        Log.info (fun f -> pf f "responding with %a" Macaddr.pp req_mac);
        let open Arp_packet in
        Some { operation = Arp_packet.Reply
               (* The Target Hardware Address and IP are copied from the request *)
             ; target_ip = arp.Arp_packet.source_ip
             ; target_mac = arp.Arp_packet.source_mac
             ; source_ip = req_ipv4
             ; source_mac = req_mac
             }

  let input_gratuitous t arp =
    let source_ip = arp.Arp_packet.source_ip in
    let source_mac = arp.Arp_packet.source_mac in
    match lookup t source_ip with
    | Some real_mac when Macaddr.compare source_mac real_mac = 0 ->
      Log.info (fun f -> f "client suggests updating %s -> %s (as expected)"
        (Ipaddr.V4.to_string source_ip) (Macaddr.to_string source_mac));
    | Some other_mac ->
      Log.warn (fun f -> f "client suggests incorrect update %s -> %s (should be %s)"
        (Ipaddr.V4.to_string source_ip)
        (Macaddr.to_string source_mac)
        (Macaddr.to_string other_mac));
    | None ->
      Log.warn (fun f -> f "client suggests incorrect update %s -> %s (unexpected IP)"
        (Ipaddr.V4.to_string source_ip)
        (Macaddr.to_string source_mac))

  let input t arp =
    let op = arp.Arp_packet.operation in
    match op with
    | Arp_packet.Request -> input_query t arp
    | Arp_packet.Reply -> input_gratuitous t arp; None
end
