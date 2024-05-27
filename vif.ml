module Netbackend = Backend.Make (Xenstore.Make (Xen_os.Xs))

(* NOTE(dinosaure): [Backend] comes from [mirage-net-xen]

   Client_ethernet is:

   [unikernel] <-[client_ethernet0]-> [client0]
               <-[client_ethernet.]-> [client.]
               <-[client_eitherntn]-> [clientN]

   Vif0 is:

   [sys-net] <-[vif0]-> [unikernel] *)
module Client_ethernet = Ethernet.Make (Netbackend)
module Client_arp = Arp.Make (Client_ethernet) (Xen_os.Time)
module Vif0 = Ethernet.Make (Netif)

type t = {
  ip : Ipaddr.V4.t * Ipaddr.V4.t (* my, our *);
  mac : Macaddr.t * Macaddr.t (* my, our *);
  ethernet : Client_ethernet.t;
  arp : Client_arp.t;
  domid : int;
}

let make backend { Dao.Client_vif.domid; device_id } ~ip ~client_ip =
  let open Lwt.Syntax in
  let* ethernet = Client_ethernet.connect backend in
  let* arp = Client_arp.connect ethernet in
  let ((_my_mac, _our_mac) as mac) =
    (Client_ethernet.mac ethernet, Netbackend.frontend_mac backend)
  in
  let ((_my_ip, _our_ip) as ip) = (ip, client_ip) in
  Lwt.return { mac; ip; ethernet; arp; domid }

let pp ppf { ip = _, ip; domid; _ } =
  Fmt.pf ppf "dom%d:%a" domid Ipaddr.V4.pp ip
