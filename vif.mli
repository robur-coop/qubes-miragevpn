module Netbackend : module type of Backend.Make (Xenstore.Make (Xen_os.Xs))
module Client_ethernet : module type of Ethernet.Make (Netbackend)
module Client_arp : Arp.S
module Client_ip : module type of Static_ipv4.Make (Client_ethernet) (Client_arp)

type t = {
  ipaddr : Ipaddr.V4.t * Ipaddr.V4.t;
  mac : Macaddr.t * Macaddr.t;
  ethernet : Client_ethernet.t;
  arp : Client_arp.t;
  ip : Client_ip.t;
  domid : int;
}

val make :
  Netbackend.t ->
  Dao.Client_vif.t ->
  gateway:Ipaddr.V4.t ->
  Ipaddr.V4.t ->
  t Lwt.t

val pp : t Fmt.t
