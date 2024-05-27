module Netbackend : module type of Backend.Make (Xenstore.Make (Xen_os.Xs))
module Client_ethernet : module type of Ethernet.Make (Netbackend)
module Client_arp : module type of Arp.Make (Client_ethernet) (Xen_os.Time)

type t = {
  ip : Ipaddr.V4.t * Ipaddr.V4.t (* my, our *);
  mac : Macaddr.t * Macaddr.t (* my, our *);
  ethernet : Client_ethernet.t;
  arp : Client_arp.t;
  domid : int;
}

val make :
  Netbackend.t ->
  Dao.Client_vif.t ->
  ip:Ipaddr.V4.t ->
  client_ip:Ipaddr.V4.t ->
  t Lwt.t

val pp : t Fmt.t
