module Netbackend = Backend.Make (Xenstore.Make (Xen_os.Xs))

(* NOTE(dinosaure): [Backend] comes from [mirage-net-xen]

   Client_ethernet is:

   [unikernel] <-[client_ethernet0]-> [client0]
               <-[client_ethernet.]-> [client.]
               <-[client_ethernetn]-> [clientN]
*)
module Client_ethernet = Ethernet.Make (Netbackend)
module Underlying_arp = Arp.Make (Client_ethernet)

module Client_arp = struct
  type t =
    { arp : Underlying_arp.t
    ; your_ip : Ipaddr.V4.t
    ; my_mac : Macaddr.t
    ; your_mac : Macaddr.t }

  let connect ~my_mac your_ip ~your_mac ethernet =
    let open Lwt.Syntax in
    let* arp = Underlying_arp.connect ethernet in
    Lwt.return { arp ; your_ip ; my_mac ; your_mac }

  type error = Underlying_arp.error
  let pp_error = Underlying_arp.pp_error

  let disconnect _ = Lwt.return_unit
  let get_ips t = Underlying_arp.get_ips t.arp
  let set_ips t ips = Underlying_arp.set_ips t.arp ips
  let remove_ip t ip = Underlying_arp.remove_ip t.arp ip
  let add_ip t ip = Underlying_arp.add_ip t.arp ip
  let input t buf = Underlying_arp.input t.arp buf
  let pp ppf _ = Fmt.string ppf "arp"

  let query t ip =
    if Ipaddr.V4.compare t.your_ip ip = 0
    then Lwt.return_ok t.your_mac else Lwt.return_ok t.my_mac
end

module Client_ip = Static_ipv4.Make(Client_ethernet)(Client_arp)

type t =
  { ipaddr : Ipaddr.V4.t * Ipaddr.V4.t
  ; mac : Macaddr.t * Macaddr.t
  ; ethernet : Client_ethernet.t
  ; arp : Client_arp.t
  ; ip : Client_ip.t
  ; domid : int }

let make backend { Dao.Client_vif.domid; _ } ~gateway ipaddr =
  let open Lwt.Syntax in
  let* ethernet = Client_ethernet.connect backend in
  let ((my_mac, your_mac) as mac) =
    (Client_ethernet.mac ethernet, Netbackend.frontend_mac backend)
  in
  let ((_my_ip, your_ip) as ipaddr) = (gateway, ipaddr) in
  let* arp = Client_arp.connect ~my_mac your_ip ~your_mac ethernet in
  let* ip = Client_ip.connect ~cidr:(Ipaddr.V4.Prefix.make 0 your_ip) ~gateway ethernet arp in
  Lwt.return { mac; ip; ethernet; arp; ipaddr; domid }

let pp ppf { ipaddr = _, ip; mac = _, mac; domid; _ } =
  Fmt.pf ppf "dom%d:%a[%a]" domid Ipaddr.V4.pp ip Macaddr.pp mac
