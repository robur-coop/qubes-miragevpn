(* Copyright (C) 2015, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

(** Wrapper for XenStore and QubesDB databases. *)

module Client_vif : sig
  type t = { domid : int; device_id : int }

  val pp : t Fmt.t
end

module Vif_map : sig
  include Map.S with type key = Client_vif.t
end

val watch_clients : (Ipaddr.V4.t Vif_map.t -> unit Lwt.t) -> 'a Lwt.t
(** [watch_clients fn] calls [fn clients] with the list of backend clients
    in XenStore, and again each time XenStore updates. *)

type network_config = {
  ip : Ipaddr.V4.t; (* The IP address of our interface to NetVM *)
  gateway : Ipaddr.V4.t; (* The IP address of NetVM (our gateway) *)
  dns : Ipaddr.V4.t * Ipaddr.V4.t;
}

val read_network_config : Qubes.DB.t -> network_config Lwt.t
(** [read_network_config db] fetches the configuration from QubesDB.
    If it isn't there yet, it waits until it is. *)

val db_root : Ipaddr.V4.t -> string
(** Returns the root path of the firewall rules in the QubesDB for a given IP address. *)

val print_network_config : network_config -> unit
