(* Copyright (C) 2016, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

(** The ethernet networks connecting us to our client AppVMs.
    Note: each AppVM is on a point-to-point link, each link being considered to be a separate Ethernet network. *)

type t
(** A collection of clients. *)

type host =
  [ `Client of Vif.t
  | `Firewall
  | `External of Ipaddr.t ]
(* Note: Qubes does not allow us to distinguish between an external address and a
   disconnected client.
   See: https://github.com/talex5/qubes-mirage-firewall/issues/9#issuecomment-246956850 *)

val create : Dao.network_config -> t
val add_client : t -> Vif.t -> unit Lwt.t
val rem_client : t -> Vif.t -> unit
val default_gateway : t -> Ipaddr.V4.t
val classify : t -> Ipaddr.t -> host
val resolve : t -> host -> Ipaddr.t
val lookup : t -> Ipaddr.V4.t -> Vif.t option

module ARP : sig
  (** We already know the correct mapping of IP addresses to MAC addresses, so we never
      allow clients to update it. We log a warning if a client attempts to set incorrect
      information. *)

  type arp
  (** An ARP-responder for one client. *)

  val create : net:t -> Vif.t -> arp
  (** [create ~net client_link] is an ARP responder for [client_link].
      It answers only for the client's gateway address. *)

  val input : arp -> Arp_packet.t -> Arp_packet.t option
  (** Process one ethernet frame containing an ARP message.
      Returns a response frame, if one is needed. *)
end
