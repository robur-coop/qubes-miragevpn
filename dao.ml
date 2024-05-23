(* Copyright (C) 2015, Thomas Leonard <thomas.leonard@unikernel.com>
   See the README file for details. *)

open Lwt.Infix
open Qubes

let src = Logs.Src.create "dao" ~doc:"QubesDB data access"

module Log = (val Logs.src_log src : Logs.LOG)

(* XXX(dinosaure):
   {[
     @dom0$ xenstore-ls /local/domain/<unikernel-id>
     ...
     backend = ""
       vif = ""
         <domid> = ""
           <deviceid> = ""
             frontend = "/local/domain/<domid>/device/vif/<deviceid>"
             frontend-id = "<domainid>"
             mac = "..."
             ip = "..."
   ]} *)
module Client_vif = struct
  type t = { domid : int; device_id : int }

  let pp f { domid; device_id } =
    Fmt.pf f "/local/domain/%d/device/vif/%d" domid device_id

  let compare = Stdlib.compare
end

module Vif_map = struct
  include Map.Make (Client_vif)

  let rec of_list = function
    | [] -> empty
    | (k, v) :: rest -> add k v (of_list rest)
end

let directory ~handle dir =
  Xen_os.Xs.directory handle dir >|= function [ "" ] -> [] | items -> items

let db_root client_ip = "/qubes-firewall/" ^ Ipaddr.V4.to_string client_ip

let vifs client domid =
  let open Lwt.Syntax in
  match int_of_string_opt domid with
  | None ->
      Log.err (fun f -> f "Invalid domid %S" domid);
      Lwt.return []
  | Some domid ->
      let path = Fmt.str "backend/vif/%d" domid in
      let fn handle =
        let* entries = directory ~handle path in
        let fn device_id = match int_of_string_opt device_id with
          | None ->
            Log.err (fun m -> m "Invalid device ID %S for domid %d" device_id domid);
            Lwt.return_none
          | Some device_id ->
            let vif = { Client_vif.domid; device_id } in
            let fn () =
              let* str = Xen_os.Xs.read handle (Fmt.str "%s/%d/ip" path device_id) in
              let[@warning "-8"] ip :: _ = String.split_on_char ' ' str in
              Lwt.return_some (vif, Ipaddr.V4.of_string_exn ip) in
            Lwt.catch fn @@ function
            | Xs_protocol.Enoent _ -> Lwt.return_none
            | exn ->
              Log.err (fun m -> m "Error getting IP address of %a: %s"
              Client_vif.pp vif (Printexc.to_string exn));
              Lwt.return_none in
            Lwt_list.filter_map_p fn entries in
      Xen_os.Xs.immediate client fn

let watch_clients fn =
  let open Lwt.Syntax in
  Xen_os.Xs.make () >>= fun xs ->
  let backend_vifs = "backend/vif" in
  Log.info (fun f -> f "Watching %s" backend_vifs);
  let watch handle =
    let* items = Lwt.catch
      (fun () -> directory ~handle backend_vifs)
      (function Xs_protocol.Enoent _ -> Lwt.return [] | exn -> Lwt.fail exn) in
    let* xs = Xen_os.Xs.make () in
    let* items = Lwt_list.map_p (vifs xs) items in
    fn (List.concat items |> Vif_map.of_list);
    Lwt.fail Xs_protocol.Eagain in
  Xen_os.Xs.wait xs watch

type network_config =
  { ip : Ipaddr.V4.t (* The IP address of our interface to NetVM *)
  ; gateway : Ipaddr.V4.t (* The IP address of NetVM (our gateway) *)
  ; dns : Ipaddr.V4.t * Ipaddr.V4.t }

exception Missing_key of string

let try_read_network_config db =
  let get name =
    match DB.KeyMap.find_opt name db with
    | None -> raise (Missing_key name)
    | Some value -> Ipaddr.V4.of_string_exn value
  in
  let ip = get "/qubes-ip" in
  let gateway = get "/qubes-gateway" in
  let dns = get "/qubes-primary-dns", get "/qubes-secondary-dns" in
  { ip; gateway; dns; }

let read_network_config qubesDB =
  let rec go bindings =
    try Lwt.return (try_read_network_config bindings)
    with Missing_key key ->
      Log.warn (fun f ->
          f "QubesDB key %S not (yet) present; waiting for QubesDB to change..."
            key);
      DB.after qubesDB bindings >>= go
  in
  go (DB.bindings qubesDB)

let print_network_config config =
  Log.info (fun f ->
      f
        "@[<v2>Current network configuration (QubesDB or command line):@,\
         NetVM IP on uplink network: %a@,\
         Our IP on client networks:  %a@,\
         DNS primary resolver:       %a@,\
         DNS secondary resolver:     %a@]"
         Ipaddr.V4.pp config.gateway
        Ipaddr.V4.pp config.ip Ipaddr.V4.pp (fst config.dns) Ipaddr.V4.pp
        (snd config.dns))

let set_iptables_error db = Qubes.DB.write db "/qubes-iptables-error"
