open Mirage

let block = block_of_xenstore_id "51712"
let fs = tar_kv_ro block

let main = main
  ~runtime_args:[]
  ~packages:[
    package "vchan" ~min:"4.0.2"
  ; package "shared-memory-ring" ~min:"3.0.0"
  ; package "mirage-net-xen" ~min:"2.1.3"
  ; package "mirage-qubes" ~min:"0.9.1"
  ; package "mirage-xen" ~min:"8.0.0"
  ; package "ipaddr"
  ; package "ethernet" ~min:"3.0.0"
  ; package "arp" ~min:"2.3.0" ~sublibs:["mirage"]
  ; package "miragevpn"
  ; package "mirage-nat" ~min:"3.0.0"
  ]
  "Unikernel.Main" (random @-> mclock @-> time @-> stackv4v6 @-> kv_ro @-> job)

let () =
  register "qubes-unikernel" [ main $ default_random $ default_monotonic_clock $ default_time
    $ generic_stackv4v6 default_network $ fs ]
