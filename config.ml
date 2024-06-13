open Mirage

let block = block_of_xenstore_id "51712"
let config = tar_kv_ro block
let ethernet = etif default_network
let arp = arp ethernet
let ipv4 = ipv4_qubes default_qubesdb ethernet arp 
let ipv6 = create_ipv6 default_network ethernet
let ipv4_only = Runtime_arg.ipv4_only ~group:"sys-net" ()
let ipv6_only = Runtime_arg.ipv4_only ~group:"sys-net" ()
let stack = direct_stackv4v6 ~ipv4_only ~ipv6_only default_network ethernet arp ipv4 ipv6

let main =
  main ~runtime_args:[]
    ~packages:
      [
        package "vchan" ~min:"4.0.2";
        package "shared-memory-ring" ~min:"3.0.0";
        package "mirage-net-xen" ~min:"2.1.3";
        package "mirage-qubes" ~min:"0.9.1";
        package "mirage-xen" ~min:"8.0.0";
        package "ipaddr";
        package "ethernet" ~min:"3.0.0";
        package "arp" ~min:"2.3.0" ~sublibs:[ "mirage" ];
        package ~sublibs:[ "mirage" ] "miragevpn";
        package "mirage-nat" ~min:"3.0.0";
      ]
    "Unikernel.Main"
    (random @-> mclock @-> pclock @-> time @-> qubesdb @-> stackv4v6 @-> kv_ro @-> job)

let () =
  register "qubes-miragevpn"
    [
      main $ default_random $ default_monotonic_clock $ default_posix_clock
      $ default_time
      $ default_qubesdb
      $ stack
      $ config;
    ]
