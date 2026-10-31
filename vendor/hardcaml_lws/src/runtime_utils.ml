open Hardcaml

let create_scope_from_shared_config (config : Lws.Config.t) =
  let%tydi { auto_label_hierarchical_ports; timeout = _; assertions } = config in
  Scope.create
    ~flatten_design:true
    ~auto_label_hierarchical_ports
    ~trace_properties:assertions
    ()
;;
