open! Core

let%expect_test "Show request parsing" =
  let case s =
    s |> Jsonrpc.Request.of_string |> [%sexp_of: Jsonrpc.Request.t] |> print_s
  in
  case {|{"jsonrpc": "2.0", "method": "subtract", "params": [42, 23], "id": 1}|};
  [%expect
    {|
    (Single
     (Method_call
      ((method_ subtract) (params (Array ((Number 42) (Number 23))))
       (id (Number 1)))))
    |}];
  case
    {|{"jsonrpc": "2.0", "method": "subtract", "params": {"subtrahend": 23, "minuend": 42}, "id": 3}|};
  [%expect
    {|
    (Single
     (Method_call
      ((method_ subtract)
       (params (Object ((subtrahend (Number 23)) (minuend (Number 42)))))
       (id (Number 3)))))
    |}];
  case {|{"jsonrpc": "2.0", "method": "update", "params": [1,2,3,4,5]}|};
  [%expect
    {|
    (Single
     (Notification
      ((method_ update)
       (params (Array ((Number 1) (Number 2) (Number 3) (Number 4) (Number 5)))))))
    |}];
  case {|{"jsonrpc": "2.0", "method": "foobar"}|};
  [%expect {| (Single (Notification ((method_ foobar)))) |}];
  case {|{"jsonrpc": "2.0", "method": "foobar", "id": "1"}|};
  [%expect {| (Single (Method_call ((method_ foobar) (id (String 1))))) |}];
  case {|{"jsonrpc": "2.0", "method": 1, "params": "bar"}|};
  [%expect {| (Single (Invalid ((id ())))) |}];
  case {|{"jsonrpc": "2.0", "method": 1, "params": "bar", "id": null}|};
  [%expect {| (Single (Invalid ((id (Null))))) |}];
  case {|[]|};
  [%expect {| (Single (Invalid ((id ())))) |}];
  case {|[1]|};
  [%expect {| (Batch ((Invalid ((id ()))))) |}];
  case {|[1,2,3]|};
  [%expect {| (Batch ((Invalid ((id ()))) (Invalid ((id ()))) (Invalid ((id ()))))) |}];
  case
    {|[
        {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], "id": "1"},
        {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},
        {"jsonrpc": "2.0", "method": "subtract", "params": [42,23], "id": "2"},
        {"foo": "boo"},
        {"jsonrpc": "2.0", "method": "foo.get", "params": {"name": "myself"}, "id": "5"},
        {"jsonrpc": "2.0", "method": "get_data", "id": "9"} 
    ]|};
  [%expect
    {|
    (Batch
     ((Method_call
       ((method_ sum) (params (Array ((Number 1) (Number 2) (Number 4))))
        (id (String 1))))
      (Notification ((method_ notify_hello) (params (Array ((Number 7))))))
      (Method_call
       ((method_ subtract) (params (Array ((Number 42) (Number 23))))
        (id (String 2))))
      (Invalid ((id ())))
      (Method_call
       ((method_ foo.get) (params (Object ((name (String myself)))))
        (id (String 5))))
      (Method_call ((method_ get_data) (id (String 9))))))
    |}];
  ()
;;

let%expect_test "Show response parsing" =
  let case s =
    s |> Jsonrpc.Response.of_string |> [%sexp_of: Jsonrpc.Response.t] |> print_s
  in
  case {|{"jsonrpc": "2.0", "result": 19, "id": 1}|};
  [%expect {| (Single (Success ((result (Number 19)) (id (Number 1))))) |}];
  case
    {|{"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "1"}|};
  [%expect
    {|
    (Single
     (Failure
      ((error ((code -32601) (message "Method not found"))) (id (String 1)))))
    |}];
  case
    {|{"jsonrpc": "2.0", "error": {"code": -32700, "message": "Parse error"}, "id": null}|};
  [%expect
    {|
    (Single
     (Failure ((error ((code -32700) (message "Parse error"))) (id Null))))
    |}];
  case
    {|{"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request"}, "id": null}|};
  [%expect
    {|
    (Single
     (Failure ((error ((code -32600) (message "Invalid Request"))) (id Null))))
    |}];
  case
    {|  {"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request", "data": "some more info"}, "id": null}|};
  [%expect
    {|
    (Single
     (Failure
      ((error
        ((code -32600) (message "Invalid Request")
         (data (String "some more info"))))
       (id Null))))
    |}];
  case
    {|[
        {"jsonrpc": "2.0", "result": 7, "id": "1"},
        {"jsonrpc": "2.0", "result": 19, "id": "2"},
        {"jsonrpc": "2.0", "error": {"code": -32600, "message": "Invalid Request"}, "id": null},
        {"jsonrpc": "2.0", "error": {"code": -32601, "message": "Method not found"}, "id": "5"},
        {"jsonrpc": "2.0", "result": ["hello", 5], "id": "9"}
    ]|};
  [%expect
    {|
    (Batch
     ((Success ((result (Number 7)) (id (String 1))))
      (Success ((result (Number 19)) (id (String 2))))
      (Failure ((error ((code -32600) (message "Invalid Request"))) (id Null)))
      (Failure
       ((error ((code -32601) (message "Method not found"))) (id (String 5))))
      (Success ((result (Array ((String hello) (Number 5)))) (id (String 9))))))
    |}];
  ()
;;
