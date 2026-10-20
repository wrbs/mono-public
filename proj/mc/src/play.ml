open! Core

let source =
  {|
1f8b08000000000000ffd59c5d6fdbc8
158687e4f043b2e368e378d3262b29d9
dd76fbdd2edade2cf6a2c5f6a2bd0916
08da5b8116c716118914482a4ef66aff
417f53ff4fbb5bb4b7055c4ad67144ce
e1e1c9840c10038ea1f8e5799f3967ce
cc78e46428c440c83cfe46394288ede7
a8fc0ccacfe14078e7cb74fe3c1f0a61
fddd116e5e84852abf533ee1acd31c1e
800f57b46b3c86c6676802866640682c
0633682866d050cca0a198414331db0c
66d050cca0a198414331838662867a50
cca0a198414331838662060dc52c19cc
a0a198414331838662060dc5ec329841
4331838662060dc50c1a8ad963308386
62060dc50c1a8a193414b3cf60060dc5
0c1a8a193414336828e680c10c1a8a19
341433682866d05498adaa66b0d7580c
8dcdd0380c8d24984143e51034540e41
43e51034540e4143e570c8c82168a81c
8286ca2168a81c8286ca2168a81c8286
ca2168a81c8286cae191808f760d9567
d050793eba756ad75079060d9567d050
79060d9567d05079064d25cf765573cc
c83368a83c8386caf3f1ad53bb86ca33
68a83c8386ca3368a83c8386ca3368a8
3cdfc1f2ece01a97a1f1181a9fa10918
1a6a5c27d8b86a9abb9846ea5e567dec
2eaea98cddc3353e234ec0e0a98cddd7
c7ae7905facf44767d5c035c5319d710
d754bc8e704d655cc7b86640305b0c66
d050cc1683d962305b0c669bc16c3398
6d06b3cd60b619cc0e83d961303b0c66
87c1ec30982583593298258359329825
83d96530bb0c6697c1ec32985d06b3c7
60f618cc1e83d963307b0c669fc1ec33
987d06b3cf60f619cc0183396030070c
e680c11c3098070ce6018379c0601e30
980718f31dfdccaf319fe09a0af35d5c
53611ee19a0af307b8a6c27c4f3f3f6b
cca7b8a6c27c1fd75498cf704d85f943
5c43cd0d21608da1cf1b4ecbdc004ddb
79c369991ba0693b6fb43183a6edbcd1
c66c31982d06b3cd60b619cc3683d966
30db0c6687c1ec30981d06b3c3607618
cc92c12c19cc92c12c19cc92c1ec3298
5d06b3cb607619cc2e83d963307b0c66
8fc1ec31983d06b3cf60f619cc3e83d9
6730fb18736d1f0c30e6135c43ed8301
c63cc235d43e1860ccb57d7080319fe2
1a6a1f1c60cc67b886da07071833b20f
ca96b9019ab67d50b6cc0dd0b4ed83b2
85d9623083a66d1f6c63b618cc1683d9
6630db0c669bc16c33986d06b3c36076
18cc0e83d961303b0c66c960960c66c9
60960c66c9607619cc2e83d96530bb0c
661763aeadcf1ec67c826ba8f5d9c398
47b8865a9f3d8cb9b63efb18f329aea1
d6671f633ec335d4faec63ccc8faecb6
cc0dd0b4adcf6ecbdc004ddbfaecb630
5b0c66d0b4adcf6dcc1683d96230db0c
669bc16c33986d06b3cd607618cc0e83
d961303b0c660763aead1b12633ec135
d4ba2131e611aea1d60d8931d7d60d17
633ec535d4bae162cc67b8865a375c8c
195937bc96b9019ab675c36b991ba069
5b37bc16668bc10c9ab675a38dd96230
5b18f31dbd4f35e6135c43cd671b631e
e11a6a3edb18f33dbd4f35e6535c43cd
6707633ec335d47c7630e63bfafcf15b
f20c1a2acfa0a1f20c1a2acfa0a1f26c
61cca7b886cab385319fe11a2acf16c6
fca0aa81f7a9dda17092f3c212a3a74a
45f9b375f9e75fd751f98c3514fe5355
5ca5d97347c867f137bbee92c2fe4b74
7dfdbfebeb6bf1a52bbc6745a6f2fcab
c7dbd1075f85eb701e17affefcedf6e5
d11fa3484537823f881db31d47e2de3c
5365f82f54320f7315cd2ec2c415eece
f81fdf6e2f75bd67e9269b6fdd9cd2e5
3fe5d72fcbafff148cd178efcb68fec5
198dffbe8ce63bce6882f76534df7346
33785f46f3efca687e501d0dfc264a6f
abc09efd7e8d3d5f8417c59bf7c90f71
fadebafe6de9bfe3d0f7d6e56f4bff3d
87beb7ae7e5bfaeabc7f88d3f7d6c5a6
f4ff45e91fe96712eb75d796c2afe362
be28ffe6f8c6f6d375bc566976192679
697db98cf3c56c9166c96cf7cf68662a
294acc8ac347b883d79dc31877f0bb73
98e00e41770e53dc61d09dc323fd2487
57faa8a34a83835e69638731eea057da
d861823be895367698e20e7aa58d1d6a
95b61b2b3deca8d27663a58d1dc6b883
5e69638709eea057dad8618a3be89536
76a855da69acf4a0a34a3b8d95367618
e30e7aa58d1d26b8835e69638729eea0
57dad8a15669d958e9a0a34acbc64a1b
3b8c7107bdd2c60e13dc41afb4b1c314
77d02b6dec50abb4db5869bfa34abb8d
95367618e30e7aa58d1d26b8835e6963
8729eea057dad8a15669afb1d25e4795
f61a2b6dec30c61df44a1b3b4c7007bd
d2c60e53dc41afb4b143add27e63a5dd
8e2aed3756dad8618c3be895367698e0
0e7aa58d1da6b8835e6963875aa583c6
4acb8e2a1d3456dad8618c3be8953676
98e00e7aa58d1da6b8835e696387c755
87c1dec1d21c6a173679a1c2d5ec6a11
e7c55255223ec123da4d11a707cce573
e12651cdb81fe3c19da6e04f0e822fd2
c5f262b92988f09fe0e125277c91a5ab
b52aa8f08ff0f07a5b391db51538e86d
65ec30c61df4b6327698e00e7a5b193b
4c7107bdad8c1d3ead3a0cdbdaea9303
8717e9cbd95c2d554e4da51fe106ac2e
cb37e7e7619e3707ff311e9cd56559ba
c82e9629c5fe191ebeb1cb1e1fae1059
99973029d84d06d1f526b33b6a3270d0
9bccd8618c3be84d66ec30c11df42633
7698e20e7a93193bfca4ea70b477104d
f3e8b3c3cda0dc670a15c10ed66cf253
dca4b193c7879d1ca7cbb039f4cff0d0
8d3d3c39089d84799845cdb17f8ec76e
6ce1c3f5e152456afe9c68b15fe0c159
0d1cc5e13acc53fee113a2eb0d6c75d4
c0e0a037b0b1c31877d01bd8d861823b
e80d6cec30c51df4063676f865d5e1b8
adb73eaeed928bcd2a4c8806fb151ebf
b1c10e7bf7325c9d13a17f8d876eecaf
374fce6f7087c626ab9ca3e3f93c5da6
ec1e83e07a8f19e37f843be83d66ec30
c61df41e337698e00e7a8f193b4c7107
bdc7decc6120fc75b8dcfe30b2dd763f
0f847c1aae94b87ff0f4559c44f34579
981a8ae1d759ba565911ab3c281f4caf
54a622218b6ca302e15d84f338b91452
8579b1c53008e55e84cbfc20969ba79b
62d1513079a50ec11e61b166ab125e65
dca16e2306c2dd3efeeae69bafe33f58
c5899a67e145f145f9685ea4e58fab57
71a66ab1dd5d6c617ffedb604f289352
1aecf3b87fb14f84cce368fb2a493378
656ef8bb776df87bdc706fc130bcfbda
70a95eb0cb7413ac0c52be56e5b458a6
69f63ae893a64e512f0b95e4719ad46c
76ffb5aa908bcde50e7b11ae951845e9
e6bc3c1dced3a4242ccf8b6f1ddf5d86
59af06de4a45f166d5e710f255b85cf6
68208b3879d567fc4a8dbd9bf85d57b6
abb0f57a76865bad625761abb5eb2c6a
b56279d9fe3d54aca3b05ac5bac2ad55
aca3b0b58ad5a322bf555b0bd3b0113f
ac3d385f8471328bb2f8457d2791eb30
2b7667a2acdc7647e1cb389f85cb34b9
9c5dc4591912767cb9fd8eb05e9a7b78
ab388a96aa5f134725d19b393c68a81c
d50bcd872bd816afcad9b2ffaea11534
c8bbf0baed9a7732b07d2bbd0baf7d7f
195aa1efb798267077166b72baa7bf0f
d38bcfe161fff62d99de9d6edf9de9c5
e943fcfabef73aed6ff27bcfdeeda57e
2f4ea7c8fd7e2f460f9b2f807bf11bd5
ef827b71f940bb16ee7ddeed6f887b9f
0d7059dc8bd1197a9bd8fb44d85d2cf6
e2d2f57ecbdf2e6eae1b0db75ad46620
82dd655619697bfb251c71f4a7b008ff
a6b2ed415688bbb9f83f35d9f7e61165
0000|}
;;

let decode_hex c =
  match c with
  | '0' .. '9' -> Char.to_int c - Char.to_int '0'
  | 'a' .. 'f' -> Char.to_int c - Char.to_int 'a' + 10
  | 'A' .. 'F' -> Char.to_int c - Char.to_int 'A' + 10
  | _ -> failwithf "Invalid hex char %c" c ()
;;

let bytes =
  lazy
    (String.split_lines source
     |> List.concat_map ~f:(fun line ->
       String.to_list line
       |> List.chunks_of ~length:2
       |> List.filter_map ~f:(function
         | [ a; b ] when Char.is_hex_digit a && Char.is_hex_digit b ->
           Some ((decode_hex a * 16) + decode_hex b |> Char.of_int_exn)
         | _ -> None))
     |> String.of_char_list)
;;

let%expect_test "size" =
  print_s [%sexp (String.length (Lazy.force bytes) : int)];
  [%expect {| 2690 |}]
;;

let inflate input =
  let o = Bigstring.create Gz.io_buffer_size in
  let buffer = Buffer.create 1024 in
  let decoder = Gz.Inf.decoder (`String input) ~o in
  let flush decoder =
    Bigstring.to_string o ~len:(Gz.io_buffer_size - Gz.Inf.dst_rem decoder)
    |> Buffer.add_string buffer;
    Gz.Inf.flush decoder
  in
  let rec aux decoder =
    match Gz.Inf.decode decoder with
    | `Malformed error -> Or_error.error_s [%message "Malformed input" (error : string)]
    | `Await _ -> raise_s [%message "should be impossible"]
    | `Flush decoder -> aux (flush decoder)
    | `End decoder ->
      let _ = flush decoder in
      Ok (Buffer.contents buffer)
  in
  aux decoder
;;

let decoded =
  let%map.Lazy bytes in
  inflate bytes |> Or_error.ok_exn
;;

let%expect_test _ =
  let nbt = Nbt.parse (Lazy.force bytes) in
  let template = Nbt_conv.parse Structure.Repr.nbt_conv nbt.data in
  [%sexp (template : Structure.Repr.t)] |> Expectree.sexp_to_string |> print_endline;
  [%expect
    {|
    ╭─╴size╶╴16 8 10
    ├─╴blocks
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴0 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴0 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴0 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴0 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴0 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴1 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴1 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴1 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴1 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴1 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴2 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴2 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴2 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴2 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴2 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴3 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴3 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴3 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴3 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴3 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴4 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴4 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴4 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴4 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴4 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴5 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴5 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴5 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴5 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴5 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴6 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴6 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴6 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴6 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴6 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴7 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴7 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴7 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴7 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴7 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴8 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴8 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴8 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴8 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴8 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴9 0 1
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴9 0 2
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴9 0 3
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴9 0 4
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴9 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴9 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴9 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴9 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴9 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴10 0 1
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴10 0 2
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴10 0 3
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴10 0 4
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴10 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴10 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴10 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴10 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴10 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴11 0 0
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴11 0 1
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴11 0 2
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴11 0 3
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴11 0 4
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴11 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴11 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴11 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴11 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴11 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴2
    │  │  ├─╴pos╶╴12 0 0
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴12 0 1
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴12 0 2
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴12 0 3
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴1
    │  │  ├─╴pos╶╴12 0 4
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴12 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴12 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴12 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴12 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴0
    │  │  ├─╴pos╶╴12 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴2
    │  │  ├─╴pos╶╴13 0 0
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴3
    │  │  ├─╴pos╶╴13 0 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴3
    │  │  ├─╴pos╶╴13 0 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴3
    │  │  ├─╴pos╶╴13 0 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴3
    │  │  ├─╴pos╶╴13 0 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴3
    │  │  ├─╴pos╶╴13 0 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴2
    │  │  ├─╴pos╶╴14 0 0
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴2
    │  │  ├─╴pos╶╴15 0 0
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴4
    │  │  ├─╴pos╶╴13 1 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴5
    │  │  ├─╴pos╶╴13 1 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴6
    │  │  ├─╴pos╶╴13 1 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴5
    │  │  ├─╴pos╶╴13 1 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴4
    │  │  ├─╴pos╶╴13 1 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴7
    │  │  ├─╴pos╶╴14 1 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴0 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴0 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴0 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴0 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴0 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴1 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴1 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴1 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴1 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴1 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴2 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴2 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴2 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴2 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴2 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴3 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴3 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴3 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴3 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴3 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴4 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴4 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴4 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴4 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴4 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴5 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴5 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴5 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴5 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴5 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴6 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴6 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴6 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴6 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴6 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴7 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴7 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴7 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴7 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴7 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴8 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴8 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴8 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴8 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴8 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴9 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴9 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴9 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴9 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴9 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴13
    │  │  ├─╴pos╶╴10 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴14
    │  │  ├─╴pos╶╴10 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴15
    │  │  ├─╴pos╶╴10 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴16
    │  │  ├─╴pos╶╴10 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴17
    │  │  ├─╴pos╶╴10 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴18
    │  │  ├─╴pos╶╴11 2 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴19
    │  │  ├─╴pos╶╴11 2 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴20
    │  │  ├─╴pos╶╴11 2 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴21
    │  │  ├─╴pos╶╴11 2 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴22
    │  │  ├─╴pos╶╴11 2 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴0 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴0 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴0 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴0 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴0 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴1 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴1 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴1 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴1 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴1 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴2 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴2 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴2 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴2 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴2 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴3 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴3 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴3 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴3 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴3 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴4 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴4 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴4 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴4 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴4 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴5 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴5 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴5 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴5 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴5 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴6 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴6 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴6 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴6 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴6 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴7 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴7 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴7 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴7 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴7 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴13
    │  │  ├─╴pos╶╴8 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴14
    │  │  ├─╴pos╶╴8 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴15
    │  │  ├─╴pos╶╴8 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴16
    │  │  ├─╴pos╶╴8 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴17
    │  │  ├─╴pos╶╴8 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴18
    │  │  ├─╴pos╶╴9 3 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴19
    │  │  ├─╴pos╶╴9 3 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴20
    │  │  ├─╴pos╶╴9 3 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴21
    │  │  ├─╴pos╶╴9 3 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴22
    │  │  ├─╴pos╶╴9 3 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴0 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴0 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴0 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴0 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴0 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴1 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴1 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴1 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴1 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴1 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴2 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴2 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴2 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴2 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴2 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴3 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴3 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴3 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴3 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴3 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴4 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴4 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴4 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴4 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴4 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴5 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴5 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴5 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴5 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴5 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴13
    │  │  ├─╴pos╶╴6 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴14
    │  │  ├─╴pos╶╴6 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴15
    │  │  ├─╴pos╶╴6 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴16
    │  │  ├─╴pos╶╴6 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴17
    │  │  ├─╴pos╶╴6 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴18
    │  │  ├─╴pos╶╴7 4 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴19
    │  │  ├─╴pos╶╴7 4 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴20
    │  │  ├─╴pos╶╴7 4 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴21
    │  │  ├─╴pos╶╴7 4 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴22
    │  │  ├─╴pos╶╴7 4 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴0 5 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴0 5 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴0 5 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴0 5 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴0 5 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴1 5 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴1 5 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴1 5 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴1 5 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴1 5 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴2 5 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴2 5 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴2 5 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴2 5 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴2 5 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴3 5 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴3 5 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴3 5 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴3 5 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴3 5 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴13
    │  │  ├─╴pos╶╴4 5 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴14
    │  │  ├─╴pos╶╴4 5 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴15
    │  │  ├─╴pos╶╴4 5 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴16
    │  │  ├─╴pos╶╴4 5 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴17
    │  │  ├─╴pos╶╴4 5 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴18
    │  │  ├─╴pos╶╴5 5 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴19
    │  │  ├─╴pos╶╴5 5 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴20
    │  │  ├─╴pos╶╴5 5 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴21
    │  │  ├─╴pos╶╴5 5 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴22
    │  │  ├─╴pos╶╴5 5 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴0 6 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴0 6 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴0 6 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴0 6 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴0 6 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴8
    │  │  ├─╴pos╶╴1 6 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴9
    │  │  ├─╴pos╶╴1 6 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴10
    │  │  ├─╴pos╶╴1 6 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴11
    │  │  ├─╴pos╶╴1 6 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴12
    │  │  ├─╴pos╶╴1 6 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴13
    │  │  ├─╴pos╶╴2 6 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴14
    │  │  ├─╴pos╶╴2 6 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴15
    │  │  ├─╴pos╶╴2 6 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴16
    │  │  ├─╴pos╶╴2 6 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴17
    │  │  ├─╴pos╶╴2 6 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴18
    │  │  ├─╴pos╶╴3 6 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴19
    │  │  ├─╴pos╶╴3 6 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴20
    │  │  ├─╴pos╶╴3 6 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴21
    │  │  ├─╴pos╶╴3 6 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴22
    │  │  ├─╴pos╶╴3 6 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴13
    │  │  ├─╴pos╶╴0 7 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴14
    │  │  ├─╴pos╶╴0 7 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴15
    │  │  ├─╴pos╶╴0 7 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴16
    │  │  ├─╴pos╶╴0 7 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴17
    │  │  ├─╴pos╶╴0 7 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴18
    │  │  ├─╴pos╶╴1 7 5
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴19
    │  │  ├─╴pos╶╴1 7 6
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴20
    │  │  ├─╴pos╶╴1 7 7
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴21
    │  │  ├─╴pos╶╴1 7 8
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴22
    │  │  ├─╴pos╶╴1 7 9
    │  │  ╰─╴nbt╶╴•
    │  ├──┬─╴state╶╴23
    │  │  ├─╴pos╶╴14 0 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ├─╴Capacity╶╴1216348160:float
    │  │        │  ╰─╴AddedStress╶╴1073741824:float
    │  │        ├─╴id╶╴create:encased_fan
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-20d
    │  ├──┬─╴state╶╴23
    │  │  ├─╴pos╶╴14 0 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ├─╴Capacity╶╴1216348160:float
    │  │        │  ╰─╴AddedStress╶╴1073741824:float
    │  │        ├─╴id╶╴create:encased_fan
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-19d
    │  ├──┬─╴state╶╴23
    │  │  ├─╴pos╶╴14 0 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ├─╴Capacity╶╴1216348160:float
    │  │        │  ╰─╴AddedStress╶╴1073741824:float
    │  │        ├─╴id╶╴create:encased_fan
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-18d
    │  ├──┬─╴state╶╴23
    │  │  ├─╴pos╶╴14 0 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ├─╴Capacity╶╴1216348160:float
    │  │        │  ╰─╴AddedStress╶╴1073741824:float
    │  │        ├─╴id╶╴create:encased_fan
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-17d
    │  ├──┬─╴state╶╴23
    │  │  ├─╴pos╶╴14 0 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ├─╴Capacity╶╴1216348160:float
    │  │        │  ╰─╴AddedStress╶╴1073741824:float
    │  │        ├─╴id╶╴create:encased_fan
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-16d
    │  ├──┬─╴state╶╴24
    │  │  ├─╴pos╶╴15 0 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ╰─╴Capacity╶╴1216348160:float
    │  │        ├─╴id╶╴create:encased_shaft
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-19d
    │  ├──┬─╴state╶╴25
    │  │  ├─╴pos╶╴15 0 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ╰─╴Capacity╶╴1216348160:float
    │  │        ├─╴id╶╴create:encased_shaft
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-18d
    │  ├──┬─╴state╶╴25
    │  │  ├─╴pos╶╴15 0 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ╰─╴Capacity╶╴1216348160:float
    │  │        ├─╴id╶╴create:encased_shaft
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-17d
    │  ├──┬─╴state╶╴25
    │  │  ├─╴pos╶╴15 0 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ╰─╴Capacity╶╴1216348160:float
    │  │        ├─╴id╶╴create:encased_shaft
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-14d
    │  │              ├─╴60d
    │  │              ╰─╴-16d
    │  ├──┬─╴state╶╴26
    │  │  ├─╴pos╶╴15 0 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴NeedsSpeedUpdate╶╴1b
    │  │        ├─╴Network
    │  │        │  ├─╴Size╶╴11d
    │  │        │  ├─╴Id╶╴-3298534948804l
    │  │        │  ├─╴Stress╶╴1126170624:float
    │  │        │  ╰─╴Capacity╶╴1216348160:float
    │  │        ├─╴id╶╴create:encased_shaft
    │  │        ├─╴Speed╶╴3246391296:float
    │  │        ╰─╴Source
    │  │           ╰─╴int-array
    │  │              ├─╴-13d
    │  │              ├─╴60d
    │  │              ╰─╴-16d
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴0 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴12d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴0 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴12d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴0 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴12d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴0 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴12d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴0 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴12d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴1 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴11d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴1 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴11d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴1 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴11d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴1 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴11d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴1 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴11d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴2 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴10d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴2 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴10d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴2 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴10d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴2 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴10d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴2 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴10d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴3 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴9d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴3 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴9d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴3 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴9d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴3 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴9d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴3 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴9d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴4 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴8d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴4 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴8d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴4 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴8d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴4 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴8d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴4 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴8d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴5 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴7d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴5 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴7d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴5 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴7d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴5 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴7d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴5 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴7d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴6 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴6d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴6 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴6d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴6 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴6d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴6 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴6d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴6 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴6d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴7 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴5d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴7 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴5d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴7 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴5d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴7 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴5d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴7 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴5d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴8 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴4d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴8 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴4d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴8 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴4d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴8 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴4d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴8 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴4d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴32
    │  │  ├─╴pos╶╴9 1 1
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴create:steam_whistle
    │  ├──┬─╴state╶╴33
    │  │  ├─╴pos╶╴9 1 2
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:posaune_block_entity
    │  ├──┬─╴state╶╴34
    │  │  ├─╴pos╶╴9 1 3
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:hohlflute_block_entity
    │  ├──┬─╴state╶╴35
    │  │  ├─╴pos╶╴9 1 4
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:trompette_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴9 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴3d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴9 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴3d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴9 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴3d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴9 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴3d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴9 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴3d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴36
    │  │  ├─╴pos╶╴10 1 1
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:vox_celeste_block_entity
    │  ├──┬─╴state╶╴37
    │  │  ├─╴pos╶╴10 1 2
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:subbass_block_entity
    │  ├──┬─╴state╶╴38
    │  │  ├─╴pos╶╴10 1 3
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:rohrflote_block_entity
    │  ├──┬─╴state╶╴39
    │  │  ├─╴pos╶╴10 1 4
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:prestant_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴10 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴2d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴10 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴2d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴10 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴2d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴10 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴2d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴10 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴2d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴40
    │  │  ├─╴pos╶╴11 1 0
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:haunted_whistle_block_entity
    │  ├──┬─╴state╶╴41
    │  │  ├─╴pos╶╴11 1 1
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:viola_block_entity
    │  ├──┬─╴state╶╴42
    │  │  ├─╴pos╶╴11 1 2
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:nasard_block_entity
    │  ├──┬─╴state╶╴43
    │  │  ├─╴pos╶╴11 1 3
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:gedeckt_block_entity
    │  ├──┬─╴state╶╴44
    │  │  ├─╴pos╶╴11 1 4
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:diapason_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴11 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴1d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴11 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴1d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴11 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴1d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴11 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴1d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴31
    │  │  ├─╴pos╶╴11 1 9
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴1d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴45
    │  │  ├─╴pos╶╴12 1 1
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:vox_humana_block_entity
    │  ├──┬─╴state╶╴46
    │  │  ├─╴pos╶╴12 1 2
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:gamba_block_entity
    │  ├──┬─╴state╶╴47
    │  │  ├─╴pos╶╴12 1 3
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴48
    │  │  ├─╴pos╶╴12 1 4
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:piccolo_block_entity
    │  ├──┬─╴state╶╴27
    │  │  ├─╴pos╶╴12 1 5
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴28
    │  │  ├─╴pos╶╴12 1 6
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴29
    │  │  ├─╴pos╶╴12 1 7
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ├──┬─╴state╶╴30
    │  │  ├─╴pos╶╴12 1 8
    │  │  ╰─╴nbt
    │  │     ╰──┬─╴Pitch╶╴0d
    │  │        ╰─╴id╶╴pipeorgans:english_horn_block_entity
    │  ╰──┬─╴state╶╴31
    │     ├─╴pos╶╴12 1 9
    │     ╰─╴nbt
    │        ╰──┬─╴Pitch╶╴0d
    │           ╰─╴id╶╴pipeorgans:english_horn_block_entity
    ├─╴palette
    │  ├──┬─╴name╶╴pipeorgans:windchest
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴east
    │  │     ╰─╴powered╶╴true
    │  ├──┬─╴name╶╴pipeorgans:windchest
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ╰─╴powered╶╴false
    │  ├──┬─╴name╶╴pipeorgans:windchest
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴west
    │  │     ╰─╴powered╶╴false
    │  ├──┬─╴name╶╴pipeorgans:windchest_master
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴west
    │  │     ├─╴powered╶╴true
    │  │     ╰─╴windy╶╴true
    │  ├──┬─╴name╶╴minecraft:redstone_wire
    │  │  ╰─╴properties
    │  │     ├─╴east╶╴none
    │  │     ├─╴north╶╴side
    │  │     ├─╴power╶╴13
    │  │     ├─╴south╶╴side
    │  │     ╰─╴west╶╴none
    │  ├──┬─╴name╶╴minecraft:redstone_wire
    │  │  ╰─╴properties
    │  │     ├─╴east╶╴none
    │  │     ├─╴north╶╴side
    │  │     ├─╴power╶╴14
    │  │     ├─╴south╶╴side
    │  │     ╰─╴west╶╴none
    │  ├──┬─╴name╶╴minecraft:redstone_wire
    │  │  ╰─╴properties
    │  │     ├─╴east╶╴side
    │  │     ├─╴north╶╴side
    │  │     ├─╴power╶╴15
    │  │     ├─╴south╶╴side
    │  │     ╰─╴west╶╴none
    │  ├──┬─╴name╶╴minecraft:lever
    │  │  ╰─╴properties
    │  │     ├─╴face╶╴floor
    │  │     ├─╴facing╶╴north
    │  │     ╰─╴powered╶╴true
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double_connected
    │  │     ╰─╴size╶╴huge
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double_connected
    │  │     ╰─╴size╶╴large
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double_connected
    │  │     ╰─╴size╶╴medium
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double_connected
    │  │     ╰─╴size╶╴small
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double_connected
    │  │     ╰─╴size╶╴tiny
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double
    │  │     ╰─╴size╶╴huge
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double
    │  │     ╰─╴size╶╴large
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double
    │  │     ╰─╴size╶╴medium
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double
    │  │     ╰─╴size╶╴small
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴double
    │  │     ╰─╴size╶╴tiny
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴single
    │  │     ╰─╴size╶╴huge
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴single
    │  │     ╰─╴size╶╴large
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴single
    │  │     ╰─╴size╶╴medium
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴single
    │  │     ╰─╴size╶╴small
    │  ├──┬─╴name╶╴pipeorgans:english_horn_extension
    │  │  ╰─╴properties
    │  │     ├─╴shape╶╴single
    │  │     ╰─╴size╶╴tiny
    │  ├──┬─╴name╶╴create:encased_fan
    │  │  ╰─╴properties
    │  │     ╰─╴facing╶╴west
    │  ├──┬─╴name╶╴create:encased_chain_drive
    │  │  ╰─╴properties
    │  │     ├─╴axis╶╴x
    │  │     ├─╴axis_along_first╶╴false
    │  │     ╰─╴part╶╴start
    │  ├──┬─╴name╶╴create:encased_chain_drive
    │  │  ╰─╴properties
    │  │     ├─╴axis╶╴x
    │  │     ├─╴axis_along_first╶╴false
    │  │     ╰─╴part╶╴middle
    │  ├──┬─╴name╶╴create:encased_chain_drive
    │  │  ╰─╴properties
    │  │     ├─╴axis╶╴x
    │  │     ├─╴axis_along_first╶╴false
    │  │     ╰─╴part╶╴end
    │  ├──┬─╴name╶╴pipeorgans:english_horn
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴north
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴huge
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:english_horn
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴north
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴large
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:english_horn
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴north
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:english_horn
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴north
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴small
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:english_horn
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴north
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴tiny
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴create:steam_whistle
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:posaune
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:hohlflute
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:trompette
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:vox_celeste
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:subbass
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:rohrflote
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:prestant
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:haunted_whistle
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:viola
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:nasard
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:gedeckt
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:diapason
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:vox_humana
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:gamba
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ├──┬─╴name╶╴pipeorgans:english_horn
    │  │  ╰─╴properties
    │  │     ├─╴facing╶╴south
    │  │     ├─╴powered╶╴false
    │  │     ├─╴size╶╴medium
    │  │     ╰─╴wall╶╴false
    │  ╰──┬─╴name╶╴pipeorgans:piccolo
    │     ╰─╴properties
    │        ├─╴facing╶╴south
    │        ├─╴powered╶╴false
    │        ├─╴size╶╴small
    │        ╰─╴wall╶╴false
    ├─╴entities╶╴•
    ╰─╴data_version╶╴3955
    |}]
;;
