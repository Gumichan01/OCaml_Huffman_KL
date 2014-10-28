(* This code was written by Juliusz Chroboczek.  MIT license. *)

exception Test_Failed of int list * int list

let tmpcounter = ref 42

let tmpfile () =
  tmpcounter := !tmpcounter + 1;
  "/tmp/bitio-test-" ^ (string_of_int !tmpcounter)

let do_test e1 e2 =
  let fn = tmpfile() in
  let result = e1 fn in
  (if result <> e2 then raise (Test_Failed (result,e2)));
  Sys.remove fn;;

do_test
  (fun fn ->
   let ch = open_out_bin fn in
   output_byte ch 42;
   output_byte ch 254;
   close_out ch;
     let ch = Bitio.open_in_bit fn in
     let list = ref [] in
     for i = 1 to 16 do
       list := (Bitio.input_bit ch)::!list
     done;
     Bitio.close_in_bit ch;
     List.rev !list)
  [0;0;1;0;1;0;1;0; 1;1;1;1;1;1;1;0];;

do_test
  (fun fn ->
   let ch = Bitio.open_out_bit fn in
   List.iter (fun bit -> Bitio.output_bit ch bit) [0;0;1;0;1;0;1;0; 1;1;1;1;1];
   Bitio.close_out_bit ch;
   let ch = open_in_bin fn in
   let list = ref [] in
   for i = 1 to 2 do
     list := (input_byte ch)::!list
   done;
   close_in ch;
   List.rev !list)
  [42;248];;

do_test
  (fun fn ->
   let ch = open_out_bin fn in
   output_byte ch 42;
   output_byte ch 0;
   close_out ch;
   let ch = Bitio.open_in_bit fn in
   ignore (Bitio.input_bit ch);
   let byte = Bitio.input_bit_byte ch in
   Bitio.close_in_bit ch;
   [byte])
  [42*2];;

do_test
  (fun fn ->
   let ch = Bitio.open_out_bit fn in
   Bitio.output_bit ch 0;
   Bitio.output_bit_byte ch 42;
   Bitio.close_out_bit ch;
   let ch = open_in_bin fn in
   let byte = input_byte ch in
   close_in ch;
   [byte])
  [42/2];;
