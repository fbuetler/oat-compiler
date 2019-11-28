open Assert
open Astlib
open Gradedtests

(* 17-913-534 Florian Bütler
   16-926-560 Philippe Voinov *)

let subtype_test c t1 t2 expected =
  Printf.sprintf "subtype %s %s" (string_of_ty t1) (string_of_ty t2),
  (fun () ->
     if Typechecker.subtype c t1 t2 == expected then ()
     else failwith "should match")              

let subtype_tests = [
  subtype_test (Tctxt.add_struct (Tctxt.add_struct Tctxt.empty "test1" [{fieldName="Name"; ftyp=TRef RString}]) "test2" [])
    (TRef (RStruct "test1")) (TRef (RStruct "test2")) true;
  subtype_test (Tctxt.add_struct (Tctxt.add_struct Tctxt.empty "test1" [{fieldName="Name"; ftyp=TRef RString}]) "test2" [])
    (TRef (RStruct "test2")) (TRef (RStruct "test1")) false;
]

let provided_tests : suite = [
  Test("Own subtypes moodle", subtype_tests);
  Test("Json self-test", executed_oat_file [("studenttests/json.oat", "test", "passed0")]); 
] 
