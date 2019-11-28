open Assert
open Astlib
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let subtype_test c t1 t2 expected =
  Printf.sprintf "subtype %s %s" (string_of_ty t1) (string_of_ty t2),
  (fun () ->
     if Typechecker.subtype c t1 t2 == expected then ()
     else failwith "should match")              

let subtype_tests = [
  subtype_test Tctxt.empty TInt TInt true;
  subtype_test Tctxt.empty TInt TBool false;
  subtype_test Tctxt.empty TBool TBool true;
  subtype_test Tctxt.empty (TRef (RFun ([], RetVoid))) (TRef (RFun ([], RetVoid))) true;
  subtype_test Tctxt.empty (TRef (RFun ([], RetVal TInt))) (TRef (RFun ([], RetVal TInt))) true;
  subtype_test Tctxt.empty (TRef (RFun ([], RetVal TInt))) (TRef (RFun ([], RetVal TBool))) false;
  subtype_test Tctxt.empty (TRef (RFun ([], RetVal (TRef RString)))) (TRef (RFun ([], RetVal (TNullRef RString)))) true;
  subtype_test Tctxt.empty (TRef (RFun ([], RetVal (TNullRef RString)))) (TRef (RFun ([], RetVal (TRef RString)))) false;
  subtype_test Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TInt)) true;
  subtype_test Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TBool)) false;
  subtype_test Tctxt.empty (TRef (RArray (TRef RString))) (TRef (RArray (TNullRef RString))) false;
  subtype_test Tctxt.empty (TRef (RArray (TNullRef RString))) (TRef (RArray (TRef RString))) false;
  subtype_test Tctxt.empty (TRef (RString)) (TRef (RString)) true;
  subtype_test Tctxt.empty (TRef (RString)) (TRef (RArray TInt)) false;
  subtype_test (Tctxt.add_struct (Tctxt.add_struct Tctxt.empty "test1" []) "test2" [])
    (TRef (RStruct "test1")) (TRef (RStruct "test2")) true;
  subtype_test (Tctxt.add_struct (Tctxt.add_struct Tctxt.empty "test1" [{fieldName="Name"; ftyp=TRef RString}]) "test2" [])
    (TRef (RStruct "test1")) (TRef (RStruct "test2")) true;
  subtype_test (Tctxt.add_struct (Tctxt.add_struct Tctxt.empty "test1" [{fieldName="Name"; ftyp=TRef RString}]) "test2" [])
    (TRef (RStruct "test2")) (TRef (RStruct "test1")) false;
]

let provided_tests : suite = [
  Test("Own subtypes", subtype_tests);
  Test("Json self-test", executed_oat_file [("studenttests/json.oat", "test", "passed0")]); 
] 
