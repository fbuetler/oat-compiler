open Assert
open Astlib
open Ast
open Driver
open Gradedtests

let struct_test_ctxt = {
  Tctxt.empty with structs = [
    ("A", [
      {fieldName="f1"; ftyp=TInt};
      {fieldName="f2"; ftyp=TNullRef (RStruct "B")};
    ]);
    ("B", [
      {fieldName="f1"; ftyp=TInt};
    ]);
    ("C", [
      {fieldName="f1"; ftyp=TInt};
      {fieldName="f2"; ftyp=TNullRef (RStruct "A")};
    ]);
  ]
}

let struct_tctxt = {Tctxt.empty with structs = [("A", [{fieldName="f1"; ftyp=TInt}]); ("B", [{fieldName="f1"; ftyp=TInt}; {fieldName="f2"; ftyp=TBool}])]}

let unit_tests = [
  ("subtype_string_array_nullable_string_array",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray (TRef RString))) (TNullRef (RArray (TRef RString))) then ()
       else failwith "should not fail")                                                                                     
  ); 
  ("no_subtype_string_array_int_array",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray (TRef RString))) (TRef (RArray TInt)) then
         failwith "should not succeed" else ())
  );
  ("Typechecker: Struct with more fields",
  (fun () ->
      if Typechecker.subtype struct_test_ctxt (TRef (RStruct "A")) (TRef (RStruct "B")) then ()
      else failwith "should not fail")
  ); 
  ("Typechecker: Struct with different ftyp",
    (fun () ->
        if Typechecker.subtype struct_test_ctxt (TRef (RStruct "A")) (TRef (RStruct "C")) then
          failwith "should not succeed" else ())
  );
  ("Typechecker: Functions with 1 argument",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RFun ([TInt],RetVal TInt))) (TRef (RFun ([TInt],RetVal TInt))) then ()
       else failwith "should not fail")
  ); 
  ("Typechecker: Functions with different argument type",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RFun ([TBool],RetVal TInt))) (TRef (RFun ([TInt],RetVal TInt))) then
         failwith "should not succeed" else ())
  );
  ("Typechecker: Struct with more fields",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RFun ([TInt],RetVal TInt))) (TRef (RFun ([TInt],RetVal TInt))) then ()
       else failwith "should not fail")
  ); 
  ("Typechecker: Struct with different ftyp",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RFun ([TBool],RetVal TInt))) (TRef (RFun ([TInt],RetVal TInt))) then
         failwith "should not succeed" else ())
  );
  ("subtype_RFunTB_RFunTA",
    (fun () ->
      if Typechecker.subtype struct_tctxt (TRef (RFun ([TRef (RStruct "A")], RetVal (TRef (RStruct "B"))))) (TRef (RFun ([TRef (RStruct "B")], RetVal (TRef (RStruct "A"))))) then ()
      else failwith "should not fail")                                                                                     
  ); 
  ("no_subtype_RFunTA_RFunTB",
    (fun () ->
      if Typechecker.subtype struct_tctxt (TRef (RFun ([TRef (RStruct "B")], RetVal (TRef (RStruct "A"))))) (TRef (RFun ([TRef (RStruct "A")], RetVal (TRef (RStruct "B"))))) then
        failwith "should not succeed" else ())
  );
  ("Binop_Neq_succeeds",
   (fun () ->
       if Typechecker.typecheck_exp Tctxt.empty (no_loc (Bop (Neq, no_loc (CInt 7L), no_loc (CInt 42L)))) == TBool then ()
       else failwith "should not fail")
  ); 
  ("Binop_Neq_fails",
  typecheck_error (fun () ->
       if Typechecker.typecheck_exp Tctxt.empty (no_loc (Bop (Neq, no_loc (CBool false), no_loc (CInt 42L)))) == TBool then ()
       else failwith "should not fail")  
  )
]

let brainfuck_tests = [
  (* Hello World from Wikipedia *)
  ("studenttests/brainfuck.oat", "ppppppppppsrppppppprpppppppppprppprpllllmerpporpopppppppooppporppollppppppppppppppporopppommmmmmommmmmmmmorpo", "Hello World!0");
  ("studenttests/brainfuck.oat", "ririririririspplerorororororo Abc123", "Cde3450");
]

let provided_tests : suite = [
  Test("Others subtype", unit_tests);
  Test("Others: another_su.oat", executed_oat_file [("studenttests/another_sum.oat", "", "200")]); 
  (* Test ("Others: Performance Comparison", [("studenttests/manually", assert_eq true false)]);  *) (* TODO the fuck ? *)
  Test("Others: Moodle Fraction Test", executed_oat_file [("studenttests/fraction.oat", "", "42")]);
  Test("Others: linked list tests", executed_oat_file [("studenttests/linkedlist.oat", "", "4")]);
  Test("Others: SP: brainfuck tests", executed_oat_file brainfuck_tests);
  Test("Others: Struct Student Test", executed_oat_file [("studenttests/structs.oat", "", "4000")]);
] 
