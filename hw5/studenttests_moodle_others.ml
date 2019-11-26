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

let context = { Tctxt.locals = [ ("x", TInt) ]
              ; Tctxt.globals = [ ("x", TBool) ; ("y", TInt) ]
              ; Tctxt.structs = []
              }

let e1 = CArr(TInt, [no_loc @@ CInt 64L; no_loc @@ CInt (-12L); no_loc @@ CInt 48L])
let e2 = CArr(TInt, [no_loc @@ CInt (-128L); no_loc @@ CBool false])

let struct_tctxt = {Tctxt.empty with structs = [("A", [{fieldName="f1"; ftyp=TInt}]); ("B", [{fieldName="f1"; ftyp=TInt}; {fieldName="f2"; ftyp=TBool}])]}

let s1 = "s1", [{fieldName="a";ftyp=TNullRef(RString)};{fieldName="b";ftyp=TInt}]
let s2 = "s2", [{fieldName="a";ftyp=TNullRef(RString)}]
let s3 = "s3", [{fieldName="a";ftyp=TRef(RString)}]

let ourcontext : Tctxt.t = {
  locals = [];
  globals = [];
  structs = [s1;s2;s3];
}

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
  );
  ("Typ_Global_succeeds",
    (fun () ->
      if (Typechecker.typecheck_exp context (no_loc (Id "y")) == TInt)
        then ()
        else failwith "should not fail");
  );
  ("Typ_Global_fails",
    (fun () ->
      if (Typechecker.typecheck_exp context (no_loc (Id "x")) == TBool)
        then failwith "should not succeed"
        else ())
  );
  ("TYP_CARR_succeeds", (fun () ->
    match Typechecker.typecheck_exp Tctxt.empty @@ no_loc e1 with
      | TRef (RArray TInt) -> ()
      | _ -> failwith "should not fail")
  ); 
  ("TYP_CARR_fails",
  Gradedtests.typecheck_error (fun () ->
    match Typechecker.typecheck_exp Tctxt.empty @@ no_loc e2 with
      | TRef (RArray TInt) -> failwith "should not succeed"
      | _ -> ())
  );
  ("subtype_struct_width",
   (fun () ->
      if Typechecker.subtype ourcontext (TRef (RStruct "s1")) (TRef (RStruct "s2"))
      then ()
      else failwith "should not fail")
   );
  ("no_subtype_struct_depth",
   (fun () ->
      if Typechecker.subtype ourcontext (TRef (RStruct "s3")) (TRef (RStruct "s2"))
      then failwith "should not succeed" 
      else ())
  );
  ("subtype_Arr_Arr",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TInt)) then ()
       else failwith "should not fail")
  );
  ("no_subtype_Int_Bool",
   (fun () ->
       if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TBool)) then
         failwith "should not succeed" else ())
  );
  ("Subtype_ref_on_arrays_pass", (fun () -> 
    if Typechecker.subtype_ref Tctxt.empty (RArray (TInt)) (RArray (TInt)) 
    then () else failwith "should not fail")
  );
  ("Subtype_ref_on_arrays_fail", (fun () -> 
    if Typechecker.subtype_ref Tctxt.empty (RArray (TInt)) (RArray (TBool)) 
    then failwith "Should not succed" else () )
  ); 
  ("typ_bop_succ",
   (fun () ->
     if Ast.TInt == Typechecker.typecheck_exp Tctxt.empty
     (Ast.no_loc (Ast.Bop (Ast.Add, Ast.no_loc(Ast.CInt 0L), Ast.no_loc (Ast.CInt 0L))))
     then () else failwith "should not fail")
  ); 
  ("typ_bop_fail",
   (fun () ->
     try if Ast.TInt == Typechecker.typecheck_exp Tctxt.empty
     (Ast.no_loc (Ast.Bop (Ast.Add, Ast.no_loc(Ast.CInt 0L), Ast.no_loc (Ast.CBool true))))
     then failwith "should not succeed"
     else failwith "should not succeed"
     with type_error -> ())
  );
  ("lenght_of_array_typecheck",
   Gradedtests.typecheck_correct(fun () ->
			 let inner_arr : Ast.exp = Ast.CArr (Ast.TInt, [Ast.no_loc (Ast.CInt 12L); Ast.no_loc (Ast.CInt 13L)] ) in
			 let exp : Ast.exp Ast.node = Ast.no_loc (Ast.Length (Ast.no_loc inner_arr))  in
			 if Typechecker.typecheck_exp Tctxt.empty exp != Ast.TInt then failwith "incorrect type for length"
			)
  ); 
  ("length_of_bool_no_typecheck",
   Gradedtests.typecheck_error (fun () ->
			 let exp : Ast.exp Ast.node = Ast.no_loc (Ast.Length (Ast.no_loc (Ast.CBool true)))  in
			 let _ = Typechecker.typecheck_exp Tctxt.empty exp in
			 ()
      )
  );
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
  Test("Others: linked list tests", executed_oat_file [("studenttests/linkedlist.oat", "", "0")]); (* TODO discuss *)
  Test("Others: SP: brainfuck tests", executed_oat_file brainfuck_tests);
  Test("Others: Struct Student Test", executed_oat_file [("studenttests/structs.oat", "", "4000")]);
  Test("Others: Fulkerson Test", executed_oat_file [("studenttests/fulkerson.oat", "", "37")]); (* TODO discuss *)
  Test("Others: hard test", executed_oat_file [("studenttests/game.oat", "", "120")]);
  Test("Others: Inverted-Index-Boolean-Query",executed_oat_file [("studenttests/inv_index.oat", "","12354")]);
  Test("Others: Directed DFS Test", executed_oat_file [("studenttests/directed_dfs.oat", "", "79")]);
  (* Test("Others: Tree", executed_oat_file [("studenttests/tree.oat", "", "13")]); *) (* TODO accroding to their post this will follow *)
	Test("Student Provided vector test", executed_oat_file [("studenttests/vector.oat", "hello oat test00000 test t", "5, 3, 9, 4, 10")]);
] 
