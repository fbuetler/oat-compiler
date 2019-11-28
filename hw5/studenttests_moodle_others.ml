open Assert
open Astlib
open Ast
open Driver
open Gradedtests
open Tctxt
open Typechecker

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

let context2 = { Tctxt.locals = [ ("i", TInt) ]
               ; Tctxt.globals = []
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

let square = "square", [{fieldName="l"; ftyp=TInt}]
let rectangle = "rectangle", [{fieldName="l"; ftyp=TInt};{fieldName="h"; ftyp=TInt}]


let myctxt : Tctxt.t = {
  structs = [square;rectangle];
  locals = [];
  globals = [];

}

let testing_ctxt = 
  let c' = Tctxt.add_struct Tctxt.empty "x" [] in
  let c' = Tctxt.add_struct c' "y" [] in
  let c' = Tctxt.add_struct c' "z" [{fieldName="f1";ftyp=(TRef (RStruct "x"))}] in
  Tctxt.add_struct c' "k" [{fieldName="f2";ftyp=(TRef (RStruct "y"))}]


let for_stmt_c = no_loc @@ For (["x", no_loc (CInt 0L)], None, None, [])

(* Returning in update statement disallowed *)
let for_stmt_f = no_loc @@ For (["x", no_loc (CInt 0L)], None, Some (no_loc @@ Ret None), [])

let jan_sandro_struct_ctxt = {
  Tctxt.empty with structs = [
    ("S", [
        {fieldName="a"; ftyp=TBool};
        {fieldName="b"; ftyp=TInt};
      ]);
    ("S_sub", [
        {fieldName="a"; ftyp=TBool};
        {fieldName="b"; ftyp=TInt};
        {fieldName="c"; ftyp=TRef(RString)}
      ]);
  ]
}

(* positive test case *)
let tc_for_correct_statement =
  let vdecl = [("i", no_loc (CBool true))] in
  let condition_opt = Some (no_loc (Id "i")) in
  let increment_opt = None in
  let block = [] in
  no_loc (For (vdecl, condition_opt, increment_opt, block))

(* negative test case *)
let tc_for_error_statement =
  let vdecl = [("i", no_loc (CBool true))] in
  let condition_opt = Some (no_loc (Id "i")) in
  let increment_opt = None in
  let block = [no_loc @@ Decl ("i", no_loc (CBool true))] in (* boolean i redeclared in block *)
  no_loc (For (vdecl, condition_opt, increment_opt, block))


let exp_typ_carr_s = no_loc (CArr (TInt, [no_loc (CInt 0L); no_loc (CInt 1L); no_loc (CInt 2L)]))
let exp_typ_carr_f = no_loc (CArr (TInt, [no_loc (CInt 0L); no_loc (CBool true); no_loc (CInt 2L)]))

let en_int : Ast.exp Ast.node = no_loc (CInt 1L)
let en_bool : Ast.exp Ast.node = no_loc (CBool true)
let en_array : Ast.exp Ast.node = no_loc (CArr (TBool, [no_loc (CBool true); no_loc (CBool false)]))

let ctxt : Tctxt.t = {Tctxt.empty with structs=["S1", [{fieldName="f1"; ftyp=TBool};
                                                       {fieldName="f2"; ftyp=TInt}]; "S2", [{fieldName="f1"; ftyp=TBool}]]}

let test_context = {
  Tctxt.empty with structs = [
    ("One", [
        {fieldName="bool"; ftyp=TBool};
        {fieldName="String"; ftyp=TNullRef (RString)};
      ]);
    ("Two", [
        {fieldName="bool"; ftyp=TBool};
      ]);
    ("Three", [
        {fieldName="bool"; ftyp=TBool};
        {fieldName="String"; ftyp=TRef (RString)};
      ]);
  ]; 
}

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

let tctxt : Tctxt.t = {
  Tctxt.empty with structs = [
    "A", [{fieldName="x"; ftyp=TInt}; {fieldName="y"; ftyp=TInt}; {fieldName="z"; ftyp=TInt}; {fieldName="visible"; ftyp=TBool}]; 
    "B", [{fieldName="x"; ftyp=TInt}; {fieldName="y"; ftyp=TInt}]
  ]}

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

let c1 = add_struct Tctxt.empty "s1" [{fieldName="a"; ftyp=TInt}; {fieldName="b"; ftyp=TInt}; {fieldName="c"; ftyp=TInt}]
let c2 = add_struct c1 "s2" [{fieldName="a"; ftyp=TInt}; {fieldName="b"; ftyp=TInt}]
let c3 = add_struct c2 "s3" [{fieldName="a"; ftyp=TInt}]
let c4 = add_struct c3 "s4" [{fieldName="a"; ftyp=TInt}; {fieldName="c"; ftyp=TInt}]

let typecheck path () =
  let () = Platform.verb @@ Printf.sprintf "** Processing: %s\n" path in
  let oat_ast = parse_oat_file path in
  Typechecker.typecheck_program oat_ast

let typecheck_error (a : assertion) () =
  try a (); failwith "Should have a type error" with Typechecker.TypeError s -> ()

let typecheck_correct (a : assertion) () =
  try a () with Typechecker.TypeError s -> failwith ("Should not have had a type error ("^s^")")


let typecheck_file_error tests =
  List.map (fun p -> p, typecheck_error (typecheck p)) tests

let typecheck_file_correct tests =
  List.map (fun p -> p, typecheck_correct (typecheck p)) tests

let unit_test_tctxt = Tctxt.empty
let unit_test_tctxt = Tctxt.add_struct unit_test_tctxt "s0" []
let unit_test_tctxt = Tctxt.add_struct unit_test_tctxt "s1" [{ fieldName = "f0"; ftyp = TBool }]
let unit_test_tctxt = Tctxt.add_struct unit_test_tctxt "s2" [{ fieldName = "f0"; ftyp = TInt }]
let unit_test_tctxt = Tctxt.add_struct unit_test_tctxt "s3" [{ fieldName = "f0"; ftyp = TInt }; { fieldName = "f1"; ftyp = TBool }]
let unit_test_tctxt = Tctxt.add_local unit_test_tctxt "l0" TInt
let unit_test_tctxt = Tctxt.add_local unit_test_tctxt "l1" TBool
let unit_test_tctxt = Tctxt.add_global unit_test_tctxt "g0" TInt
let unit_test_tctxt = Tctxt.add_global unit_test_tctxt "g1" TBool

let subtype_pass n a b =
  n,
  (fun () ->
     if Typechecker.subtype unit_test_tctxt a b then
       ()
     else
       failwith "should not fail"
  )

let subtype_fail n a b =
  n,
  (fun () ->
     if Typechecker.subtype unit_test_tctxt a b then
       failwith "should not succeed"
     else
       ()
  )

let subtype_ret_pass n a b = subtype_pass n (TRef(RFun([], a))) (TRef(RFun([], b)))
let subtype_ret_fail n a b = subtype_fail n (TRef(RFun([], a))) (TRef(RFun([], b)))

let typecheck_exp_inner e t =
  let act = Typechecker.typecheck_exp unit_test_tctxt e in
  if act <> t then
    failwith "Types did not match"
  else
    ()

let typecheck_exp_fail n e t =
  n, (fun () ->
      try typecheck_exp_inner e t; failwith "Should have a type error" with Typechecker.TypeError s -> ()
    )

let typecheck_exp_pass n e t =
  n, (fun () ->
      try typecheck_exp_inner e t with Typechecker.TypeError s -> failwith ("Should not have had a type error ("^s^")")
    )

let nl (e:Ast.exp) = Ast.no_loc e

let unit_tests_binop n ops tp tf tr = List.flatten (List.map (fun op -> [
      typecheck_exp_pass (n^"_0") (nl(Bop(op, nl(tp), nl(tp)))) tr;
      typecheck_exp_fail (n^"_1") (nl(Bop(op, nl(tf), nl(tp)))) tr;
      typecheck_exp_fail (n^"_2") (nl(Bop(op, nl(tp), nl(tf)))) tr;
      typecheck_exp_fail (n^"_3") (nl(Bop(op, nl(tf), nl(tf)))) tr;
    ]) ops)

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
  ("subtype_func_ret",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RFun ([], RetVoid))) (TRef (RFun ([], RetVoid))) then ()
      else failwith "should not fail")                                                                                     
  ); 
  ("no_subtype_func_ret",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RFun ([], RetVal (TNullRef RString)))) (TRef (RFun ([], RetVoid))) then
        failwith "should not succeed" else ())
  );
  ("typ_newarray_ok",
   (fun () ->
      let t = Typechecker.typecheck_exp context2 (no_loc (NewArr (TInt, no_loc (CInt 5L), "x", no_loc (CInt 0L)))) in
      if (t = TRef (RArray TInt))
      then ()
      else failwith "should not fail"
   )
  );
  ("typ_newarray_err",
   (fun () ->
      try 
        let _ = Typechecker.typecheck_exp context2 (no_loc (NewArr (TInt, no_loc (CInt 5L), "i", no_loc (CInt 0L))))
        in failwith "should have a type error"
      with Typechecker.TypeError s -> ()
   )
  );
  ("subtype_func_ret",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RFun ([], RetVoid))) (TRef (RFun ([], RetVoid))) then ()
      else failwith "should not fail")                                                                                     
  ); 
  ("no_subtype_func_ret",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RFun ([], RetVal (TNullRef RString)))) (TRef (RFun ([], RetVoid))) then
        failwith "should not succeed" else ())
  );
  ("typecheck fdecl 1", typecheck_correct
     (fun () -> 
        Typechecker.typecheck_fdecl Tctxt.empty { frtyp = RetVal TInt
                                                ; fname = "f"
                                                ; args = []
                                                ; body = [ no_loc (Decl ("a", no_loc (CInt 42L)))
                                                         ; no_loc (Ret (Some (no_loc (Id "a"))))
                                                         ]
                                                } (no_loc "")
     )
  ); 
  ("typecheck fdecl 2", typecheck_error
     (fun () ->
        Typechecker.typecheck_fdecl Tctxt.empty { frtyp = RetVal TInt
                                                ; fname = "f"
                                                ; args = []
                                                ; body = [ no_loc (Decl ("a", no_loc (CInt 42L)))
                                                         ; no_loc (Ret (Some (no_loc (Id "a"))))
                                                         ; no_loc (Decl ("b", no_loc (CInt 42L)))
                                                         ; no_loc (Ret (Some (no_loc (Id "b"))))
                                                         ]
                                                } (no_loc "")
     )
  );
  ("overwrite global variable in for loop vdecls", (typecheck_correct (fun () -> Typechecker.typecheck_program
                                                                          [ Gvdecl (no_loc {name="x"; init=no_loc @@ CInt 0L})
                                                                          ; Gfdecl (no_loc {frtyp=RetVoid; fname="f"; args=[]; body=[ no_loc @@ For ([("x", no_loc @@ CInt 1L)], None, None, [])
                                                                                                                                    ; no_loc @@ Ret None ]});]))
  );
  ("overwrite local variable in for loop vdecls", (typecheck_error (fun () -> Typechecker.typecheck_program 
                                                                       [ Gfdecl (no_loc {frtyp=RetVoid; fname="f"; args=[]; body=[ no_loc @@ Decl ("x", no_loc @@ CInt 0L)
                                                                                                                                 ; no_loc @@ For ([("x", no_loc @@ CInt 1L)], None, None, [])
                                                                                                                                 ; no_loc @@ Ret None ]});]))
  );
  ("subtype_correct",
   (fun () ->
      if Typechecker.subtype Tctxt.empty ((TRef (RArray (TRef (RArray TBool))))) ((TRef (RArray (TRef (RArray TBool))))) then ()
      else failwith "should not fail")
  );
  ("subtype_fail",
   (fun () ->
      if Typechecker.subtype Tctxt.empty ((TRef (RArray (TRef (RArray TBool))))) (TRef (RArray ((TRef (RArray (TRef (RArray TBool))))))) then
        failwith "should not succeed" else ())
  );
  ("Subtype Test Positive",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TInt)) then ()
      else failwith "should not fail")
  ); 
  ("Subtype Test Negative",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RArray TInt)) (TRef (RArray TBool)) then
        failwith "should not succeed" else ())
  );
  ("sub_substruct_test",
   (fun () ->
      if Typechecker.subtype myctxt (TRef (RStruct "rectangle")) (TRef (RStruct "square")) then ()
      else failwith "shall not pass");
  );
  ("array_struct_test",
   (fun () ->
      if Typechecker.subtype myctxt (TRef (RStruct "rectangle")) (TNullRef (RArray TInt)) then failwith "shall not succeed"
      else ());
  );
  ("Positive Student Test",
   (fun () -> let ctxt = add_struct (
        add_struct (
          add_struct Tctxt.empty "A" [{ fieldName = "x"; ftyp = TInt }]
        ) "B" [{ fieldName = "x"; ftyp = TInt }; 
               { fieldName = "y"; ftyp = TRef (RStruct "A") }]
      ) "C" [{ fieldName = "x"; ftyp = TInt };
             { fieldName = "y"; ftyp = TRef (RStruct "A") };
             { fieldName = "z"; ftyp = TRef (RStruct "B") }] in
      if subtype ctxt (TRef (RStruct "B")) (TRef (RStruct "A")) &&
         subtype ctxt (TRef (RStruct "C")) (TRef (RStruct "B")) &&
         subtype ctxt (TRef (RStruct "C")) (TRef (RStruct "A")) &&
         (not (subtype ctxt (TRef (RStruct "A")) (TRef (RStruct "B")))) &&
         (not (subtype ctxt (TRef (RStruct "A")) (TRef (RStruct "C")))) &&
         (not (subtype ctxt (TRef (RStruct "B")) (TRef (RStruct "C")))) then ()
      else failwith "should not fail")
  ); 
  ("Negative Student Test",
   (fun () -> let ctxt = add_struct (
        add_struct (
          add_struct Tctxt.empty "A" [{ fieldName = "x"; ftyp = TInt }]
        ) "B" [{ fieldName = "x"; ftyp = TInt }; 
               { fieldName = "y"; ftyp = TRef (RStruct "A") }]
      ) "C" [{ fieldName = "x"; ftyp = TInt };
             { fieldName = "y"; ftyp = TRef (RStruct "A") };
             { fieldName = "z"; ftyp = TRef (RStruct "B") }] in
      if subtype ctxt (TRef (RStruct "A")) (TRef (RStruct "B")) ||
         subtype ctxt (TRef (RStruct "A")) (TRef (RStruct "C")) ||
         subtype ctxt (TRef (RStruct "B")) (TRef (RStruct "C")) ||
         (not (subtype ctxt (TRef (RStruct "C")) (TRef (RStruct "A")))) ||
         (not (subtype ctxt (TRef (RStruct "C")) (TRef (RStruct "B")))) ||
         (not (subtype ctxt (TRef (RStruct "B")) (TRef (RStruct "A")))) then failwith "should not succeed"
      else ())
  );
  ("student_subtye_struct", 
   (fun () ->
      if
        Typechecker.subtype
          testing_ctxt
          (TRef (RStruct "z"))
          (TRef (RStruct "k"))
      then failwith "should not succeed"
      else ())
  ); 
  ("student_CArr",
   (fun () ->
      try
        let _ = Typechecker.typecheck_exp
            testing_ctxt
            {elt= CArr (TNullRef (RStruct "z"), [{elt = CNull (RStruct "z"); loc = Range.norange}]);
             loc = Range.norange}
        in
        ()
      with Typechecker.TypeError x -> failwith ("should succed, got: " ^ x))
  );
  ("positive_unit_test",
   (fun () ->
      if Typechecker.subtype Tctxt.empty 
          (TRef (RArray (TRef (RFun ([TNullRef RString], RetVoid))))) 
          (TRef (RArray (TRef (RFun ([TNullRef RString], RetVoid)))))
      then ()
      else failwith "should not fail")
  ); 
  ("negative_unit_test",
   (fun () ->
      if Typechecker.subtype Tctxt.empty 
          (TRef (RArray (TRef (RFun ([TNullRef RString], RetVoid))))) 
          (TRef (RArray (TRef (RFun ([TRef RString], RetVoid)))))  
      then failwith "should not succeed" else ())
  );
  ("TYP_FOR_success",
   (fun () ->
      let c, r = Typechecker.typecheck_stmt Tctxt.empty for_stmt_c RetVoid in
      if (c, r) = (Tctxt.empty, false) then ()
      else failwith "should not fail")
  );
  (* ("TYP_FOR_fail",
     (fun () ->
     try
       let _ = Typechecker.typecheck_stmt Tctxt.empty for_stmt_f RetVoid in
       failwith "should not succeed"
       with Typechecker.TypeError s -> ())
     ); TODO: check why it is failing *)
  ("arr_subtype_neq", 
   (fun () ->
      if Typechecker.subtype jan_sandro_struct_ctxt (TRef (RArray (TRef(RStruct "S_sub")))) (TRef (RArray (TRef(RStruct "S")))) then
        failwith "should not succee")
  );
  ("arr_subtype_eq",
   (fun () ->
      if not @@ Typechecker.subtype jan_sandro_struct_ctxt (TRef (RArray (TRef(RStruct "S")))) (TRef (RArray (TRef(RStruct "S")))) then
        failwith "should succeed")
  );
  ("TYP_CARR_SUCCESS",
   (fun () ->
      ignore (try Typechecker.typecheck_exp Tctxt.empty exp_typ_carr_s
              with TypeError s -> failwith "should succeed"))                                                                                  
  ); 
  ("TYP_CARR_FAIL",
   (fun () ->
      ignore (try ignore (Typechecker.typecheck_exp Tctxt.empty exp_typ_carr_f); failwith "should not succeed"
              with TypeError s -> TBool))
  );
  ("our_tc_for_correct",
   typecheck_correct (fun () -> let _ = typecheck_stmt Tctxt.empty tc_for_correct_statement RetVoid in ())
  ); 
  ("our_tc_for_error",
   typecheck_error (fun () -> let _ = typecheck_stmt Tctxt.empty tc_for_error_statement RetVoid in ())
  );
  ("Length_succeeds",
   (fun () ->
      if Typechecker.typecheck_exp Tctxt.empty (no_loc (Length (no_loc (NewArr( TInt, no_loc ( CInt 2L), "ar1", no_loc (CInt 1L)))))) = TInt then ()
      else failwith "should not fail")
  ); 
  ("Binop_Neq_fails",
   typecheck_error (fun () ->
       if Typechecker.typecheck_exp Tctxt.empty (no_loc (Length (no_loc (CInt 1L)))) = TInt then ()
       else failwith "should not succeed")  
  );
  (* ("subtype_positive",
     (fun () ->
     if Typechecker.subtype subt_ctxt (TRef (RStruct "Octopus")) (TRef (RStruct "Human"))
     then ()
     else failwith "should not fail")
     );
     ("subtype_negative",
     (fun () ->
     if not @@ Typechecker.subtype subt_ctxt (TRef (RStruct "Human")) (TRef (RStruct "Octopus"))
     then ()
     else failwith "should not fail")
     ); TODO: where is the subt_ctxt *)
  ("Index_ok",
   (fun () ->
      if Typechecker.typecheck_exp Tctxt.empty (no_loc (Index (en_array, en_int))) == TBool then ()
      else failwith "should not fail")
  ); 
  ("Index_not_int_error",
   typecheck_error (fun () ->
       if Typechecker.typecheck_exp Tctxt.empty (no_loc (Index (en_array, en_bool))) == TBool then ()
       else failwith "should not fail")  
  );
  ("sub_subrstruct_correct", (fun () -> if Typechecker.subtype_ref ctxt
                                 (RStruct "S1") (RStruct "S2") then () else failwith "should not fail")
  );
  ("sub_subrstruct_incorrect", (fun () -> if Typechecker.subtype_ref ctxt
                                   (RStruct "S2") (RStruct "S1") then failwith "should not succeed" else ())
  );
  ("newarr_variable_redefinition",
   Gradedtests.typecheck_error
     (fun () ->
        let _ =
          Typechecker.typecheck_exp
            (Tctxt.add_local Tctxt.empty "y" TInt)
            (Ast.no_loc (Ast.NewArr (TInt, Ast.no_loc (Ast.CInt 2L),
                                     "y",
                                     Ast.no_loc (Ast.CInt 3L))))
        in
        ()
     )
  );
  ("newarr_global_shadow",
   Gradedtests.typecheck_correct
     (fun () ->
        let _ =
          Typechecker.typecheck_exp
            (Tctxt.add_global Tctxt.empty "y" TInt)
            (Ast.no_loc (Ast.NewArr (TInt, Ast.no_loc (Ast.CInt 2L),
                                     "y",
                                     Ast.no_loc (Ast.CInt 3L))))
        in
        ()
     )
  );
  ("Typechecker: Struct with more fields",
   (fun () ->
      if Typechecker.subtype test_context (TRef (RFun ([TInt; TRef (RStruct "Two")], RetVal (TRef (RStruct "Three"))))) 
          (TRef (RFun ([TInt; TRef (RStruct "One")], RetVal (TRef (RStruct "Two"))))) then ()
      else failwith "should not fail")
  ); 
  ("Typechecker: Struct with different ftyp",
   (fun () ->
      if  Typechecker.subtype test_context (TRef (RFun ([TInt; TRef (RStruct "Three")], RetVal (TRef (RStruct "Three"))))) 
          (TRef (RFun ([TInt; TRef (RStruct "One")], RetVal (TRef (RStruct "Two")))))
          ||
          Typechecker.subtype test_context (TRef (RFun ([TInt; TRef (RStruct "Two")], RetVal (TRef (RStruct "Two"))))) 
            (TRef (RFun ([TInt; TRef (RStruct "One")], RetVal (TRef (RStruct "One")))))
      then 
        failwith "should not succeed" else ())
  );
  ("subtype_TRef_Rfun_TNullref_Rfun",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RFun ([TNullRef RString], RetVal (TRef (RArray TInt))))) (TNullRef (RFun ([TRef RString], RetVal (TNullRef (RArray TInt))))) then ()
      else failwith "should not fail")                                                                                     
  ); 
  ("no_subtype_TRef_Rfun_TNullref_Rfun",
   (fun () ->
      if Typechecker.subtype Tctxt.empty (TRef (RFun ([TRef RString], RetVal (TRef (RArray TInt))))) (TNullRef (RFun ([TNullRef RString], RetVal (TNullRef (RArray TInt))))) then
        failwith "should not succeed" else ())
  );
  (* ("typecheck_exp_NewArr",
     (fun () ->
       if Typechecker.typecheck_exp (Tctxt.add_local Tctxt.empty "ididid" TInt) (Ast.no_loc (Ast.NewArr (TInt, (Ast.no_loc (Ast.CInt 1L)), "ididid", (Ast.no_loc (Ast.Id "ididid"))))) = TRef (RArray TInt) then ()
       else failwith "should not fail")                                                                                     
     ); TODO: prohibited variable redelclaration *)
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
  ("sub_subrstruct", (fun () -> 
       begin if Typechecker.subtype_ref tctxt (RStruct "A") (RStruct "B") then 
           () 
         else 
           failwith "should not fail"
       end)
  );
  ("no_sub_subrstruct", (fun () -> 
       begin if Typechecker.subtype_ref tctxt (RStruct "B") (RStruct "A") then 
           failwith "should fail" 
         else 
           ()
       end)
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
  ("subtype_struct_struct",
   (fun () ->
      if Typechecker.subtype  c4 (TRef (RStruct "s1")) (TRef (RStruct "s2")) &&
         Typechecker.subtype  c4 (TRef (RStruct "s1")) (TRef (RStruct "s3")) &&
         Typechecker.subtype  c4 (TRef (RStruct "s2")) (TRef (RStruct "s3")) then ()
      else failwith "should not fail")                                                                                     
  ); 
  ("no_subtype_struct_struct",
   (fun () ->
      if Typechecker.subtype  c4 (TRef (RStruct "s3")) (TRef (RStruct "s1")) ||
         Typechecker.subtype  c4 (TRef (RStruct "s2")) (TRef (RStruct "s1")) ||
         Typechecker.subtype  c4 (TRef (RStruct "s3")) (TRef (RStruct "s2")) ||
         Typechecker.subtype  c4 (TRef (RStruct "s1")) (TRef (RStruct "s4")) then
        failwith "should not succeed" else ())
  );
  (* (unit_tests_binop "typeck_binop_int" [Add; Sub; Mul; IAnd; IOr; Shl; Shr; Sar] (CInt(0L)) (CBool(false)) TInt);
     (unit_tests_binop "typeck_binop_cmp" [Lt; Lte; Gt; Gte] (CInt(0L)) (CBool(false)) TBool);
     (unit_tests_binop "typeck_binop_bool" [And; Or] (CBool(false)) (CInt(0L)) TBool); *)
  ("TYP_STRUCTEX, reordering correct",
   Gradedtests.typecheck_correct (fun () ->
       let ctxt         = Tctxt.add_struct Tctxt.empty "Maybe" [{fieldName = "case"; ftyp = TInt}; {fieldName = "some_val"; ftyp = TInt}] in
       let maybe_struct = Ast.CStruct ("Maybe", [("some_val", Ast.no_loc (Ast.CInt 42L)); ("case", Ast.no_loc (Ast.CInt 1L))]) in
       let exp          = Ast.no_loc maybe_struct in
       let _            = Typechecker.typecheck_exp ctxt exp in
       ())
  );
  ("TYP_STRUCTEX, wrong field names incorrect",
   Gradedtests.typecheck_error (fun () ->
       let ctxt         = Tctxt.add_struct Tctxt.empty "Maybe" [{fieldName = "case"; ftyp = TInt}; {fieldName = "some_val"; ftyp = TInt}] in
       let maybe_struct = Ast.CStruct ("Maybe", [("field1", Ast.no_loc (Ast.CInt 1L)); ("field2", Ast.no_loc (Ast.CInt 42L))]) in
       let exp          = Ast.no_loc maybe_struct in
       let _ = Typechecker.typecheck_exp ctxt exp in
       ())
  );
]

let brainfuck_tests = [
  (* Hello World from Wikipedia *)
  ("studenttests/brainfuck.oat", "ppppppppppsrppppppprpppppppppprppprpllllmerpporpopppppppooppporppollppppppppppppppporopppommmmmmommmmmmmmorpo", "Hello World!0");
  ("studenttests/brainfuck.oat", "ririririririspplerorororororo Abc123", "Cde3450");
]

let rucksackTest = 
  [
    ("studenttests/rucksack.oat", "0 20 5 2 2 3 10 10 5 100 3 5 1 100", "20");
    ("studenttests/rucksack.oat", "0 15 5 2 2 3 10 10 5 100 3 5 1 100", "110");
    ("studenttests/rucksack.oat", "1 20 5 2 2 3 10 10 5 100 3 5 1 100", "215");
    ("studenttests/rucksack.oat", "1 15 5 2 2 3 10 10 5 100 3 5 1 100", "208");
    ("studenttests/rucksack.oat", "2 20 5 2 2 3 10 10 5 100 3 5 1 100", "215");
    ("studenttests/rucksack.oat", "2 15 5 2 2 3 10 10 5 100 3 5 1 100", "208");
    ("studenttests/rucksack.oat", "0 20 5 2 3 4 1 10 12 40 10 30 9 30", "46");
    ("studenttests/rucksack.oat", "1 20 5 2 3 4 1 10 12 40 10 30 9 30", "54");
    ("studenttests/rucksack.oat", "2 20 5 2 3 4 1 10 12 40 10 30 9 30", "70");
  ]

let prepend list = List.map (fun (path, input, output) -> 
    ("studenttests/" ^ path, input, output)
  ) list

(* Push string "Hello" onto the stack and print each character one by one *)
let prog1 = String.concat "\n" 
    ["P111"; "P108"; "P108"; "P101"; "P72"; "."; "."; "."; "."; "."; "E"; ]

(* Push Null-terminated string "Hello" onto the stack write it to memory and 
 * print each character one by one *)
let prog2 = String.concat "\n"
    [
      (* Push string characters on the stack *)
      "P0"; "P111"; "P108"; "P108"; "P101"; "P72";
      (* Store string in memory *)
      "P0"; ">"; "P1"; ">"; "P2"; ">"; "P3"; ">"; "P4"; ">";

      "P0"; "<"; ".";
      "P1"; "<"; ".";
      "P2"; "<"; ".";
      "P3"; "<"; ".";
      "P4"; "<"; ".";
      "E"
    ]

(* Push Null-terminated string "Hello" onto the stack, write it to memory,
 * and print each character in a loop *)
let prog3 = String.concat "\n" 
    [
      (* Push string characters on the stack in reverse order *)
      "P0"; "P111"; "P108"; "P108"; "P101"; "P72";
      (* Store string in memory *)
      "P1"; ">"; "P2"; ">"; "P3"; ">"; "P4"; ">"; "P5"; ">";
      (* Print string from memory *)

      (* Set counter at mem[0] to 1 *)
      "P1"; "P0"; ">";

      (* Read counter *)
      "P0"; "<";

      (* Load character from memory *)
      "<";

      "C";
      (* Terminate if character is \0 *)
      "?8";

      "."; 
      (* Increment counter at mem[0] *)
      "P0"; "<"; "P1"; "+"; "P0"; ">";

      (* Jump back to Read counter *)
      "J-13";

      "E";
    ]

(* Calculate factorial *)
let prog4 = String.concat "\n" [
    (* The number to calculate the factorial of will be prepended by the
     * testcase *)

    (* Store result in mem[0] *)
    "P1"; "P0"; ">";

    "C"; "?10";

    "C"; "P0"; "<"; "*"; "P0"; ">";

    "P-1"; "+";
    "J-11";

    "P0"; "<"; ",";
    "P32"; ".";
    "E";
  ]

(* Print fibonacci numbers *)
let prog5 = String.concat "\n" [
    (* The number of iterations is prepended by the testcase *)
    "P0"; ">";
    "P0"; "P1"; 

    "P0"; "<"; "?21";

    "C"; ",";

    "C"; "P1"; ">"; 

    "+"; "P2"; ">";

    "P1"; "<";
    "P2"; "<";

    (* Decrement counter *)
    "P0"; "<"; "P-1"; "+"; "P0"; ">";

    (* Print space *)
    "P32"; ".";

    "J-24";
    "E";
  ]

let sl_tests = prepend [
    ("sl.oat", "\'" ^ prog1 ^ "\'", "Hello0");
    ("sl.oat", "\'" ^ prog2 ^ "\'", "Hello0");
    ("sl.oat", "\'" ^ prog3 ^ "\'", "Hello0");
    ("sl.oat", "\'P0\n" ^ prog4 ^ "\'", "1 0");
    ("sl.oat", "\'P1\n" ^ prog4 ^ "\'", "1 0");
    ("sl.oat", "\'P2\n" ^ prog4 ^ "\'", "2 0");
    ("sl.oat", "\'P3\n" ^ prog4 ^ "\'", "6 0");
    ("sl.oat", "\'P4\n" ^ prog4 ^ "\'", "24 0");
    ("sl.oat", "\'P5\n" ^ prog4 ^ "\'", "120 0");
    ("sl.oat", "\'P6\n" ^ prog4 ^ "\'", "720 0");
    ("sl.oat", "\'P1\n" ^ prog5 ^ "\'", "1 0");
    ("sl.oat", "\'P2\n" ^ prog5 ^ "\'", "1 1 0");
    ("sl.oat", "\'P3\n" ^ prog5 ^ "\'", "1 1 2 0");
    ("sl.oat", "\'P4\n" ^ prog5 ^ "\'", "1 1 2 3 0");
    ("sl.oat", "\'P5\n" ^ prog5 ^ "\'", "1 1 2 3 5 0");
    ("sl.oat", "\'P6\n" ^ prog5 ^ "\'", "1 1 2 3 5 8 0");
    ("sl.oat", "\'P7\n" ^ prog5 ^ "\'", "1 1 2 3 5 8 13 0");
    ("sl.oat", "\'P8\n" ^ prog5 ^ "\'", "1 1 2 3 5 8 13 21 0");
    ("sl.oat", "\'P9\n" ^ prog5 ^ "\'", "1 1 2 3 5 8 13 21 34 0");
    ("sl.oat", "\'P10\n" ^ prog5 ^ "\'", "1 1 2 3 5 8 13 21 34 55 0");
    ("sl.oat", "\'P11\n" ^ prog5 ^ "\'", "1 1 2 3 5 8 13 21 34 55 89 0");
  ]

let provided_tests : suite = [
  Test("Others subtype", unit_tests);
  Test("Others: another_su.oat", executed_oat_file [("studenttests/another_sum.oat", "", "200")]); 
  Test("Others: Moodle Fraction Test", executed_oat_file [("studenttests/fraction.oat", "", "42")]);
  Test("Others: linked list tests", executed_oat_file [("studenttests/linkedlist.oat", "", "0")]); (* Note: modified *)
  Test("Others: SP: brainfuck tests", executed_oat_file brainfuck_tests);
  Test("Others: Struct Student Test", executed_oat_file [("studenttests/structs.oat", "", "4000")]);
  Test("Others: Fulkerson Test", executed_oat_file [("studenttests/fulkerson.oat", "", "37")]); (* Note: modified *)
  Test("Others: hard test", executed_oat_file [("studenttests/game.oat", "", "120")]);
  Test("Others: Inverted-Index-Boolean-Query",executed_oat_file [("studenttests/inv_index.oat", "","12354")]);
  Test("Others: Directed DFS Test", executed_oat_file [("studenttests/directed_dfs.oat", "", "79")]);
  (* Test("Others: Tree", executed_oat_file [("studenttests/tree.oat", "", "13")]); *) (* TODO accroding to their post this will follow *)
  Test("Others: vector test", executed_oat_file [("studenttests/vector.oat", "hello oat test00000 test t", "5, 3, 9, 4, 10")]);
  Test("Others: search_tree", executed_oat_file [("studenttests/searchtree.oat", "", "29335566255")]);
  Test("Others: sl tests", executed_oat_file sl_tests);
  Test("Others: search_tree", executed_oat_file [("studenttests/searchtree.oat", "", "29335566255")]);
  Test("Others: NevilleTest", executed_oat_file [("studenttests/treePostorder.oat", "", "579112210")]);
  Test("Others: tc struct tests", typecheck_file_correct ["studenttests/tc_correct_struct.oat"]);
  Test("Others: tc struct tests", typecheck_file_error [ "studenttests/tc_error_struct.oat"; "studenttests/tc_error_struct_recursion.oat";]);
  Test("Others: Generators", executed_oat_file [("studenttests/gen.oat", "", "810111214161718201420263238445056620")]);
  Test("Others: Binary tree with insertion and deletion", executed_oat_file [("studenttests/binary_tree_structs.oat", "", "-5 -4 -3 -2 -1 1 2 3 4 5 | -5 -4 -2 -1 2 3 5 | return: 1")]); (* Note: modified *)
  Test("Others: Palindrome", executed_oat_file [("studenttests/palindrome.oat", "","42")]);
  Test("Others: Binary Tree", executed_oat_file [("studenttests/BinaryTree.oat", "", "0")]);
  Test("Others: Perceptron Test", executed_oat_file [("studenttests/single_perceptron.oat", "", "correctly classified1")]);
  Test("Others: complex numbers", executed_oat_file [("studenttests/complexnumbers.oat", "", "5")]);
  Test("Others: Moodle Oat Test", executed_oat_file [("studenttests/oat_test.oat", "", "2")]); (* Note: modified *)
  Test("Others: moodle tests", executed_oat_file [("studenttests/treap.oat", "", "[[[[(-5, 1668674806)], (2, 1000676753)], (8, 908095735)], (16, 71666532), [[(27, 1250496027)], (42, 1116302264)]]0")]);
  Test("Others: Areas Test", executed_oat_file [("studenttests/areas.oat", "", "45")]);
  Test("Others: flist test", executed_oat_file [("studenttests/flist.oat", "", "8")]);
  Test("Others: oat test", executed_oat_file [
      ("studenttests/military_grade_encryption.oat", "e HelloWorld securekey", "EagfhOfhaR0");
      ("studenttests/military_grade_encryption.oat", "d EagfhOfhaR securekey", "HelloWorld0");
    ]);
  Test("Others: ArrayList test", executed_oat_file [("studenttests/arraylist.oat", "", "3 6 8 9 10 12 14 16 24 42 1337 4 5 6 7 8 9 C:24")]);
  Test("Others: typechecking our list middle test", typecheck_file_correct ["studenttests/list_middle.oat"]);
  Test("Others: running our list middle test", executed_oat_file [("studenttests/list_middle.oat", "", "50")]);
  Test("Others: Three D Test", executed_oat_file [("studenttests/threed.oat", "", "110")]);
  Test("Others: Heapsort", executed_oat_file [("studenttests/heapsort.oat", "", "fhkmopsy{0")]);
  Test("Others: Kronecker Student Test", executed_oat_file [("studenttests/struct_kron.oat", "", "896532362420566342351347412162872128492883832321214565621125148204714357161812102427181524271815268143912213912214161666242496242492410236153361530")]);
  Test("Others: function pointers", executed_oat_file["studenttests/fptrs.oat", "", "1"]);
  Test("Others: scope_struct_stress", executed_oat_file [("studenttests/testitest.oat", "", "111222\nfalse, true, 22, true, 22, 222\n20, 20, 10, 10, 10, 990\n990, 10, 7770, 7770, 7770")]); (* Note: Modified a few things *) 
  Test("Others: Rucksack Tests", executed_oat_file rucksackTest);
  (* Test("Others: IsTree ok 1", executed_oat_file [("studenttests/is_tree.oat", "1", "1");
                                                 ("studenttests/is_tree.oat", "0", "1");
                                                 ("studenttests/is_tree.oat", "2 0 1", "1");
                                                 ("studenttests/is_tree.oat", "3 0 1 1 2", "1");
                                                 ("studenttests/is_tree.oat", "4 0 1 0 2 0 3", "1");
                                                 ("studenttests/is_tree.oat", "5 0 1 0 2 0 3 3 4", "1");
                                                 ("studenttests/is_tree.oat", "6 0 1 1 2 2 3 2 4 2 5", "1"); ]);
     Test("Others: IsTree err 1", executed_oat_file[("studenttests/is_tree.oat", "2", "0");
                                                 ("studenttests/is_tree.oat", "3 0 1", "0");
                                                 ("studenttests/is_tree.oat", "3 0 1 0 1", "0");
                                                 ("studenttests/is_tree.oat", "4 0 1 1 2 2 0", "0");
                                                 ("studenttests/is_tree.oat", "5 0 1 0 2 0 3 1 0", "0");
                                                 ("studenttests/is_tree.oat", "6 1 2 2 3 3 4 4 5 5 1", "0");
                                                 ("studenttests/is_tree.oat", "5", "0"); ]);
     Test("Others: IsTree input err 1", executed_oat_file [("studenttests/is_tree.oat", "finthechat", "254");
                                                        ("studenttests/is_tree.oat", "0 1 2", "254");
                                                        ("studenttests/is_tree.oat", "", "254");
                                                        ("studenttests/is_tree.oat", "2 1 2 1", "254"); ]); TODO: prohibited variable redelclaration*)
  Test("Others: student tests", executed_oat_file [("studenttests/s_min_dist.oat", "0", "03250")]);
  Test("Others: determinants", Gradedtests.executed_oat_file ["studenttests/determinant.oat", "", "2_25_-132_-30696_0"]);
  Test("Others: student tests", executed_oat_file [("studenttests/s_min_dist.oat", "0", "03250")]);
  Test("Others: pps", executed_oat_file [ ("studenttests/pps.oat", "", "136101552530")]);
  Test("Others: Schuiki/Zaruba Oat test", executed_oat_file [
      ("studenttests/levenstein.oat", "kitten sitting", "3 3032 0");
      ("studenttests/levenstein.oat", "apocalypse maleficent", "10 6045904 0");
    ]);
  Test("Others: Oat test, higher order functions", executed_oat_file [("studenttests/higher_order_functions.oat", "", "5")]);

] 
