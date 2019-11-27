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
]

let brainfuck_tests = [
  (* Hello World from Wikipedia *)
  ("studenttests/brainfuck.oat", "ppppppppppsrppppppprpppppppppprppprpllllmerpporpopppppppooppporppollppppppppppppppporopppommmmmmommmmmmmmorpo", "Hello World!0");
  ("studenttests/brainfuck.oat", "ririririririspplerorororororo Abc123", "Cde3450");
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
  (* Test ("Others: Performance Comparison", [("studenttests/manually", assert_eq true false)]);  *) (* TODO the fuck ? *)
  Test("Others: Moodle Fraction Test", executed_oat_file [("studenttests/fraction.oat", "", "42")]);
  Test("Others: linked list tests", executed_oat_file [("studenttests/linkedlist.oat", "", "0")]); (* TODO discuss: wrong result ? https://gitlab.ethz.ch/flbuetle/compiler-design/commit/c42a0129556f6172dc48b993471a4c3eb18bf537 *)
  Test("Others: SP: brainfuck tests", executed_oat_file brainfuck_tests);
  Test("Others: Struct Student Test", executed_oat_file [("studenttests/structs.oat", "", "4000")]);
  Test("Others: Fulkerson Test", executed_oat_file [("studenttests/fulkerson.oat", "", "37")]); (* TODO discuss: variable redeclaration https://gitlab.ethz.ch/flbuetle/compiler-design/commit/6cbeed4da92d08339f0b7e2d56c71dc87174b614 *)
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
  Test("Others: Binary tree with insertion and deletion", executed_oat_file [("studenttests/binary_tree_structs.oat", "", "-5 -4 -3 -2 -1 1 2 3 4 5 | -5 -4 -2 -1 2 3 5 | return: 1")]); (* TODO discuss: if? for int[] *)
  Test("Others: Palindrome", executed_oat_file [("studenttests/palindrome.oat", "","42")]);
  Test("Others: Binary Tree", executed_oat_file [("studenttests/BinaryTree.oat", "", "0")]);
  Test("Others: Perceptron Test", executed_oat_file [("studenttests/single_perceptron.oat", "", "correctly classified1")]);
  Test("Others: complex numbers", executed_oat_file [("studenttests/complexnumbers.oat", "", "5")]);
  Test("Others: Moodle Oat Test", executed_oat_file [("studenttests/oat_test.oat", "", "2")]);
] 
