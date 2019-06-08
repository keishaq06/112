(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)
(* Keisha Quirimit kquirimi
 * Jeffrey Yang jedyang*)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)
let arrayTable = Tables.array_table
let want_dump = ref false
let varTable = Tables.variable_table

let rec getValue (memref : Absyn.memref) = 
    
    match memref with
        | Variable ident -> (Hashtbl.find varTable ident)
        | Arrayref (name, index) -> Array.get 
        (Hashtbl.find arrayTable name) 
        (match index with 
            | Number number -> (int_of_float number) - 1
            | Memref memref -> (int_of_float (getValue memref)) - 1
            | Unary (oper, expr) -> (int_of_float 
            ((Hashtbl.find Tables.unary_fn_table oper) 
            (eval_expr expr))) - 1
            | Binary (oper, expr1, expr2) -> (int_of_float 
            ((Hashtbl.find Tables.binary_fn_table oper) 
            (eval_expr expr1) (eval_expr expr2))) - 1 
        );
and           
 eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> getValue memref
    | Unary (oper, expr) -> Hashtbl.find 
    Tables.unary_fn_table oper (eval_expr expr)
    | Binary (oper, expr1, expr2) -> Hashtbl.find 
    Tables.binary_fn_table oper 
    (eval_expr expr1) (eval_expr expr2)


let setRef memref expr = 
    match memref with 
        | Variable ident -> Hashtbl.add varTable ident (eval_expr expr) 
        | Arrayref (name, index) -> 
                Array.set (Hashtbl.find arrayTable name)
        ((int_of_float (eval_expr index)) - 1) (eval_expr expr)





let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref = 
        try  let number = Etc.read_number ()
             in (match memref with
                | Variable ident -> Hashtbl.add varTable ident number
                | Arrayref (one, two) -> ()
                
                ; 
            
             print_newline ())
        with End_of_file -> 
         Hashtbl.add Tables.variable_table "eof" 1.0
          (*   (print_string "End_of_file"; print_newline ())*)
    in List.iter input_number memref_list

let interp_goto label : Absyn.program option = 
    Some(Hashtbl.find Tables.label_table label)

let interp_if expr label : Absyn.program option = 
    match expr with 
        | Binary (oper, expr1, expr2) -> let result = 
            (Hashtbl.find Tables.if_table oper) (eval_expr expr1) 
            (eval_expr expr2) in 
                if result then interp_goto label else None

        | Memref memref -> None
        | Unary (one, two) -> None
        | Number number -> None
    
    
    



let interp_stmt (stmt : Absyn.stmt) : 
    Absyn.program option = match stmt with
    | Dim (ident, expr) -> Hashtbl.add arrayTable ident (Array.make
    (int_of_float (eval_expr expr)) 0.0) ; None
    | Let (memref, expr) -> setRef memref expr; None
    | Goto label -> interp_goto label 
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list ; None
    | Input memref_list -> interp_input memref_list ; None


let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> let next_line = interp_stmt stmt in
        match next_line with
            | None -> interpret otherlines
            | Some line -> interpret line

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)



