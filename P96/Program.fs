// [snippet: (**) Problem 96 : Syntax checker]
/// In a certain programming language (Ada) identifiers are defined by the syntax diagram below.
///  
///   ---->|letter|---+-------------------------------------+------>
///                   |                                     |
///                   +--------------+------>|letter|---+---+
///                   |             / \                 |
///                   +--->| - |---+   +---->|digit |---+
///                   |                                 |
///                   +---------------------------------+
///
/// Transform the syntax diagram into a system of syntax diagrams which do not contain loops; i.e. 
/// which are purely recursive. Using these modified diagrams, write a predicate identifier/1 that can 
/// check whether or not a given string is a legal identifier.
///  
/// Example in Prolog: 
/// % identifier(Str) :- Str is a legal identifier 
///  
/// Example in F#: 
/// 
/// > identifier "this-is-a-long-identifier";;
/// val it : bool = true
/// > identifier "this-ends-in-";;
/// val it : bool = false
/// > identifier "two--hyphens";;
/// val it : bool = false

open System
open Microsoft.FSharp.Text.Lexing

open Ast
open Lexer
open Parser

let rec evalExpr expr =
    match expr with
    | Identifier ident          -> ident


let identifier expr =
    try
        let lexbuff = LexBuffer<char>.FromString(expr)
        let expr = Parser.start Lexer.tokenize lexbuff
        evalExpr expr |> ignore
        true
    with ex ->
        false
