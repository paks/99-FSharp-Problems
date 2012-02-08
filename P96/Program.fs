// This project type requires the F# PowerPack at http://fsharppowerpack.codeplex.com/releases
// Learn more about F# at http://fsharp.net

open System
open Microsoft.FSharp.Text.Lexing

open Ast
open Lexer
open Parser

let rec evalExpr expr =
    match expr with
    | Identifier ident          -> ident

printfn "Calculator"

let rec readAndProcess() =
    printf ":"
    match Console.ReadLine() with
    | "quit" -> ()
    | expr ->
        try
            printfn "Lexing [%s]" expr
            let lexbuff = LexBuffer<char>.FromString(expr)
            
            printfn "Parsing..."
            let expr = Parser.start Lexer.tokenize lexbuff
            
            printfn "Evaluating Equation..."
            let result = evalExpr expr
            
            printfn "Result: %s" (result.ToString())
            
        with ex ->
            printfn "Unhandled Exception: %s" ex.Message

        readAndProcess()

readAndProcess()

let identifier expr =
    try
        let lexbuff = LexBuffer<char>.FromString(expr)
        let expr = Parser.start Lexer.tokenize lexbuff
        evalExpr expr |> ignore
        true
    with ex ->
        false
