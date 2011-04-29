module Main

open System
open Ast
open Parser
open Microsoft.FSharp.Text.Lexing

let program = @"
let /* The eight queens solver from Appel */
    var N := 8
    type intArray = array of int
    var row := intArray [ N ] of 0
    var col := intArray [ N ] of 0
    var diag1 := intArray [ N+N-1 ] of 0
    var diag2 := intArray [ N+N-1 ] of 0

    function printboard() =
    (
        for i := 0 to N-1 do 
        (
            for j := 0 to N-1 do 
                print(if col[i]=j then "" O"" else "" ."")
            ;
            print(""\n"")
        );
        print(""\n"")
    )

    function try(c:int) =
        if c=N 
        then 
            printboard()
        else 
            for r := 0 to N-1 do 
                if row[r]=0 & diag1[r+c]=0 & diag2[r+7-c]=0
                then 
                    (
                        row[r] := 1; 
                        diag1[r+c] := 1;
                        diag2[r+7-c] := 1; 
                        col[c] := r;
                                        try(c+1);
                        row[r] := 0; 
                        diag1[r+c] := 0;
                        diag2[r+7-c] := 0
                    )
in 
    try(0) 
end
"
let lexbuf = Lexing.LexBuffer<char>.FromString program
printfn "%s" program
printfn "================"

let ast = Parser.start Lexer.tokenize lexbuf
printfn "%A" ast

printfn "Performing semantic analysis"
printfn "================"

Semantic.analyze ast |> ignore

printfn "Semantic analysis done!"

printfn "Press any key"
Console.ReadKey |> ignore
