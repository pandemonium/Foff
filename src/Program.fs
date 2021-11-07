open System

open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Data

open Foff


type Person =
  { Name : string
    Age  : int }

module Person =
  let make a b =
    { Name = a; Age = b }

  let decoder : Person Decoder =
    make <!> Decode.field "name" Decode.string
         <*> Decode.field "age"  Decode.int

[<EntryPoint>]
let main argv =



//  Decode.fromString Decode.string "\"Hello, world\""
//  Decode.fromString Decode.int "a42"
//  Decode.fromString Decode.guid "\"313632a8-4f03-402a-b9d8-9e6f3d878a50\""
  Decode.fromString (Decode.field "name" Decode.int) """{"name": "Patrik Andersson", "age": 3500}"""
  |> printfn "Parsed %A"
  0 // return an integer exit code