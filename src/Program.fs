open Foff

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

//open System


type Person =
  { Name : string
    Age  : int }

module Person =
  let make (a : string) (b : int) : Person =
    { Name = a
      Age  = b }

  let decoder : Person Decoder =
    let dec =
      make <!> Decode.field "name" Decode.string 
           <*> Decode.field "age"  Decode.int
    in dec

  let encoder : Person Encoder = fun it ->
    [ "name", Encode.string it.Name
      "age",  Encode.int    it.Age ]
    |> Encode.object

  let encoder' : Person Encoder =
    let image it =
      [ "name", Encode.string it.Name
        "age",  Encode.int    it.Age ]
    in contramap image Encode.object

[<EntryPoint>]
let main argv =
//  Decode.fromString Decode.string "\"Hello, world\""
//  Decode.fromString Decode.int "a42"
//  Decode.fromString Decode.guid "\"313632a8-4f03-402a-b9d8-9e6f3d878a50\""
//  Decode.fromString Person.decoder """{"name": "Patrik Andersson", "age": 3500}"""
//  |> printfn "Parsed %A"

  Person.make "Patrik Andersson" 47
  |> Person.encoder
  |> Encode.toString 2
  |> printfn "%A"


  0 // return an integer exit code