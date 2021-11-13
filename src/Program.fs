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

type AddressablePerson =
  { Denizen  : Person
    Location : Address }

and Address =
  { Street     : string
    PostalCode : int
    City       : string }

module Address =
  let make a b c =
    { Street = a; PostalCode = b; City = c }

  let decoder : Address Decoder =
    let dec =
      make <!> Decode.field "street"     Decode.string
           <*> Decode.field "postalCode" Decode.int
           <*> Decode.field "city"       Decode.string
    in dec

  let encoder : Address Encoder =
    let image it =
      [ "street",     Encode.string it.Street
        "postalCode", Encode.int    it.PostalCode
        "city",       Encode.string it.City ]
    in contramap image Encode.object

module AddressablePerson =
  let make a b =
    { Denizen = a; Location = b }

  let decoder : AddressablePerson Decoder =
    let dec =
      make <!> Decode.field "denizen"  Person.decoder
           <*> Decode.field "location" Address.decoder
    in dec

  let encoder : AddressablePerson Encoder =
    let image it =
      [ "denizen",  Person.encoder  it.Denizen
        "location", Address.encoder it.Location ]
    in contramap image Encode.object

type Identity = Free of Person | Addressable of AddressablePerson

module Identity =
  let decoder : Identity Decoder  =
    (Free <!> Person.decoder) <|> (Addressable <!> AddressablePerson.decoder)

  let encoder : Identity Encoder =
    function Free        p -> Person.encoder p
           | Addressable p -> AddressablePerson.encoder p    

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