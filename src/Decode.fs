module Foff.Decode

open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Data

open System
open System.Globalization
open System.IO

open Newtonsoft.Json
open Newtonsoft.Json.Linq

module DecodeState =
  let fromValueAt path value =
    { Path = path; Value = value }

  let state : DecodeState Decoder = ask

  let atField name value state =
    { state with Path  = $"{state.Path}.{name}"
                 Value = value }

  (* Can I match on the types to derive the JTokenType? *)

  let primitive<'r> tokenType : 'r Decoder=
    state >>= fun ({ Value = v } as state) ->
      if v.Type = tokenType
        then result <| v.Value<'r> ()
        else throw <| ExpectedType (tokenType.ToString (), state)


let fromValue path (decoder : 'a Decoder) =
  DecodeState.fromValueAt path >> ReaderT.run decoder

let fromString decoder value =
  let serializer = 
    JsonSerializerSettings ( DateParseHandling      = DateParseHandling.None,
                             CheckAdditionalContent = true )
    |> JsonSerializer.Create

  try use reader = new JsonTextReader (new StringReader (value))
      serializer.Deserialize<JsonValue> (reader)
      |> fromValue "$" decoder
  with :? JsonReaderException as ex ->
    throw <| ParserException ex

let string : string Decoder =
  DecodeState.primitive JTokenType.String

let int : int Decoder =
  DecodeState.primitive JTokenType.Integer

let liftParser (parse : string -> bool * 'a) : 'a Decoder =
  let apply state =
    parse >> function true, x -> result x
                    | _ -> throw <| ExpectedType (typeof<'a>.Name, state)
  in zip DecodeState.state string >>= uncurry apply

let guid : Guid Decoder =
  liftParser Guid.TryParse

let field (name : string) (decoder : 'a Decoder) : 'a Decoder =
  monad { let! st = DecodeState.state
          let fieldValue = st.Value.[name]
          return! local (DecodeState.atField name fieldValue) decoder }

