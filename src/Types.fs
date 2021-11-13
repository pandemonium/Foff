namespace Foff

open FSharpPlus
open FSharpPlus.Control
open FSharpPlus.Data


type JsonValue = Newtonsoft.Json.Linq.JToken

type DecodeState =
    { Path  : string
      Value : JsonValue }

type Because = ParserException of exn
             | ExpectedType    of expectation : string * state : DecodeState
             | Fail            of string
             | AndThen         of Because * Because

type Because with
  static member (+) (p, q) = AndThen (p, q)

type 'a Out = Result<'a, Because>

type 'a Decoder = ReaderT<DecodeState, 'a Out>

(* StateT probably. *)
type 'a Encoder = 'a -> JsonValue