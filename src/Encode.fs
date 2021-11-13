module Foff.Encode

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control

open System

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.IO


let string : string Encoder = fun it ->
  JValue (it) :> JsonValue

let int : int Encoder = fun it ->
  JValue (it) :> JsonValue

let object : (string * JsonValue) list Encoder =
  map (fun (k, v) -> JProperty(k, v)) >> fun x -> JObject x

let toString space (token : JsonValue) =
  let format     = if space = 0 then Formatting.None 
                                else Formatting.Indented
  use stream     = new StringWriter (NewLine = "\n")
  use jsonWriter = new JsonTextWriter (stream,
                                       Formatting  = format,
                                       Indentation = space)

  token.WriteTo jsonWriter
  stream.ToString ()