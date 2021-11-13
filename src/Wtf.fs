module Wtf

open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Control


type Bad = Nej of string

type 'a Out = Result<'a, Bad>

type 'a Dec = ReaderT<string, 'a Out>

let decInt : int Dec = monad { return 1 }

let decString : string Dec = monad { return "" }

type Perzon =
  { Name : string
    Age  : int }

module Person =
  let make a b =
    { Name = a; Age = b }

(* Just fine - infers: ReaderT<string, Result<Perzon, Bad>> *)
let decoder =
  Person.make <!> decString <*> decInt

(* Type constraint mismatch when applying the default type 'obj' for a type 
   inference variable. No overloads match for method '<*>'. *)
//let decoder' : Perzon Dec =
//  Person.make <!> decString <*> decInt

(* Just fine. *)
let decoder'' : Perzon Dec =
  let f =
    Person.make <!> decString <*> decInt
  in f