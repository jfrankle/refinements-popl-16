module Synth.Library
open Lang
open Util

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// LIBRARY.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Private state of the singleton module.
let mutable private env  : Map<id, value> = Map.empty
let mutable private gam  : Map<id, typ>   = Map.empty
let mutable private info : id_info_t list = []

// Add to the library.
let public Add(name:id, t:typ, body:value) =
    env  <- env.Add(name, body)
    gam  <- gam.Add(name, t)
    info <- {name=name; t=t; about=AIUninteresting} :: info

let public Info()  = info
let public Gam()   = gam
let public Env()   = env

// Print.
let ToString() =
    let print_entry (x, (t, v)) = sprintf "let %s : %O = %O" x t v
    String.concat "\n" (env.map(fun x v -> (gam.[x], v)).ToList().map(print_entry))