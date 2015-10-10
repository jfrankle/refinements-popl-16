module Synth.FocusCtx
open Lang
open Refns
open Skeleton
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// FOCUSING CONTEXT.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type public focusctx private(rep : list<skeleton>) =
    member public this.Rep = rep

    // Create a focusing context by inserting a list of initial values.
    static member public Create(rep : list<id_info>) = 
        focusctx(List.map skeleton.Create rep)

    // Push entries.
    member public this.Push(s : skeleton)            = focusctx(s :: this.Rep)
    member public this.PushMany(ls : list<skeleton>) = focusctx(ls @ this.Rep)

    // Pop entries.
    member public this.Head      with get() = this.Rep.Head
    member public this.Tail      with get() = focusctx(this.Rep.Tail)
    member public this.Empty     with get() = this.Rep.IsEmpty

    // Duplicate a world.
    member public this.DuplicateWorld(w, w') =
        focusctx(List.map (fun (skel:skeleton) -> skel.DuplicateWorld(w, w')) this.Rep)

    // Delete a world.
    member public this.DeleteWorld(w) =
        focusctx(List.map (fun (skel:skeleton) -> skel.DeleteWorld(w)) this.Rep)