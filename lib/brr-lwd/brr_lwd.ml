open Brr
module Elwd = Elwd

let onload v = 
  let onload' _ = 
    let root = Lwd.observe @@ v in
    let on_invalidate _ = 
      ignore @@ G.request_animation_frame @@ fun _ -> ignore (Lwd.quick_sample root)
    in
    Lwd.set_on_invalidate root on_invalidate;
    El.append_children (Document.body G.document) @@ Lwd.quick_sample root
  in
  ignore (Fut.map onload' Ev.(next load @@ Window.as_target G.window))

let use_state a = 
  let a = Lwd.var a in
  a, fun f -> Lwd.set a (f (Lwd.peek a))

module Let_syntax = struct 
  let (let+) a f = Lwd.map ~f a
  let (let*) a f = Lwd.bind ~f a
end