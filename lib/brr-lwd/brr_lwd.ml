open Brr
module Elwd = Elwd

let onload ~el v = 
  let onload' _ = 
    let root = Lwd.observe @@ v in
    let on_invalidate _ = 
      ignore @@ G.request_animation_frame @@ fun _ -> ignore (Lwd.quick_sample root)
    in
    Lwd.set_on_invalidate root on_invalidate;
    El.append_children el @@ Lwd.quick_sample root
  in
  ignore (Fut.map onload' Ev.(next load @@ Window.as_target G.window))

module Let_syntax = struct 
  let (let+) a f = Lwd.map ~f a
  let (let*) a f = Lwd.bind ~f a
end