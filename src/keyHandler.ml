module V = Candyvec.Vector
module M = Candyvec.Matrix4

let camera_pos = ref {V.x = 0.0; y = 0.0; z = 0.80;}
let camera_trans = ref (M.translation !camera_pos)
let camera_at = V.zero

let get_camera_mat () =
  let camera_trans = !camera_trans in
  let new_pos = M.mult_vec ~mat:camera_trans ~vec:V.zero in
  camera_pos := new_pos;
  Sdlcaml.Gl.Util.Camera.make_matrix ~pos:new_pos
    ~at:V.zero ~up:(V.normal_axis `Y)

let has_shift =
  List.exists (function
    | Sdlcaml.Sdl.Key.KMOD_LSHIFT -> true
    | Sdlcaml.Sdl.Key.KMOD_RSHIFT -> true
    | _ -> false)

let handler ~key ~state =
  let open Sdlcaml.Sdl in
  let module U = Sdlcaml.Sdlut in
  match key.Key.synonym with
  | Key.SDLK_Q -> U.force_exit_game_loop ()
  | Key.SDLK_Z ->
    if has_shift key.Key.modify_state then
      let pos = M.get_trans !camera_trans in
      camera_trans := M.translation {pos with V.z = pos.V.z -. 0.01};
    else
      let pos = M.get_trans !camera_trans in
      camera_trans := M.translation {pos with V.z = pos.V.z +. 0.01};
  | Key.SDLK_X ->
    if has_shift key.Key.modify_state then
      let pos = M.get_trans !camera_trans in
      camera_trans := M.translation {pos with V.x = pos.V.x -. 0.01};
    else
      let pos = M.get_trans !camera_trans in
      camera_trans := M.translation {pos with V.x = pos.V.x +. 0.01};
  | Key.SDLK_C ->
    if has_shift key.Key.modify_state then
      let pos = M.get_trans !camera_trans in
      camera_trans := M.translation {pos with V.y = pos.V.y -. 0.01};
    else
      let pos = M.get_trans !camera_trans in
      camera_trans := M.translation {pos with V.y = pos.V.y +. 0.01};
  | _ -> ()
