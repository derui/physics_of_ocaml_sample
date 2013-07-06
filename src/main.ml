module R = Realcamel

open Sdlcaml
open Bigarray
open Sugarpot.Std.Prelude
module M = Candyvec.Matrix4
module V = Candyvec.Vector
module S = Sugarpot.Std
module U = Sdlut

let error _ =
  let open Gl.Api in
  match glGetError () with
  | Get.GL_NO_ERROR -> "NO_ERROR"
  | Get.GL_INVALID_ENUM -> "INVALID_ENUM"
  | Get.GL_INVALID_VALUE -> "INVALID_VALUE"
  | Get.GL_INVALID_OPERATION  -> "GL_INVALID_OPERATION"
  | Get.GL_STACK_OVERFLOW -> "STACK_OVERFLOW"
  | Get.GL_STACK_UNDERFLOW -> "STACK_UNDERFLOW"
  | Get.GL_OUT_OF_MEMORY -> "OUT_OF_MEMORY"
  | Get.GL_TABLE_TOO_LARGE -> "TABLE_TOO_LARGE"


let set_up _ =
  begin
    U.init ~auto_clean:true ~flags:[`VIDEO;`JOYSTICK];
    let open Sdlcaml.Sdl in
    ignore (Video.set_attribute ~attr:Video.GL_RED_SIZE ~value:8);
    ignore (Video.set_attribute ~attr:Video.GL_BLUE_SIZE ~value:8);
    ignore (Video.set_attribute ~attr:Video.GL_GREEN_SIZE ~value:8);
    ignore (Video.set_attribute ~attr:Video.GL_DEPTH_SIZE ~value:16);
    ignore (Video.set_attribute ~attr:Video.GL_DOUBLEBUFFER ~value:1);

    let surf = Video.set_video_mode ~width:640 ~height:480 ~depth:32
      ~flags:[Video.SDL_OPENGL;] in
    begin
      ignore (Video.get_attribute Video.GL_DOUBLEBUFFER);
      surf
    end
  end
;;

let tear_down surface =
  begin
    Sdl_video.free_surface surface;
  end
;;

let load_shaders () =
  let open Gl.Api in
  let open Gl.Util in
  let vertexShaderID = Shader.load "vertex_shader.vert" in
  let fragmentShaderID = Shader.load "fragment_shader.frag" in

  let open S.Either in
  match (vertexShaderID, fragmentShaderID) with
  | (Left(l), _) | (_, Left(l)) -> failwith l
  | (Right(vertexShaderID), Right(fragmentShaderID)) ->
    let shader_prog = glCreateProgram () in
    glAttachShader ~program:shader_prog ~shader:vertexShaderID;
    glAttachShader ~program:shader_prog ~shader:fragmentShaderID;

    glLinkProgram shader_prog;
    let vertexPosAttrib = glGetAttribLocation shader_prog "VertexPosition" in
    (shader_prog, vertexPosAttrib)
;;

let octahedron_position =
  let base = Array.make (5 * 5 * 5) V.zero in
  Array.mapi (fun i v ->
    let iy = (i / 5) mod 5 in
    let ix = i mod 5 in
    let iz = i / 25 in
    {V.x = (float_of_int ix) *. Octahedron.half_width *. 2.0;
     y = (float_of_int iy) *. Octahedron.half_height *. 2.0;
     z = (float_of_int iz) *. Octahedron.half_width *. 2.0}
  ) base
;;

let engine = ref (R.Engine.make ())
let engine_setup () =
  let orig = !engine in
  engine := Array.fold_left (fun engine v ->
    let body = Octahedron.make_rbi () in
    R.Engine.add_body engine (R.RigidBodyInfo.set_pos body v)
  ) orig octahedron_position
;;

let main_loop surface () =
  let open Gl.Api in
  let open Gl.VBO in
  begin
    engine := R.Engine.execute_pipeline !engine;
    List.iter (fun body ->
      let (x, y, z) = V.of_vec |< R.RigidBodyInfo.pos body in
      Printf.printf "body => x:%f y:%f z:%f\n" x y z
    ) ((S.Option.option_map id @< Array.to_list) |< R.Engine.bodies !engine);

    let vao = glGenVertexArray () in
    glBindVertexArray vao;
    let vbobj, element = Octahedron.make_vbo () in
    let background, grid = Background.make_vbo () in
    let sprog, pos = load_shaders () in
    let model, pers = (glGetUniformLocation sprog "modelView",
                       glGetUniformLocation sprog "projection") in
    let perspective = Gl.Util.Camera.make_perspective_matrix ~fov:90.0 ~ratio:(480.0 /. 640.0)
      ~near:0.001 ~far:1000.0
    and camera = KeyHandler.get_camera_mat () in
    (* Gl.Util.Camera.make_matrix ~pos:({V.x = 1.0; y = 0.0;z = 1.0}) *)
    (* ~at:V.zero ~up:(V.normal_axis `Y) in *)
    let perspective = M.multiply ~m1:perspective ~m2:camera in
    glViewport 0 0 640 480;
    glClear [Clear.GL_COLOR_BUFFER_BIT;Clear.GL_DEPTH_BUFFER_BIT];
    glClearColor ~red:1.0 ~green:1.0 ~blue:1.0 ~alpha:0.0;

    glUseProgram sprog;
    glEnableVertexAttribArray pos;
    glBindBuffer Buffer.GL_ARRAY_BUFFER vbobj;
    glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER element;

    glUniformMatrix ~location:pers ~transpose:false
      ~value:(Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout (M.to_array ~order:M.Column perspective));

    Array.iter (fun v ->
      let mat = M.translation v in
      begin

        glVertexAttribPointer ~index:pos ~size:3 ~vert_type:VertexArray.GL_FLOAT ~normalize:false
          ~stride:0;

        glUniformMatrix ~location:model ~transpose:false
          ~value:(Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout (M.to_array ~order:M.Column mat));

        glEnable Enable.GL_DEPTH_TEST;
        glDrawElements ~mode:DrawElements.GL_TRIANGLES ~elements_type:DrawElements.GL_UNSIGNED_SHORT
          ~size:24;
        glDisable Enable.GL_DEPTH_TEST;

      end
    ) octahedron_position;

    glUnbindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER;
    glUnbindBuffer Buffer.GL_ARRAY_BUFFER;

    Background.render grid background pos;
    glDeleteBuffers ~size:1 ~buffers:[vbobj];
    glDeleteBuffers ~size:2 ~buffers:[fst grid;fst background];
    glFlush ();

    Sdl_video.gl_swap ();
  end
;;

let _ =
  let surface = set_up () in
  begin
    engine_setup ();
    U.keyboard_callback ~func:KeyHandler.handler;
    U.display_callback ~func:(main_loop surface);
    U.game_loop ~fps:60 ();
    tear_down surface;
  end
;;
