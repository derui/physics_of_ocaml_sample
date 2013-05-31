module R = Realcamel

open Sdlcaml
open Bigarray
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

let vertex_shader_src = "
#version 130
in vec3 VertexPosition;
uniform mat4 modelView;
uniform mat4 projection;
uniform mat4 camera;
out vec3 diffuseColor;
const vec3 light = normalize(vec3(10.0, 6.0, 3.0));
const vec3 lightColor = vec3(1.0, 1.0, 0.0);
const vec3 diffuseMaterial = vec3(0.0, 1.0, 1.0);
void main(void) {
    vec3 v = normalize(VertexPosition) * 0.5 + 0.5;
    diffuseColor = vec3(dot(v, light))* lightColor * diffuseMaterial;
    gl_Position = modelView * vec4(VertexPosition, 1.0);
    gl_Position = camera * gl_Position;
    gl_Position = projection * gl_Position;
}"
;;

let fragment_shader_src = "
#version 130
out vec4 Color;
in vec3 diffuseColor;
void main() {
  Color = vec4(diffuseColor, 1.0);
}"

let load_shaders () =
  let open Gl.Api in
  let vertexShaderID = ShaderUtil.compile Shader.GL_VERTEX_SHADER vertex_shader_src in
  let fragmentShaderID = ShaderUtil.compile Shader.GL_FRAGMENT_SHADER fragment_shader_src in

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
  let base = Array.make (10 * 10 * 10) V.zero in
  Array.mapi (fun i v ->
    let iy = (i / 10) mod 10 in
    let ix = i mod 10 in
    let iz = i / 100 in
    {V.x = (float_of_int ix) *. Octahedron.half_width *. 1.0;
      y = (float_of_int iy) *. Octahedron.half_height *. 1.0;
      z = (float_of_int iz) *. Octahedron.half_width *. 1.0}
  ) base
;;

let main_loop surface () =
  let open Gl.Api in
  let open Gl.VBO in
  begin
    let vao = glGenVertexArray () in
    glBindVertexArray vao;
    let vbobj, element = Octahedron.make_vbo () in
    let sprog, pos = load_shaders () in
    let model, pers, cam = (glGetUniformLocation sprog "modelView",
                            glGetUniformLocation sprog "projection",
                            glGetUniformLocation sprog "camera") in
    let perspective = Gl.Util.perspective_projection ~fov:90.0 ~ratio:(480.0 /. 640.0)
      ~near:0.01 ~far:1000.0
    and camera = Gl.Util.Camera.make_matrix ~pos:({V.x = 0.0; y = 0.0;z = 2.0})
      ~at:V.zero ~up:(V.normal_axis `Y) in

    print_string (M.to_string camera);
    glViewport 0 0 640 480;
    glClear [Clear.GL_COLOR_BUFFER_BIT;Clear.GL_DEPTH_BUFFER_BIT];
    glClearColor ~red:1.0 ~green:1.0 ~blue:1.0 ~alpha:0.0;

    glUseProgram sprog;

    Array.iter (fun v ->
      let mat = M.translation v in
      begin
        glEnableVertexAttribArray pos;
        glBindBuffer Buffer.GL_ARRAY_BUFFER vbobj;
        glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER element;
        glVertexAttribPointer ~index:pos ~size:3 ~vert_type:VertexArray.GL_FLOAT ~normalize:false
          ~stride:0;
        
        glUniformMatrix ~location:model ~transpose:false
          ~value:(Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout (M.to_array ~order:M.Column mat));
        glUniformMatrix ~location:pers ~transpose:false
          ~value:(Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout (M.to_array ~order:M.Column perspective));
        glUniformMatrix ~location:cam ~transpose:false
          ~value:(Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout (M.to_array ~order:M.Column camera));

        glEnable Enable.GL_DEPTH_TEST;
        glDrawElements ~mode:DrawElements.GL_TRIANGLES ~elements_type:DrawElements.GL_UNSIGNED_SHORT
          ~size:24;
        glDisable Enable.GL_DEPTH_TEST;
      end
    ) octahedron_position;
    glDeleteBuffers ~size:1 ~buffers:[vbobj];
    glFlush ();

    Sdl_video.gl_swap ();
  end
;;

let _ =
  let surface = set_up () in
  begin
    U.keyboard_callback ~func:(fun ~key ~state ->
      let open Sdlcaml.Sdl in
      match key.Key.synonym with
      | Key.SDLK_Q -> U.force_exit_game_loop ()
      | _ -> ()
    );
    U.display_callback ~func:(main_loop surface);
    U.game_loop ~fps:60 ();
    tear_down surface;
  end
;;

