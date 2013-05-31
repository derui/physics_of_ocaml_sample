
open Sugarpot.Std

let compile shader_type source =
  let open Sdlcaml.Gl.Api in
  let id = glCreateShader shader_type in
  match glGetError () with
  | Get.GL_NO_ERROR ->
    glShaderSource id source;
    glCompileShader id;

    if glGetShader_bool ~shader:id ~pname:GetShader.GL_COMPILE_STATUS then
      Either.Right(id)
    else
      let info = glGetShaderInfoLog id in
      glDeleteShader id;
      Either.Left(info)
  | _ -> Either.Left("Some error occurs")


let info_log id = Sdlcaml.Gl.Api.glGetShaderInfoLog id

let source id = Sdlcaml.Gl.Api.glGetShaderSource id

let delete id = Sdlcaml.Gl.Api.glDeleteShader id
