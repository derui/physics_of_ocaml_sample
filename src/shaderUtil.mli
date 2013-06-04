(**
   Shader module is wrapped operations for shader.
*)


(** type of Shader  *)

(** Create and compile shader, then return shader id if operations are all right.

    @param shader_type Specified shader type to create and compile
    @param source source to compile shader
    @return shader id if operations all right, otherwise return error string
*)
val compile : Sdlcaml.Gl.Api.Shader.shader_type -> string ->
  (Sdlcaml.Gl.Api.shader, string) Sugarpot.Std.Either.t

(** Get infomation log of given shader  *)
val info_log : Sdlcaml.Gl.Api.shader -> string

(** Get source of given shader  *)
val source : Sdlcaml.Gl.Api.shader -> string

(** delete shader that is created  *)
val delete : Sdlcaml.Gl.Api.shader -> unit
