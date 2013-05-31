module R = Realcamel

open Sdlcaml
open Bigarray

let half_width :float = 0.02
let half_height:float = 0.02
let triangle_count = 8

let make_vbo () =
  let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [| 0.0; half_height; 0.0;
       -.half_width; 0.0; half_width;
       half_width; 0.0; half_width;
       half_width; 0.0; -.half_width;
       -.half_width; 0.0; -.half_width;
       0.0; -.half_height; 0.0;
    |]
  and index_data = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
     [| 0; 1; 2;
        0; 2; 3;
        0; 3; 4;
        0; 4; 1;
        5; 1; 2;
        5; 2; 3;
        5; 3; 4;
        5; 4; 1;
     |] in
  
  let open Gl.VBO in
  let id = glGenBuffer ()
  and element_id = glGenBuffer () in
  glBindBuffer Buffer.GL_ARRAY_BUFFER id;
  glBufferData ~target:BufferData.GL_ARRAY_BUFFER ~size:((Bigarray.Array1.dim vertex_data) * 4)
    ~data:vertex_data ~usage:BufferData.GL_STATIC_DRAW;

  glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER element_id;
  glBufferData ~target:BufferData.GL_ELEMENT_ARRAY_BUFFER ~size:((Bigarray.Array1.dim index_data) * 2)
    ~data:index_data ~usage:BufferData.GL_STATIC_DRAW;
  (id, element_id)
;;

