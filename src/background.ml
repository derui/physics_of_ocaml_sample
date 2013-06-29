module R = Realcamel

open Sdlcaml
open Bigarray
module S = Sugarpot.Std
module V = Candyvec.Vector

let grid_interspace :float = 0.05
let grid_vertical_num = 100
let grid_horizontal_num = 100
let ground_width = (float_of_int grid_vertical_num) *. grid_interspace /. 2.0
let ground_height = (float_of_int grid_horizontal_num) *. grid_interspace /. 2.0
  
let grid_vertices =
  let array = Array.make ((grid_vertical_num + grid_horizontal_num) * 3 * 2) 0.0 in
  (* 最初の100頂点は縦、次の100頂点は横分 *)
  
  let make_vertical_line index =
    [ {V.x = ground_width -. (grid_interspace *. (float_of_int index));
       y = 0.0;
       z = ground_height;};
      {V.x = ground_width -. (grid_interspace *. (float_of_int index));
       y = 0.0;
       z = -.ground_height;}
    ]
  and make_horizontal_line index =
    [ {V.x = ground_width;
       y = 0.0;
       z = ground_height  -. (grid_interspace *. (float_of_int index));};
      {V.x = -.ground_width;
       y = 0.0;
       z = ground_height -. (grid_interspace *. (float_of_int index));}
    ] in
  let index_list = S.List.range_int (0, 99) in
  let open S.Prelude in
  let vertical_vertex_list = List.concat |< List.map make_vertical_line index_list
  and horizon_vertex_list = List.concat |< List.map make_horizontal_line index_list in
  let vertices_array = Array.of_list |< List.concat [vertical_vertex_list; horizon_vertex_list] in

  Array.iteri (fun index v ->
    let base = index * 3 in
    array.(base + 0) <- v.V.x;
    array.(base + 1) <- v.V.y;
    array.(base + 2) <- v.V.z;
  ) vertices_array;

  array
;;

let grid_indices =
  let open S.Prelude in
  (Array.of_list @< List.map succ) |< S.List.range_int (1, (grid_vertical_num + grid_horizontal_num) * 2)
;;

let gen_buffers vertices indices =
  let open Sdlcaml.Gl.VBO in
  let id = glGenBuffer ()
  and element_id = glGenBuffer () in
  glBindBuffer Buffer.GL_ARRAY_BUFFER id;
  glBufferData ~target:BufferData.GL_ARRAY_BUFFER ~size:(Bigarray.Array1.dim vertices)
    ~data:vertices ~usage:BufferData.GL_STATIC_DRAW;

  glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER element_id;
  glBufferData ~target:BufferData.GL_ELEMENT_ARRAY_BUFFER ~size:(Bigarray.Array1.dim indices)
    ~data:indices ~usage:BufferData.GL_STATIC_DRAW;
  (id, element_id)
;;

let make_vbo () =
  
  let ground_vertex = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    [| ground_width /. 2.0; -1.0; ground_height /. 2.0; 
       ground_width /. 2.0; -1.0; -. ground_height /. 2.0;
       -. ground_width /. 2.0; -1.0; -. ground_height /. 2.0;
       -. ground_width /. 2.0; -1.0; ground_height /. 2.0; 
     |]
  and ground_index = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
     [| 0; 1; 2;
        0; 2; 3;
     |] in

  let grid_vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    grid_vertices 
  and grid_index_data = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
    grid_indices in
  let open Gl.VBO in
  let ground_id = gen_buffers ground_vertex ground_index
  and grid_id = gen_buffers grid_vertex_data grid_index_data in

  (ground_id, grid_id)
;;

let render (grid_vert, grid_elem) (ground_vert, ground_elem) pos =
  let open Gl.Api in
  let open Gl.VBO in
  
  glBindBuffer Buffer.GL_ARRAY_BUFFER grid_vert;
  glVertexAttribPointer ~index:pos ~size:3 ~vert_type:VertexArray.GL_FLOAT ~normalize:false
    ~stride:0;
  glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER grid_elem;
  
  glEnable Enable.GL_DEPTH_TEST;
  glDrawElements ~mode:DrawElements.GL_LINES ~elements_type:DrawElements.GL_UNSIGNED_SHORT
    ~size:(grid_vertical_num + grid_horizontal_num * 2);
  glDisable Enable.GL_DEPTH_TEST;

  glUnbindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER;
  glUnbindBuffer Buffer.GL_ARRAY_BUFFER;

  glFlush ();

  glBindBuffer Buffer.GL_ARRAY_BUFFER ground_vert;
  glVertexAttribPointer ~index:pos ~size:3 ~vert_type:VertexArray.GL_FLOAT ~normalize:false
    ~stride:0;
  glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER ground_elem;

  glEnable Enable.GL_DEPTH_TEST;
  glDrawElements ~mode:DrawElements.GL_TRIANGLES ~elements_type:DrawElements.GL_UNSIGNED_SHORT
    ~size:6;
  glDisable Enable.GL_DEPTH_TEST;

  glUnbindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER;
  glUnbindBuffer Buffer.GL_ARRAY_BUFFER;
;;

