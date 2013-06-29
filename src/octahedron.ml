module R = Realcamel
module V = Candyvec.Vector
module Q = Candyvec.Quaternion

open Sdlcaml
open Bigarray

let half_width :float = 0.01
let half_height:float = 0.02
let triangle_count = 8

let base_vertices =
  [ {V.x = 0.0; y = half_height; z = 0.0;};
    {V.x = -.half_width; y = 0.0; z = half_width;};
    {V.x = half_width; y = 0.0; z = half_width;};
    {V.x = half_width; y = 0.0; z = -.half_width;};
    {V.x = -.half_width; y = 0.0; z = -.half_width;};
    {V.x = 0.0; y = -.half_height; z = 0.0};
  ]

let base_faces =
  [ (0, 1, 2);
    (0, 2, 3);
    (0, 3, 4);
    (0, 4, 1);
    (5, 1, 2);
    (5, 2, 3);
    (5, 3, 4);
    (5, 4, 1);
  ] 

let make_collidable () =
  let make_shape () =
    {R.Shape.mesh = R.Mesh.convert ~vertices:(Array.of_list base_vertices) ~faces:(Array.of_list base_faces);
     offset_pos = V.zero;
     offset_orientation = Q.identity ();
    } in
  {R.Collidable.shapes = [| make_shape ()|]; center = V.zero;
   half_size = {V.x = half_width; y = half_height; z = half_width;}
  }

let make_rigidbody () =
  {R.RigidBody.empty with R.RigidBody.mass = 0.1; restitution = 0.2; friction = 0.3;}

let make_state () = {R.State.empty with R.State.motion_type = R.State.Active}

let make_rbi () =
  {R.RigidBodyInfo.body = make_rigidbody ();
   collidable = make_collidable ();
   state = make_state ();
  }

let make_vbo () =
  let vertex_data = Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
    (Util.vector_to_vertices base_vertices)
    
  and index_data = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
    (let array = Array.make ((List.length base_faces) * 3) 0 in
     ignore (List.fold_left (fun index (x, y, z) ->
       array.(index + 0) <- x;
       array.(index + 1) <- y;
       array.(index + 2) <- z;
       index + 3
     ) 0 base_faces);
     array)
  in
  
  let open Gl.VBO in
  let id = glGenBuffer ()
  and element_id = glGenBuffer () in
  glBindBuffer Buffer.GL_ARRAY_BUFFER id;
  glBufferData ~target:BufferData.GL_ARRAY_BUFFER ~size:(Bigarray.Array1.dim vertex_data)
    ~data:vertex_data ~usage:BufferData.GL_STATIC_DRAW;

  glBindBuffer Buffer.GL_ELEMENT_ARRAY_BUFFER element_id;
  glBufferData ~target:BufferData.GL_ELEMENT_ARRAY_BUFFER ~size:(Bigarray.Array1.dim index_data)
    ~data:index_data ~usage:BufferData.GL_STATIC_DRAW;
  (id, element_id)
;;

