module V = Candyvec.Vector

let vector_to_vertices vertices =
  let array = Array.make ((List.length vertices) * 3) 0.0 in
  ignore (List.fold_left (fun index v ->
    array.(index + 0) <- v.V.x;
    array.(index + 1) <- v.V.y;
    array.(index + 2) <- v.V.z;
    index + 3
  ) 0 vertices);
  array

