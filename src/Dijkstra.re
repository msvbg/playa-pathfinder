let list_vertices graph =>
  List.fold_left
    (
      fun acc ((a, b), _) => {
        let acc =
          if (List.mem b acc) {
            acc
          } else {
            [b, ...acc]
          };
        let acc =
          if (List.mem a acc) {
            acc
          } else {
            [a, ...acc]
          };
        acc
      }
    )
    []
    graph;

let neighbors v =>
  List.fold_left
    (
      fun acc ((a, b), d) =>
        if (a == v) {
          [(b, d), ...acc]
        } else {
          acc
        }
    )
    [];

let remove_from v lst => {
  let rec aux acc =>
    fun
    | [] => failwith "remove_from"
    | [x, ...xs] =>
      if (x == v) {
        List.rev_append acc xs
      } else {
        aux [x, ...acc] xs
      };
  aux [] lst
};

let with_smallest_distance q dist =>
  switch q {
  | [] => assert false
  | [x, ...xs] =>
    let rec aux distance v => (
      fun
      | [x, ...xs] => {
          let d = Hashtbl.find dist x;
          if (d < distance) {
            aux d x xs
          } else {
            aux distance v xs
          }
        }
      | [] => (v, distance)
    );
    aux (Hashtbl.find dist x) x xs
  };

let heuristic source target => 1;

let dijkstra max_val zero add graph source target => {
  let vertices = list_vertices graph;
  let dist_between u v =>
    try (List.assoc (u, v) graph) {
    | _ => zero
    };
  let dist = Hashtbl.create 1;
  let previous = Hashtbl.create 1;
  List.iter
    (
      fun v => {
        /* initializations */
        Hashtbl.add dist v max_val /* unknown distance function from source to v */
      }
    )
    vertices;
  Hashtbl.replace dist source zero; /* distance from source to source */
  let rec loop =
    fun
    | [] => ()
    | q => {
        let (u, dist_u) =
          with_smallest_distance q dist; /* vertex in q with smallest distance in dist */
        if (dist_u == max_val) {
          failwith "vertices inaccessible"
        }; /* all remaining vertices are inaccessible from source */
        if (u == target) {
          ()
        } else {
          let q = remove_from u q;
          List.iter
            (
              fun (v, d) =>
                if (List.mem v q) {
                  let alt = add dist_u (dist_between u v);
                  let dist_v = Hashtbl.find dist v;
                  if (alt < dist_v)
                    {
                      /* relax (u,v,a) */
                      Hashtbl.replace dist v alt;
                      Hashtbl.replace previous v u
                    } /* previous node in optimal path from source */
                }
            )
            (neighbors u graph);
          loop q
        }
      };
  loop vertices;
  let s = ref [];
  let u = ref target;
  while (Hashtbl.mem previous !u) {
    s := [!u, ...!s];
    u := Hashtbl.find previous !u
  };
  [source, ...!s]
};
