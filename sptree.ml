(** Handling data structures **)

(* Trees are represented using (sorted) lists of neighbors, where the
   labels of the nodes follow the cyclic order of the convex hull. *)


(* insertion in sorted list *)
let rec insert t=function
  | a::q when t>a -> a::insert t q
  | a::q when t=a -> q
  | l -> t::l

(* deletion in sorted list *)
let rec delete t = function
    []->[]
   |a::q when t>a -> a::delete t q
   |a::q when t=a -> q
   |l -> l

(* get index in array *)
exception Find of int
let find v x =
  try
    for i = 0 to Array.length v-1 do
      if v.(i) = x then raise (Find i)
    done;
    raise Not_found
  with Find i -> i

(* edge deletion *)
let remove t (u,v) =
  let t' = Array.copy t in
  t'.(u) <- delete v t'.(u);
  t'.(v) <- delete u t'.(v);
  t'

(* edge insertion *)
let add t (u,v) =
  let t' = Array.copy t in
  t'.(u) <- insert v t'.(u);
  t'.(v) <- insert u t'.(v);
  t'

(* [x1...xn] -> [(x1,x2)...(xn-1,xn),(xn,x1)] *)
let rec combine_aux a = function
  |t::t2::q -> (t,t2)::combine_aux a (t2::q)
  |[t] -> [(t,a)]
  |[] -> []

let combine = function
    [] -> []
  | [t] -> []
  | t::q -> combine_aux t (t::q)

(* converts tree into integer *)
let toint t =
  Array.fold_left (fun x y -> (x lsl 8) +  List.fold_left (fun a b -> a+(1 lsl b)) 0  y) 0 t 

(** Generating all trees **)

(** A slide of the edge uv along an edge vw is the flip uv -> vw. It
   is known that any non-crossing spanning trees can be transformed in
   any other one using only slides. So to generate all non-crossing
   spanning trees, we start from a border path and apply slides until
   we do not find anything more. **)

(* tests whether a tree is non-crossing *)
exception Break 
let noncross t =
  try
  for u = 0 to 7 do
    for v = u+1 to 7 do
      for w = v+1 to 7 do
        for x = w+1 to 7 do
          if List.mem w t.(u) && List.mem x t.(v) then raise Break
        done;
      done;
    done;
  done;
  true
with Break -> false

(* generates all non-crossing trees obtained by sliding an edge in t *)
let neighbors t =
  let res = ref [] in
  for u = 0 to Array.length t-1 do
    if List.length t.(u) >= 2 then 
      List.iter (fun (a,b) ->
          let t' = remove (add t (a,b)) (u,a) in
          let t'' = remove (add t (a,b)) (u,b) in
          res := t'::t'':: !res
        ) (combine t.(u))
  done;
  List.filter noncross !res


(* applies neighbords until we do not discover new trees *)
let explore t =
  let s = Stack.create () in
  let seen = Hashtbl.create 1000 in
  let res=ref [] in
  Stack.push t s;
  try
    while true do
      let t' = Stack.pop s in 
      if not (Hashtbl.mem seen (toint t')) then begin
          res:= t' :: !res;
          Hashtbl.add seen (toint t') true;
          List.iter (fun x-> Stack.push x s) (neighbors t')
        end
    done;
    []
  with Stack.Empty -> !res 

(* generates all non-crossing trees *)
let trees = explore [|[1];[0;2];[1;3];[2;4];[3;5];[4;6];[5;7];[6]|] |> Array.of_list


(** Generating the reconfiguration graph **)
(* testing whether t can be transformed into t' in one rotation *)
let is_adj t t' =
  let v = List.find_all (fun x-> List.length t.(x) > List.length t'.(x)) [0;1;2;3;4;5;6;7] in
  let w = List.find_all (fun x-> List.length t.(x) < List.length t'.(x)) [0;1;2;3;4;5;6;7] in
  let u = List.find_all (fun x-> t.(x) <> t'.(x) && List.length t.(x) = List.length t'.(x)) [0;1;2;3;4;5;6;7] in
  if List.length u = 1 && List.length v = 1 && List.length w = 1 then
    let u' = List.hd u and v'=List.hd v and w' = List.hd w in
    t'.(w') = insert u' t.(w') &&   t'.(v') = delete u' t.(v') && t'.(u') = insert w' (delete v' t.(u'))
  else false

(* initializing and filling the adjacency matrix *)
let m_adj = Array.make_matrix (Array.length trees) (Array.length trees) false

let _ =
  for i = 0 to Array.length trees -1 do
    for j = i+1 to Array.length trees -1 do
      if is_adj trees.(i) trees.(j) then begin
          m_adj.(i).(j) <- true;
          m_adj.(j).(i) <- true;
        end
    done;
  done

(* uses bfs to get all distances to a given tree t *)
let bfs m t =
  let s = Queue.create () in
  let dist = Array.make (Array.length m) (-1) in
  dist.(t) <- 0;
  Queue.push t s;
  try
    while true do
      let t' = Queue.pop s in 
      for i = 0 to Array.length m -1 do
        if dist.(i) = -1 && m.(t').(i) then begin
            dist.(i) <- dist.(t')+1;
            Queue.push i s
          end
      done;
    done;
    [||]
  with Queue.Empty -> dist


(** finds the distance between T_1 and T'_1 **)

let _ =
  let dist = bfs m_adj (find trees [|[1;3];[0];[3];[0;2;7];[5;7];[4];[7];[3;4;6]|]) in
  Printf.printf "The distance is %d.\n%!" dist.(find trees [|[1];[0;6];[3;5;6];[2];[5];[2;4];[1;2;7];[6]|])
