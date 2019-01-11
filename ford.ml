open Graph


let ford_fulkerson gr_residual source puits =

  (*changer le graphe residual en un int graph *)
  let gr_residual= map gr_residual (fun x -> int_of_string x ) in 


  
  (*creation et initialisation des flots à 0 *)
  let init_zero gr = map gr (fun x -> 0 ) in
  let  gr = init_zero gr_residual in 

 
  
  (* fonction trouver un chemin de id1 à puits avec le cout min du chemin *)
  let rec find_path acu coutmin gr id1 =
    if id1=puits
    then  ((id1::acu),coutmin)
      else 
        let out = out_arcs gr id1 in
        let rec test_puits out = match out with
          | [] -> ([],coutmin)
          | (a,b)::rest -> if (List.exists (fun x -> if x=a then true else false) acu) then test_puits rest else if (b<=0) then test_puits rest else match find_path (id1::acu) (if b<coutmin then b else coutmin) gr a with
            |[],_ -> test_puits rest
            | aux -> aux 
        in
        test_puits out  
    in 
   let rec affiche_chemin = function
    | [] -> Printf.printf(" fin ") 
    | a::rest -> Printf.printf("%s  ::  ") a ; affiche_chemin rest
  in

   
  (* Remplir le graphe residual avec les chemins de find_path *)
  let rec residual gr path = match path with  
      | ([],_) -> gr
      | (chemin,cout) ->
        let rec loop graph = function
          |[] -> graph
          |a::b::rest -> begin  match find_arc graph b a with
              | None ->  loop (add_arc  graph b a cout) (b::rest)
              | Some x -> loop (add_arc graph b a (x+cout)) (b::rest)
                            end
          |a::rest -> graph
        in 
        let rec loop2 graph = function
          |[] -> graph 
          |a::b::rest -> begin match find_arc graph a b with
            | Some x -> loop2 (add_arc graph a b (x-cout)) (b::rest)
            | None -> assert false
              end
          |a::rest -> graph
        in
        let gr=loop gr chemin in
        let gr=loop2 gr chemin in
        let (chemin2,cout2)=find_path [] max_int gr source in
        affiche_chemin(List.rev(chemin2));
        Printf.printf("\n");
        residual gr (List.rev(chemin2),cout2)
        (* residual (loop2 (loop gr chemin) chemin) (find_path [] max_int gr source) *)
  in
  let  (chemin,cout)=find_path [] max_int gr_residual source in


  affiche_chemin (List.rev chemin);
  Printf.printf("\n");

  
  let new_gr_residual = residual gr_residual (List.rev(chemin),cout) in

  
  (* Remplir le graphe à retourner grâce au graphe residual *)
  let  list_node = v_fold gr_residual (fun acu id _ -> id::acu ) [] in

  
  let remplir_gr gr gr_residual =
   (* let list_node= list_id gr_residual in*)
     let list_node= v_fold gr_residual (fun acu id _ -> id::acu ) [] in
    let rec loop3 id1 gr = function
      | [] -> gr 
      | (id2,cost)::rest -> match find_arc gr_residual id2 id1 with
        | None -> loop3 id1 gr rest
        | Some x -> loop3 id1 (add_arc gr id1 id2 x) rest 
    in
    let rec loop4 gr = function
      |[] -> gr
      |id::rest -> loop4 (loop3 id gr (out_arcs gr_residual id)) rest
    in
    loop4 gr list_node
  in

  
  let transforme_to_string_graph gr = map gr (fun x-> string_of_int x)  in
   transforme_to_string_graph (remplir_gr gr new_gr_residual) 

     


