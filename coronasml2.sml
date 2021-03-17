(***************************************************************************
  Project     : Programming Languages 1 - Assignment 1 - Exercise 2
  Author(s)   : Eleni Elpida Kapsali (eleni_kaps@hotmail.com)
  Date        : May 3, 2020
  Description : Coronographs (SML code)
  -----------
  School of ECE, National Technical University of Athens.
*)
(* mergeSort function adopted from the course's slides*)
local
fun readInt input = 
     Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

fun connected visited V =
  if(V <= 0) then true
  else(
   if(not(Array.sub(visited, (V-1)))) then false
   else connected visited (V-1)
  ); 
  
fun mergeSort nil = nil
  | mergeSort [e] = [e] 
  | mergeSort theList =
     let
       fun halve nil = (nil, nil)
         | halve [a] = ([a], nil)
         | halve (a::b::cs) =
            let
              val (x, y) = halve cs
            in
              (a::x, b::y)
            end;

       fun merge (nil, ys) = ys
         | merge (xs, nil) = xs
         | merge (x::xs, y::ys) =
             if x < y then x :: merge(xs, y::ys)
                      else y :: merge(x::xs, ys);

        val (x, y) = halve theList
       in
        merge (mergeSort x, mergeSort y)
       end;

fun printList2 l = print(String.concatWith " " (map Int.toString l) ^ "\n"); 

(*The two functions below create a bool array, true for nodes in the cycle else false*)
fun cycle_arr_aux [] arr = ()
  | cycle_arr_aux (h::t) arr = 
    (
    Array.update(arr, h, true);
    cycle_arr_aux t arr
    );

fun cycle_arr V incycle = 
  let
    val arr = Array.array(V, false)
  in
    cycle_arr_aux incycle arr;
    arr 
  end;

fun add_edge_aux v1 v2 graph_array = 
  let 
    val list1 = Array.sub(graph_array, v1)
    val list2 = Array.sub(graph_array, v2)
  in Array.update(graph_array, v1, v2::list1);
     Array.update(graph_array, v2, v1::list2)
  end;

fun add_edge graph_arr E (inStream) = 
  if (E <= 0) then ()
  else(
     let
       val v1 = ((readInt inStream) - 1)
       val v2 = ((readInt inStream) - 1)
       val _ = TextIO.inputLine inStream
     in
       add_edge_aux v1 v2 graph_arr;
       add_edge graph_arr (E-1) (inStream)
     end
    );

fun pop [] = []
  | pop (h::t) = t;

fun push x l = x::l;

val equals : int * int -> bool = 
  fn (x, y) => (x = y);

fun the_cycle_aux incycle prev c y =
  let
    val p = (Array.sub(prev, c))
  in
   if(equals (p, y)) then ()
   else(
     incycle := push p (!incycle);
     the_cycle_aux incycle prev p y
   )
  end; 

fun the_cycle [] incycle prev = ()
  | the_cycle list incycle prev = 
    (
       let
         val h = List.hd(list)
         val x = List.last(list)
         val y = (Array.sub(prev, x))
         (*val p = (Array.sub(prev, h))*)
       in
        incycle := push y (!incycle);
        (*incycle := push p (!incycle);*)
        the_cycle_aux incycle prev h y
       end
     );


fun cnt_Nodes_for [] counted cycle stack cnt = ()
  | cnt_Nodes_for (v::t) counted cycle stack cnt = 
       (
       if((not (Array.sub(counted, v))) andalso (not (Array.sub(cycle, v)))) then(
        cnt := 1 + (!cnt);
        Array.update(counted, v, true);
        stack := push v (!stack);
        cnt_Nodes_for t counted cycle stack cnt
       )
       else cnt_Nodes_for t counted cycle stack cnt 
       );

fun cnt_Nodes graph_arr counted cycle stack [] cnt = ()
  | cnt_Nodes graph_arr counted cycle stack (s::t) cnt =  
   (  
     stack := pop (!stack);
     cnt_Nodes_for (Array.sub(graph_arr, s)) counted cycle stack cnt; 
     cnt_Nodes graph_arr counted cycle stack (!stack) cnt
   );  
   
fun cnt_Nodes2 graph_arr counted [] cycle cntNodes = (!cntNodes)
  | cnt_Nodes2 graph_arr counted (h::t) cycle cntNodes =
    ( 
    let 
      val stack = ref []
      val cnt = ref 1
    in
     Array.update(counted, h, true);
     stack := push h (!stack);
     cnt_Nodes graph_arr counted cycle stack (!stack) cnt;
     cntNodes := push (!cnt) (!cntNodes);
     cnt_Nodes2 graph_arr counted t cycle cntNodes
    end
     );

 
val cnt_Nodes1 : int * (int list array) * (int list ref) -> (int list) = 
  fn (V, graph_arr, incycle) =>
   (if ((!incycle) = []) then nil
    else( 
      let
      val cycle = (cycle_arr V (!incycle))
      val counted = Array.array(V, false)
      val cntNodes = ref []
    in
      cnt_Nodes2 graph_arr counted (!incycle) cycle cntNodes
    end
    )
   );

fun DFS_for [] visited s prev stack instack incycle = ()
  | DFS_for (v::t) visited s prev stack instack incycle = 
       (
       if(not (Array.sub(visited, v))) then(
        Array.update(prev, v, s);
        Array.update(visited, v, true);
        Array.update(instack, v, true);
        stack := push v (!stack);
        DFS_for t visited s prev stack instack incycle
       )
       else if ((Array.sub(instack, v))) then(
        incycle := push v (!incycle);
        incycle := push s (!incycle);
        DFS_for t visited s prev stack instack incycle
       )
       else DFS_for t visited s prev stack instack incycle
       );


fun DFS_aux visited graph_arr prev instack stack [] incycle = ()
  | DFS_aux visited graph_arr prev instack stack (s::t) incycle =  
   ( 
     Array.update(instack, s, false);
     stack := pop (!stack);
     DFS_for (Array.sub(graph_arr, s)) visited s prev stack instack incycle; 
     DFS_aux visited graph_arr prev instack stack (!stack) incycle
   );  

fun DFS V graph_arr =  
  let 
    val visited = Array.array(V,false)
    val instack = Array.array(V, false)
    val prev = Array.array(V, ~1)
    val stack = ref []
    val incycle = ref []
  in
    Array.update(visited, 0, true);
    Array.update(instack, 0, true);
    stack := push 0 (!stack);
    DFS_aux visited graph_arr prev instack stack (!stack) incycle;
    if(((!incycle) = nil) orelse (not (connected visited V))) then print ("NO CORONA" ^ "\n")
    else(
    print("CORONA ");
    the_cycle (!incycle) incycle prev;
    print((Int.toString (List.length(!incycle))) ^ "\n");
    let
      val tree_nodes = cnt_Nodes1(V, graph_arr, incycle)
    in
     printList2 (mergeSort tree_nodes)
    end
    )
  end;

fun Graph T (inStream) = 
  if(T <= 0) then ()
  else(
     let 
       val V = readInt (inStream)
       val E = readInt inStream
       val _ = TextIO.inputLine inStream
       val graph_arr = Array.array(V, nil)
     in
       add_edge graph_arr E (inStream);
       if(V <> E) then print("NO CORONA" ^ "\n")
       else DFS V graph_arr;
       Graph (T-1) (inStream)
     end
     );
in
 fun coronograph inFile = 
   let
     (* Open input file. *)
     val inStream = TextIO.openIn inFile
     val T = readInt (inStream)
     val _ = TextIO.inputLine (inStream)
   in
    Graph T (inStream)
   end
end