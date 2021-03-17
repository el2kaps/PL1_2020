(***************************************************************************
  Project     : Programming Languages 1 - Assignment 2 - Exercise 3
  Author(s)   : Eleni Elpida Kapsali (eleni_kaps@hotmail.com)
  Date        : May 31, 2020
  Description : Stayhome (SML code)
  -----------
  School of ECE, National Technical University of Athens.
*)
(*Function readInput() adopted from a code at shmmy.ntua.gr*)
local
(*Functions which read input and create the man of the world before the virus*)
fun create_margins 0 = nil
    | create_margins (m):char list = String.sub("X",0) :: create_margins(m-1);
  
  fun strip s = 
    let
     val s_size = String.size(s)
    in
     String.substring(s, 0, (s_size-1))
    end;

  fun readInput (inFile : string) = 
    let 
     val inStream = TextIO.openIn inFile
     fun loop inStream = 
       case TextIO.inputLine inStream of SOME line => String.explode("X" ^ (strip line) ^ "X") :: loop inStream | NONE => []
    in
     loop inStream before TextIO.closeIn inStream
    end;
(**********************************************************************)
(*Functions that print Int Array2 used in debugging*)
fun print_aux arr i j =
 if(i >= 0 andalso i < Array2.nRows(arr) andalso j >= 0 andalso j < Array2.nCols(arr)) then 
 (print(Int.toString(Array2.sub(arr,i,j)) ^ " ");
  print_aux arr i (j+1))
 else();

fun print_a arr i = 
  if(i >= 0 andalso i < Array2.nRows(arr)) then 
  (print_aux arr i 0;
   print("\n");
   print_a arr (i+1))
   else();

fun printArray arr =
  print_a arr 0;
(****************************************)
(*Fuction that find the neighbors of a cell*)
 fun isNeighbor world i j = 
  if (Array2.sub(world, i, j) <> String.sub("X", 0)) then ([[i,j]])
  else [];

fun Neighbors world i j = 
  let 
    val down = isNeighbor world (i+1) j
    val left = isNeighbor world i (j-1)
    val right = isNeighbor world i (j+1)
    val up = isNeighbor world (i-1) j
  in 
   [] @ down @ left @ right @ up
  end;

(*******************************************************************************)
(*The above 3 functions are used to find "W" cell*)

  fun loop_j arr i j M queue1 = 
  if(j > M orelse j < 1) then (queue1)
  else(
    if(Array2.sub(arr, i, j) = String.sub("W",0)) then (queue1 @ [i, j, 0])
    else (loop_j arr i (j+1) M queue1)
  );

  fun loop_i arr i M N queue1 = 
    if(i > N orelse i < 1) then (queue1)
    else(
      let 
       val x = loop_j arr i 1 M queue1
      in      
       loop_i arr (i+1) M N x
      end
    );

  fun find_W world N M = 
    let 
      val queue1 = [] in loop_i world 1 M N queue1 end;
(**************************************************************************************)
(*Function that prints a char 2D Array used for debugging*)
fun showMatrix arr =
    Array2.appi Array2.RowMajor (fn (_, col, c) =>
      print (str c ^ (if col + 1 = Array2.nCols arr then "\n" else "" )))
      {base=arr,row=0,col=0,nrows=NONE,ncols=NONE};
(**************************************************************************************)
(*First Flood-Fill, Covid Spread, a new int Array2 world2 saves the time Covid infects a cell*)
(*The above functions are used to infect the airports*)
fun loop_ja arr world2 i j M queue2 infect_airports = 
    if(j > M orelse j < 1) then ()
    else(
      if(Array2.sub(arr, i, j) = String.sub("A",0)) then 
      (Array2.update(world2, i, j, (!infect_airports));
       Array2.update(arr, i, j, String.sub("X", 0)); (*mark as visited*)
       queue2 := (!queue2)@[[i, j, (!infect_airports)]];
       loop_ja arr world2 i (j+1) M queue2 infect_airports
      )
      else (loop_ja arr world2 i (j+1) M queue2 infect_airports)
    );

  fun loop_ia arr world2 i M N queue2 infect_airports = 
    if(i > N orelse i < 1) then ()
    else( 
       loop_ja arr world2 i 1 M queue2 infect_airports;
       loop_ia arr world2 (i+1) M N queue2 infect_airports
    );

fun InfectAirports world world2 M N queue2 infect_airports = 
  loop_ia world world2 1 M N queue2 infect_airports;
(****************)
fun for_neigh [] world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2 = ()
  | for_neigh (h::t) world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2 =
    let
     val neigh_i = List.nth(h,0)
     val neigh_j = List.nth(h,1)
     val s = Array2.sub(world, neigh_i, neigh_j)
    in
     if(s = String.sub(".",0) orelse s = String.sub("A",0) orelse s = String.sub("S",0) orelse s = String.sub("T",0)) then
     (
      if (s = String.sub("S",0)) then
      (
        S_coor := [neigh_i, neigh_j]; 
        Array2.update(world, neigh_i, neigh_j, String.sub("X",0)); 
        Array2.update(world2, neigh_i, neigh_j, next_time);
        queue2 := (!queue2)@[[neigh_i,neigh_j, next_time]];
        for_neigh t world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2
      )
      else if (s = String.sub("T",0)) then 
      (
        T_coor := [neigh_i, neigh_j]; 
        Array2.update(world, neigh_i, neigh_j, String.sub("X",0));
        Array2.update(world2, neigh_i, neigh_j, next_time);
        queue2 := (!queue2)@[[neigh_i,neigh_j, next_time]];
        for_neigh t world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2  
      )
      else(
        if(s = String.sub("A",0) andalso (!covid_in_airport) = false) then
        (
          covid_in_airport := true; 
          infect_airports := (next_time + 5); 
          countdown := 2; 
          Array2.update(world, neigh_i, neigh_j, String.sub("X",0)); 
          Array2.update(world2, neigh_i, neigh_j, next_time);
          queue2 := (!queue2)@[[neigh_i,neigh_j, next_time]];
          for_neigh t world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2 
        )
        else
        (Array2.update(world, neigh_i, neigh_j, String.sub("X",0)); 
         Array2.update(world2, neigh_i, neigh_j, next_time);
         queue2 := (!queue2)@[[neigh_i,neigh_j, next_time]];
         for_neigh t world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2 
        )
     )
     )
     else ()
    end;

fun While2 [] world world2 covid_in_airport infect_airports countdown queue2 S_coor T_coor = ()
  | While2 (h::t) world world2 covid_in_airport infect_airports countdown queue2 S_coor T_coor =
     let 
      val curr = h
      val curr_i = List.nth(h,0)
      val curr_j = List.nth(h,1)
      val curr_t = List.nth(h,2)
      val next_time = curr_t + 2
      val neigh = Neighbors world curr_i curr_j
     in
      for_neigh neigh world world2 next_time S_coor T_coor covid_in_airport infect_airports countdown queue2;
      While2 t world world2 covid_in_airport infect_airports countdown queue2 S_coor T_coor
     end;

fun While1 (queue1 : (int list list ref), queue2 : (int list list ref), world : (char Array2.array), world2 : (int Array2.array), M : int, N : int, countdown : (int ref), covid_in_airport : (bool ref), infect_airports : (int ref), S_coor : (int list ref), T_coor : (int list ref)) =
    ( 
    if((!queue1) <> nil orelse (!countdown) >=0) then(
      queue2 := [];
      if((!covid_in_airport) = true andalso (!countdown) > 0) then 
      (
       countdown := (!countdown) - 1; 
       While2 (!queue1) world world2 covid_in_airport infect_airports countdown queue2 S_coor T_coor;
       queue1 := !queue2; 
       While1(queue1, queue2, world, world2, M, N, countdown, covid_in_airport, infect_airports, S_coor, T_coor)
      )
      else if((!covid_in_airport) = true andalso (!countdown) = 0) then 
      (
       countdown := ~1; 
       InfectAirports world world2 M N queue2 infect_airports; 
       While2 (!queue1) world world2 covid_in_airport infect_airports countdown queue2 S_coor T_coor;
       queue1 := !queue2;
       While1(queue1, queue2, world, world2, M, N, countdown, covid_in_airport, infect_airports, S_coor, T_coor)
      )
      else 
      (
       While2 (!queue1) world world2 covid_in_airport infect_airports countdown queue2 S_coor T_coor;
       queue1 := !queue2;
       While1(queue1, queue2, world, world2, M, N, countdown, covid_in_airport, infect_airports, S_coor, T_coor)
      )
    )
    else ()
    );

(*CovidSpread returns a Array2, the world2 after Covid*)
fun CovidSpread world M N q S_coor T_coor =
  let
   val covid_in_airport = ref false
   val infect_airports = ref 0
   val countdown = ref ~1
   val queue1 = ref [q]
   val queue2 = ref []
   val world2 = Array2.array((N+2), (M+2), ~1)
  in
   Array2.update(world, List.nth(q,0), List.nth(q,1), String.sub("X",0));
   Array2.update(world2, List.nth(q,0), List.nth(q,1), 0);
   While1(queue1, queue2, world, world2, M, N, countdown, covid_in_airport, infect_airports, S_coor, T_coor);
   world2
  end;
(****************************************************************************)
fun isNeighbor2 world2 i j = 
  if (Array2.sub(world2, i, j) <> ~1) then ([i,j])
  else [];

fun Neighbors2 world2 world3 i j = 
  let 
    val down = if(Array2.sub(world2,(i+1),j) <> ~1) then([[(i+1),j,~5]]) else([])
    val left = if(Array2.sub(world2,i,(j-1)) <> ~1) then([[i,(j-1),~4]]) else([])
    val right = if(Array2.sub(world2,i,(j+1)) <> ~1) then([[i,(j+1),~3]]) else([])
    val up = if(Array2.sub(world2,(i-1),j) <> ~1) then([[(i-1),j,~2]]) else([])
  in 
   [] @ down @ left @ right @ up
  end;

fun for_neigh2 [] world2 world3 counter stay T_coor queue2 curr_i curr_j = ()
  | for_neigh2 (h::t) world2 world3 counter stay T_coor queue2 curr_i curr_j  =
    (
    let
     val neigh_i = List.nth(h,0)
     val neigh_j = List.nth(h,1)
     val move = List.nth(h,2)
     val s = Array2.sub(world2, neigh_i, neigh_j)
    in
     if(s > (!counter)) then
     (
      if (neigh_i = List.nth((!T_coor),0) andalso neigh_j = List.nth((!T_coor),1)) then (
        stay := true;
        Array2.update(world2, neigh_i, neigh_j, ~1);
        Array2.update(world3, neigh_i, neigh_j, [curr_i,curr_j,move]); 
        queue2 := (!queue2)@[[neigh_i,neigh_j]];
        for_neigh2 t world2 world3 counter stay T_coor queue2 curr_i curr_j
      )
      else(
        Array2.update(world2, neigh_i, neigh_j, ~1);
        Array2.update(world3, neigh_i, neigh_j, [curr_i,curr_j,move]); 
        queue2 := (!queue2)@[[neigh_i,neigh_j]];
        for_neigh2 t world2 world3 counter stay T_coor queue2 curr_i curr_j
      )
     )
     else (
       for_neigh2 t world2 world3 counter stay T_coor queue2 curr_i curr_j
     )
    end
    );

fun While2_2 [] world2 world3 counter stay queue2 T_coor = ()
  | While2_2 (h::t) world2 world3 counter stay queue2 T_coor =
     let 
      val curr = h
      val curr_i = List.nth(h,0)
      val curr_j = List.nth(h,1)
      val neigh = Neighbors2 world2 world3 curr_i curr_j
     in
      for_neigh2 neigh world2 world3 counter stay T_coor queue2 curr_i curr_j;
      While2_2 t world2 world3 counter stay queue2 T_coor
     end;

fun While1_2 (queue1 : (int list list ref), queue2 : (int list list ref), world2 : (int Array2.array), world3 : (int list Array2.array), counter : (int ref), stay : (bool ref), T_coor : (int list ref)) =
    ( 
    if((!queue1) <> nil andalso (!stay) = false) then(
     counter := (!counter) + 1;
     queue2 := [];
     While2_2 (!queue1) world2 world3 counter stay queue2 T_coor;
     queue1 := (!queue2);
     While1_2 (queue1, queue2, world2, world3, counter, stay, T_coor)
    )
    else ()
    );

(********************************************)
(*ReturnHome returns Array2 world3, each cell contains a list, counter and stay*)
(*For each cell [prev_i,prev_j,move]*)
fun ReturnHome world2 N M S_coor T_coor = 
  let
    val queue1 = ref [!S_coor]
    val queue2 = ref []
    val i_S = List.nth(!S_coor, 0)
    val j_S = List.nth(!S_coor, 1)
    val counter = ref 0
    val stay = ref false
    val world3 = Array2.array((N+2), (M+2), [0])
  in
   Array2.update(world2, i_S, j_S, ~1);
   While1_2 (queue1, queue2, world2, world3, counter, stay, T_coor);
   (counter, world3, stay)
  end;
(***************************************)
fun FinalList [] res = res
  | FinalList (h::t) res = 
    (
     if(h = ~5) then (FinalList t res@["D"])
     else if(h = ~4) then (FinalList t res@["L"])
     else if(h = ~3) then (FinalList t res@["R"])
     else (FinalList t res@["U"])
    ); 

fun printList x = (print(String.concatWith "" (x)); print("\n"));   


fun WhileTrue (world3 : int list Array2.array, path : int list ref, i_S : int, j_S : int, i : int, j : int) = 
  if(i = i_S andalso j = j_S) then ()
  else(
    path := (!path)@ [(List.nth(Array2.sub(world3,i,j),2))];
    WhileTrue (world3, path, i_S, j_S, (List.nth(Array2.sub(world3,i,j),0)), (List.nth(Array2.sub(world3,i,j),1)))
  );

fun printPath world3 T_coor S_coor = 
  let
   val path = ref []
   val i_S = List.nth((!S_coor),0)
   val j_S = List.nth((!S_coor),1)
   val i = List.nth((!T_coor),0)
   val j = List.nth((!T_coor),1)
  in
   WhileTrue(world3, path, i_S, j_S, i, j);
   (*path := List.rev(!path);*)
   printList (FinalList (!path) [])
  end;

(*******************Solution*****************)
fun solve world N M = 
     let 
      val S_coor = ref []
      val T_coor = ref []
      val queue1 = find_W world N M
      val world2 = CovidSpread world M N queue1 S_coor T_coor
      val (counter, world3, stay) = ReturnHome world2 N M (S_coor) (T_coor)
     in  
      (*printArray world2*)  
      if((!stay) = false) then (print("IMPOSSIBLE" ^ "\n"))
      else(
       print(Int.toString((!counter)) ^ "\n");
       printPath world3 T_coor S_coor
      )
     end;

(**************************************************************************************)
(*******************Main*****************)
in
fun stayhome inFile = 
   let
     val input = readInput inFile
     val N = length input
     val M = ((length (hd input)) - 2)
     val margins = create_margins (M+2)
     val w_list = (margins :: input) @ [margins] 
     val world = Array2.fromList w_list
   in
    solve world N M
   end; 
end;
(**************************************************************************************)

