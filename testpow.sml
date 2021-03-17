(***************************************************************************
  Project     : Programming Languages 1 - Assignment 1 - Exercise 1
  Author(s)   : Eleni Elpida Kapsali (eleni_kaps@hotmail.com)
  Date        : April 24, 2020
  Description : Powers2
  -----------
  School of ECE, National Technical University of Athens.
*)

local
 fun log2_for_2powers x = 
    if (x > 1) then 1 + log2_for_2powers(x div 2)
    else 0;

 fun forloop pow (sumpow:Int64.int) idx N = 
   if idx >= 0 then (
    let 
      val powidx = Array.sub(pow, idx)
    in
     let   
       val (a:Int64.int) = sumpow + (Int64.fromInt powidx)
     in
      if a <= (Int64.fromInt N) then(
        Array.update(pow, idx, 2*powidx);
        forloop pow a idx N
        )
       else forloop pow sumpow (idx-1) N
     end
    end 
   )  
   else (
    if ((sumpow:Int64.int) = (Int64.fromInt N)) then true
    else false

   );

 fun pow2_update pow2 pow K idx =
    if idx >= K then ()
    else (
       let
        val pow_idx = log2_for_2powers (Array.sub(pow, idx))
       in
        Array.update(pow2, pow_idx, (Array.sub(pow2, pow_idx)+1));
        pow2_update pow2 pow K (idx+1)
       end
       );

 fun arrayToList arr = Array.foldr (op ::) [] arr;

 fun solve N K =  
    let 
      val pow = Array.array(K, 1)
    in 
     if((forloop pow (Int64.fromInt K) (K-1) N) = false) then []
     else(
       let
          val size = log2_for_2powers (Array.sub(pow, K-1)) + 1
       in
         let
           val pow2 = Array.array(size, 0)
         in 
           pow2_update pow2 pow K 0;
           arrayToList pow2
         end
       end
     )  
    end;
 
 (* A function to read an integer from specified input. *)
 fun readInt input = 
     Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

 fun printList l = print("[" ^ String.concatWith "," (map Int.toString l) ^ "]" ^ "\n"); 

     
 fun powers2_for T (inStream) = 
     if (T <= 0) then ()
     else(
        let 
          val N = readInt inStream
          val K = readInt inStream
          val _ = TextIO.inputLine inStream
        in
         printList (solve N K);
         powers2_for (T-1) (inStream)
        end
     );

in
  fun powers2 inFile = 
      let
       (* Open input file. *)
        val inStream = TextIO.openIn inFile
        val T = readInt (inStream)
        val _ = TextIO.inputLine (inStream)
      in
       powers2_for T (inStream)
      end
end
      

