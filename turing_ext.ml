(*
 * David Méndez
 * hello@davidemdot.com
 *)

(*
"... an infinite memory capacity obtained in the form of an infinite tape 
marked out into squares on each of which a symbol could be printed. At any 
moment there is one symbol in the machine; it is called the scanned symbol. 
The machine can alter the scanned symbol and its behavior is in part 
determined by that symbol, but the symbols on the tape elsewhere do not affect 
the behavior of the machine. However, the tape can be moved back and forth 
through the machine, this being one of the elementary operations of the 
machine. Any symbol on the tape may therefore eventually have an innings."

                                                           (Alan Turing, 1948)
*)

open List
open Turing

class tape =
  object (self)
    inherit Turing.tape as super

    method string_toHead b =
      super#gc;
      let (p, _, _) = super#state in
      let st = String.create (length p) in
      iteri (fun i c -> st.[i] <- match c with Blank -> b | S s -> s) p;
      st

    method string_fromHead b =
      super#gc;
      let (_, c, n) = super#state in
      let st = String.create (length n + 1) in
      iteri (fun i c -> st.[i] <- match c with Blank -> b | S s -> s) (c :: n);
      st

    method erase =
      let (p, _, n) = super#state in
      let l_p = length p in
      for i = 1 to l_p do super#move Back done;
      for i = 1 to l_p + 1 + length n do
        super#print Blank; super#move Forth
      done;
      super#gc

    method print_string s =
      String.iter (fun c -> super#print (S c); super#move Forth) s;
      for i = 1 to String.length s do super#move Back done

    method copy_from (t : tape) =
      self#erase;
      let (p, c, n) = t#state in
      iter (fun c -> super#move Back; super#print c) p;
      for i = 1 to length p do super#move Forth done;
      super#print c;
      iter (fun c -> super#move Forth; super#print c) n;
      for i = 1 to length n do super#move Back done
  end


exception TM_stop

class machine (t : tape) (tt : Turing.trans_table) (s0 : Turing.state) =
  object (self)
    inherit Turing.machine (t :> Turing.tape) tt s0 as super

    method tape_ext = t
    method limited_run i =
      if super#counter = i then ()
      else try super#step; self#limited_run i with TM_stop -> ()
  end
