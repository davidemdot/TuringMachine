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

type symbol =
  | Blank
  | S of char

type movement = Back | Forth | Stay

class tape =
  object
    val mutable prev = []      (* previous symbols are kept in reverse order *)
    val mutable next = [Blank] (* the current symbol is at the head *)

    method move = function
      | Back -> if prev = [] then next <- Blank :: next
                else begin next <- hd prev :: next; prev <- tl prev end
      | Forth -> prev <- hd next :: prev;
                 if tl next = [] then next <- [Blank]
                 else next <- tl next
      | Stay -> ()
    method print s = next <- s :: tl next
    method scanned = hd next
    method state = rev prev, hd next, tl next
    method gc =
      let rec aux = function
        | [] -> []
        | Blank :: t -> aux t
        | l -> rev l in
      prev <- aux (rev prev);
      next <- hd next :: aux (rev (tl next))
  end


type state =
  | St of string
  | Stop
  | Halt of string

type rule_key = { current : state; scanned : symbol }
type rule_val = { toprint : symbol; move : movement; next : state }
type trans_rule = { k : rule_key; v : rule_val }

type trans_table = < rule: rule_key -> rule_val option >

let tt_fromArray (ar : trans_rule array) : trans_table =
  object
    val tt =
      let table = Hashtbl.create (Array.length ar) in
      Array.iter (fun r -> Hashtbl.add table r.k r.v) ar;
      table

    method rule k = try Some (Hashtbl.find tt k) with Not_found -> None
  end


exception TM_stop

class machine (t : tape) (tt : trans_table) (s0 : state) =
  object (self)
    val mutable state = s0    
    val mutable num_steps = 0

    method counter = num_steps
    method run = try self#step; self#run with TM_stop -> ()
    method state = state
    method step =
      if state = Stop || state = Halt "No rule" then raise TM_stop
      else match tt#rule { current = state; scanned = t#scanned } with
        | None -> state <- Halt "No rule"
        | Some v -> num_steps <- num_steps + 1;
                    t#print v.toprint;
                    t#move v.move;
                    state <- v.next
    method tape = t
    method reset = num_steps <- 0; state <- s0
  end
