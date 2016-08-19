# Turing Machine
A Turing machine implementation in OCaml.

> "... an infinite memory capacity obtained in the form of an infinite tape 
> marked out into squares on each of which a symbol could be printed. At any 
> moment there is one symbol in the machine; it is called the scanned symbol. 
> The machine can alter the scanned symbol and its behavior is in part 
> determined by that symbol, but the symbols on the tape elsewhere do not affect 
> the behavior of the machine. However, the tape can be moved back and forth 
> through the machine, this being one of the elementary operations of the 
> machine. Any symbol on the tape may therefore eventually have an innings."

![Alan Turing](https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Alan_Turing_signature.svg/200px-Alan_Turing_signature.svg.png)

## Modules
The `Turing` module provides the basic types and classes to manipulate a Turing machine in a object-oriented style.
Also, the `Turing_ext` module extend this support implementing classes to represent tapes and machines.

## Example
Here a [Busy Beaver](https://en.wikipedia.org/wiki/Busy_beaver), which is a Turing machine with two states and two symbols:
```ocaml
open Turing

(* Transition table *)

let r1 = {k = {current = St "A"; scanned = Blank}; v = {toprint = S '1'; move = Forth; next = St "B"}}
and r2 = {k = {current = St "A"; scanned = S '1'}; v = {toprint = S '1'; move = Back; next = St "B"}}
and r3 = {k = {current = St "B"; scanned = Blank}; v = {toprint = S '1'; move = Back; next = St "A"}}
and r4 = {k = {current = St "B"; scanned = S '1'}; v = {toprint = S '1'; move = Forth; next = Stop}}

let t = tt_fromArray [|r1; r2; r3; r4|]

(* Blank tape and initial status = "A" *)

let bb = new machine (new tape) t (St "A")

(* Run! *)

bb#run

(* Checking how many steps it took and the final status *)

bb#counter
bb#tape#state
```
