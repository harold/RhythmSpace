module Patterns

// Here's the first thing I thought of.
type intlist = System.Collections.Generic.List<int>
type patternmap = System.Collections.Generic.Dictionary<int,intlist>
let patterns = ref Map.empty<int,intlist>
for i = 0 to 16 do patterns := Map.add i (new intlist()) !patterns

let intPow a b = (int (System.Math.Pow((float a),(float b))))

let powers = Array.init 16 (fun i -> intPow 2 i)

for i = 0 to (intPow 2 16)-1 do
    let d = ref 0
    Array.iter (fun n -> if n&&&i>0 then d := !d+1) powers
    (!patterns).[!d].Add i
