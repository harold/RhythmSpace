module Patterns

// Here's the first thing I thought of.
type intlist = System.Collections.Generic.List<int>
let patterns = ref Map.empty<int,intlist>
for i = 0 to 16 do patterns := Map.add i (new intlist()) !patterns

let intPow a b = (int (System.Math.Pow((float a),(float b))))

let powers = Array.init 16 (fun i -> intPow 2 (15-i))

for i = 0 to (intPow 2 16)-1 do
    let d = ref 0
    Array.iter (fun n -> if n&&&i>0 then d := !d+1) powers
    (!patterns).[!d].Add i

Map.iter (fun k (v:intlist) -> (v.Reverse())) !patterns

let byBeatStrength = [| // Endianness... ouch.
    System.Collections.BitArray([|0x01uy; 0x00uy|]); // 1000 0000 0000 0000 
    System.Collections.BitArray([|0x01uy; 0x01uy|]); // 1000 0000 1000 0000 
    System.Collections.BitArray([|0x01uy; 0x11uy|]); // 1000 0000 1000 1000 
    System.Collections.BitArray([|0x11uy; 0x11uy|]); // 1000 1000 1000 1000 

    System.Collections.BitArray([|0x15uy; 0x11uy|]); // 1010 1000 1000 1000 
    System.Collections.BitArray([|0x15uy; 0x15uy|]); // 1010 1000 1010 1000 
    System.Collections.BitArray([|0x15uy; 0x55uy|]); // 1010 1000 1010 1010 
    System.Collections.BitArray([|0x55uy; 0x55uy|]); // 1010 1010 1010 1010 

    System.Collections.BitArray([|0x5duy; 0x55uy|]); // 1011 1010 1010 1010 
    System.Collections.BitArray([|0x5duy; 0x5duy|]); // 1011 1010 1011 1010 
    System.Collections.BitArray([|0x5duy; 0xdduy|]); // 1011 1010 1011 1011 
    System.Collections.BitArray([|0xdduy; 0xdduy|]); // 1011 1011 1011 1011 

    System.Collections.BitArray([|0xdfuy; 0xdduy|]); // 1111 1011 1011 1011 
    System.Collections.BitArray([|0xdfuy; 0xdfuy|]); // 1111 1011 1111 1011 
    System.Collections.BitArray([|0xdfuy; 0xffuy|]); // 1111 1011 1111 1111 
    System.Collections.BitArray([|0xffuy; 0xffuy|]); // 1111 1111 1111 1111 
|]
