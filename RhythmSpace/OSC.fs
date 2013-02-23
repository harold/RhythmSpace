module OSC

type Data =
    | I32 of int32
    | Timetag of (int32 * int32)
    | F32 of float32
    | Str of string
    | Blob of byte []

type Message = (Data * Data list)

let stringToByteArray (s:string) = System.Text.Encoding.UTF8.GetBytes s

let s0 = "\u0000"
let stringToOSCString (s:string) =
    match s.Length%4 with
    | 0 -> s+s0+s0+s0+s0
    | 1 -> s+s0+s0+s0
    | 2 -> s+s0+s0
    | 3 -> s+s0
    | _ -> failwith("modular division is broken")

let maybeFlip bytes = if System.BitConverter.IsLittleEndian then Array.rev bytes else bytes
let getIntBytes (i:int32) = maybeFlip (System.BitConverter.GetBytes(i))
let getFloatBytes (f:float32) = maybeFlip (System.BitConverter.GetBytes(f))

let dataToByteArray (d:Data) =
    match d with
    | I32 i -> getIntBytes i
    | Timetag t -> Array.append (getIntBytes(fst t)) (getIntBytes(snd t))
    | F32 f -> getFloatBytes f
    | Str s -> stringToByteArray (stringToOSCString s)
    | Blob b -> b

let dataToTypeTag (d:Data) =
    match d with
    | I32 _ -> "i"
    | Timetag _ -> "t"
    | F32 _ -> "f"
    | Str _ -> "s"
    | Blob _ -> "b"

let messageToBytes m =
    let address = fst m
    let arguments = snd m
    let out = ref (match address with | Str s -> stringToByteArray (stringToOSCString s)
                                      | _ -> failwith("strange address"))
    let typeTag = System.Text.StringBuilder(",")
    List.iter (fun d -> typeTag.Append(dataToTypeTag d) |> ignore) arguments
    out := Array.append !out (stringToByteArray (stringToOSCString (typeTag.ToString())))
    List.iter (fun d -> out := Array.append !out (dataToByteArray d)) arguments
    !out

let stringToData s =
    let i = ref 0
    let f = ref 0.f
    if System.Int32.TryParse(s, i) then I32(!i)
    else if System.Single.TryParse(s, f) then F32(!f)
    else Str(s)

let stringToMessage (s:string) =
    let a = s.Split([|' '|])
    let arguments = List.init (a.Length-1) (fun i -> stringToData (a.[i+1]))
    (Str(a.[0]), arguments)

let repl() =
    let socket = new System.Net.Sockets.UdpClient()
    socket.Connect("localhost",54321)
    while true do
        let bytes = messageToBytes (stringToMessage (System.Console.ReadLine()))
        socket.Send( bytes, bytes.Length ) |> ignore
        //let reply = socket.Receive( ref (socket.Client.RemoteEndPoint :?> System.Net.IPEndPoint) )
        //printfn "%A" (System.Text.Encoding.UTF8.GetString( reply ))
