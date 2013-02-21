module OSC

let toOSCString (s:string) =
        let modFour = s.Length%4
        System.Text.Encoding.UTF8.GetBytes( match modFour with
                                            | 0 -> s+"\u0000\u0000\u0000\u0000"
                                            | 1 -> s+"\u0000\u0000\u0000"
                                            | 2 -> s+"\u0000\u0000"
                                            | 3 -> s+"\u0000"
                                            | _ -> failwith("modular division is broken") )

let repl() =
    let socket = new System.Net.Sockets.UdpClient()
    socket.Connect("localhost",54321)
    while true do
        let bytes = toOSCString( System.Console.ReadLine() )
        socket.Send( bytes, bytes.Length ) |> ignore
        let x = socket.ReceiveAsync()
        printfn "%A" (System.Text.Encoding.UTF8.GetString( x.Result.Buffer ))
