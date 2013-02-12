let outputDevice = Seq.find (fun (device:Midi.OutputDevice) -> device.Name.Contains("LoopBe")) Midi.OutputDevice.InstalledDevices
outputDevice.Open()
let clock = new Midi.Clock(130.f)

let list = ref (new System.Collections.Generic.List<Midi.Message>())

let callbackHandler time =
    let t = time + 4.f
    clock.Schedule( !list, t )

let deligate = (new Midi.CallbackMessage.CallbackType(callbackHandler))

let callback = new Midi.CallbackMessage( deligate, 0.f )

let note pitch time =
    (!list).Add(new Midi.NoteOnMessage(outputDevice,Midi.Channel.Channel1,pitch,127,time))
    (!list).Add(new Midi.NoteOffMessage(outputDevice,Midi.Channel.Channel1,pitch,127,time+1.f))

list := (new System.Collections.Generic.List<Midi.Message>())
(!list).Add( callback )
note Midi.Pitch.C3 0.f
note Midi.Pitch.C3 1.f
note Midi.Pitch.C3 2.f
note Midi.Pitch.C3 3.f
note Midi.Pitch.D3 1.f
note Midi.Pitch.D3 3.f
note Midi.Pitch.E3 0.5f
note Midi.Pitch.E3 1.5f
note Midi.Pitch.E3 2.5f
note Midi.Pitch.E3 3.5f
clock.Schedule(!list,0.f)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    clock.Start()
    let quit = ref false
    while not !quit do
        let s = System.Console.ReadLine()
        if s="q" then quit := true
    clock.Stop()
    outputDevice.Close()
    0 // return an integer exit code
