open System.Windows.Forms
open System.Drawing

type MyForm() as self =
  inherit Form()
  do
    self.SetStyle(ControlStyles.DoubleBuffer ||| ControlStyles.UserPaint ||| ControlStyles.AllPaintingInWmPaint, true)
    self.UpdateStyles()
    self.FormBorderStyle <- FormBorderStyle.Fixed3D
    self.ClientSize <- new Size(32*16,32*4)

let f = new MyForm(Text="Rhythm Space")

type StripBinding = {
    up:string
    down:string
    numerator:int
    denominator:int
}

let stripbindings = [| 
    { up="B0"; down="A1";      numerator=14; denominator=2 };
    { up="C1"; down="ASharp1"; numerator=15; denominator=3 };
    { up="CSharp1"; down="B1"; numerator=16; denominator=4 };
    { up="D1"; down="C2";      numerator=17; denominator=5 };
|]

type Strip() =
    let numerator = ref 0
    let denominator = ref 1
    let number = ref 1
    let data = new System.Collections.BitArray(16)
    member this.isOn(i) = data.[i]
    member this.set(i,v) = data.Set(i,v)
    // TODO: These would be cleaner as properties.
    member this.getNumber() = !number
    member this.setNumber(i) = number := i; this.update()
    member this.getNumerator() = !numerator
    member this.setNumerator(i) = numerator := i; this.update()
    member this.getDenominator() = !denominator
    member this.setDenominator(i) = denominator := i; this.update()
    member this.update() =
        if !numerator > !denominator then numerator := !denominator
        let pattern = (!Patterns.patterns).[!number]
        let l = (float (pattern.Count-1))
        let p = (float !numerator)/(float !denominator)
        let index = int (round (p*l))
        for i = 0 to 15 do
            this.set( i, Patterns.powers.[i]&&&pattern.[index] > 0 )
        f.Invalidate()

let strips = Array.init 4 (fun i -> new Strip())
strips.[0].set(0, true)
strips.[0].set(10, true)

strips.[1].set(4, true)
strips.[1].set(12, true)

strips.[2].set(2, true)
strips.[2].set(6, true)
strips.[2].set(8, true)
strips.[2].set(14, true)

let outputDevice = Seq.find (fun (device:Midi.OutputDevice) -> device.Name.Contains("LoopBe")) Midi.OutputDevice.InstalledDevices
outputDevice.Open()

let inputDevice  = Seq.find (fun (device:Midi.InputDevice) -> device.Name.Contains("nanoKONTROL")) Midi.InputDevice.InstalledDevices
inputDevice.Open()
inputDevice.StartReceiving(null)
inputDevice.add_ControlChange( fun msg -> for i = 0 to 3 do
                                              let sb = stripbindings.[i]
                                              let s = strips.[i]
                                              if sb.denominator = (int msg.Control) then
                                                s.setDenominator(1+msg.Value/4)
                                              if sb.numerator = (int msg.Control) then
                                                s.setNumerator(int ((float32 msg.Value)/127.f * (float32 (s.getDenominator())))) )

inputDevice.add_NoteOn( fun msg -> for i = 0 to 3 do
                                       let sb = stripbindings.[i]
                                       let s = strips.[i]
                                       let id = (msg.Pitch.ToString())
                                       if sb.up = id then
                                           if s.getNumber() < 16 then s.setNumber(s.getNumber()+1)
                                       if sb.down = id then
                                           if s.getNumber() > 0 then s.setNumber(s.getNumber()-1)
                                   printfn "%A" (strips.[0].getNumber()) )

let clock = new Midi.Clock(130.f)

let list = ref (new System.Collections.Generic.List<Midi.Message>())

let note pitch time =
    (!list).Add(new Midi.NoteOnMessage(outputDevice,Midi.Channel.Channel1,pitch,127,time))
    (!list).Add(new Midi.NoteOffMessage(outputDevice,Midi.Channel.Channel1,pitch,127,time+1.f))

let callbackHandler time =
    let t = time + 4.f
    (!list).RemoveRange(1,(!list).Count-1)
    for y = 0 to 3 do
        let p = match y with
                | 0 -> Midi.Pitch.C3
                | 1 -> Midi.Pitch.D3
                | 2 -> Midi.Pitch.E3
                | 3 -> Midi.Pitch.F3
                | _ -> Midi.Pitch.C3
        for x = 0 to 15 do
            if strips.[y].isOn(x) then note p (0.25f * (float32 x))
    clock.Schedule( !list, t )

let deligate = (new Midi.CallbackMessage.CallbackType(callbackHandler))

let callback = new Midi.CallbackMessage( deligate, 0.f )

list := (new System.Collections.Generic.List<Midi.Message>())
(!list).Add( callback )
clock.Schedule(!list,0.f)

let rendersquare (g:Graphics) x y value =
    let rowcolor = match y with
                   | 0 -> Color.DarkCyan
                   | 1 -> Color.DarkMagenta
                   | 2 -> Color.DarkGoldenrod
                   | _ -> Color.White
    let c = if value then rowcolor else Color.FromArgb(32,32,32)
    g.FillRectangle(new SolidBrush(Color.FromArgb(48,48,48)),x*32,y*32,32,32)
    g.FillRectangle(new SolidBrush(c),x*32+1,y*32+1,32-1,32-1)

let redraw (args:PaintEventArgs) =
    let g = args.Graphics
    for y = 0 to 3 do
        for x = 0 to 15 do
            rendersquare g x y (strips.[y].isOn(x))

let click (args:MouseEventArgs) =
    let x = args.X/32
    let y = args.Y/32
    let s = strips.[y]
    s.set(x, not (s.isOn(x)))
    f.Invalidate()

f.Paint.Add(redraw)
f.MouseClick.Add(click)

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    clock.Start()
    Application.Run(f)
    clock.Stop()
    inputDevice.Close()
    outputDevice.Close()
    0 // return an integer exit code
