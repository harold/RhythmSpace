open System.Windows.Forms
open System.Drawing

type Strip() =
    let numerator = 0
    let denominator = 1
    let data = new System.Collections.BitArray(16)
    member this.isOn(i) = data.[i]
    member this.set(i,v) = data.Set(i,v)

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

type MyForm() as self =
  inherit Form()
  do
    self.SetStyle(ControlStyles.DoubleBuffer ||| ControlStyles.UserPaint ||| ControlStyles.AllPaintingInWmPaint, true)
    self.UpdateStyles()
    self.FormBorderStyle <- FormBorderStyle.Fixed3D
    self.ClientSize <- new Size(32*16,32*4)

let f = new MyForm(Text="Rhythm Space")

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
    outputDevice.Close()
    0 // return an integer exit code
