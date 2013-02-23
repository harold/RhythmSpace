open System.Windows.Forms
open System.Drawing

type MyForm() as self =
  inherit Form(KeyPreview=true)
  do
    self.SetStyle(ControlStyles.DoubleBuffer ||| ControlStyles.UserPaint ||| ControlStyles.AllPaintingInWmPaint, true)
    self.UpdateStyles()
    self.FormBorderStyle <- FormBorderStyle.Fixed3D
    self.AutoSizeMode <- AutoSizeMode.GrowAndShrink
    self.AutoSize <- true

let f = new MyForm(Text="Rhythm Space")
let u = OSC.UDPConnection(8000)

type Strip(color, trackNumber, note, instrument) as this =
    let dirty = new Event<_>()
    let color = ref color
    let data = ref (new System.Collections.BitArray(16))
    let beatStrength = ref 0
    do
        dirty.Publish.Add (fun () -> this.send())
    member this.onDirty = dirty.Publish
    member this.getColor() = !color
    member this.isOn(i) = (!data).[i]
    member this.set(i,v) = (!data).Set(i,v); dirty.Trigger()
    member this.setNumber(n) =
        let i = ref 0
        let added = ref 0
        (!data).SetAll(false)
        if n>0 then
            while !i < 16 do
                this.set(!i,true)
                added := !added+1
                if !added = n then i:=16 else i := (!i+(int (floor (16.f/(float32 n)))))
        dirty.Trigger()
    member this.translate n =
        let newArray = new System.Collections.BitArray(16)
        for i = 0 to 15 do
            newArray.Set( i, (!data).Get(((i+16)+n)%16) )
        data := newArray
        dirty.Trigger()
    member this.perturb =
        let r = new System.Random()
        let indices = new Patterns.intlist()
        for i = 0 to 15 do if this.isOn(i) then indices.Add(i)
        let l = indices.Count
        let stillTrying = ref true
        while !stillTrying && indices.Count>0 do
            let index = r.Next(indices.Count)
            let v = indices.[index]
            indices.RemoveAt(index)
            let left  = ((v+16)-1)%16
            let right = ((v+16)+1)%16
            if 0 = r.Next(2) then
                if not (this.isOn(left)) then
                    this.set(v, false)
                    this.set(left, true)
                    stillTrying := false
            else if not (this.isOn(right)) then
                this.set(v, false)
                this.set(right, true)
                stillTrying := false
    member this.incBeatStrength() =
        if !beatStrength < 15 then
            beatStrength := !beatStrength + 1
            data := new System.Collections.BitArray( (Patterns.byBeatStrength.[!beatStrength]) )
            dirty.Trigger()
    member this.decBeatStrength() =
        if !beatStrength > 0 then
            beatStrength := !beatStrength - 1
            data := new System.Collections.BitArray( (Patterns.byBeatStrength.[!beatStrength]) )
            dirty.Trigger()
    member this.send() =
        let linesTable = System.Text.StringBuilder("{")
        for i = 0 to 15 do 
            if this.isOn(i) then
                linesTable.Append((i+1).ToString()+",") |> ignore
        linesTable.Append("}") |> ignore
        let code = Renoise.code trackNumber (linesTable.ToString()) note instrument
        u.SendMessage (OSC.Str("/renoise/evaluate"), [OSC.Str(code)])
        

let strips = [| new Strip(Color.DarkCyan,      "2", "48", "0");
                new Strip(Color.DarkMagenta,   "4", "50", "0");
                new Strip(Color.DarkGoldenrod, "5", "52", "0");
                new Strip(Color.White,         "6", "53", "0"); |]

strips.[0].set(0, true)
strips.[0].set(10, true)

strips.[1].set(4, true)
strips.[1].set(12, true)

strips.[2].set(2, true)
strips.[2].set(6, true)
strips.[2].set(8, true)
strips.[2].set(14, true)

type SixteenGridControl( strip:Strip ) as this =
    inherit Control(Size=new Size(32*16,32), Margin=Padding.Empty)
    do
        this.DoubleBuffered <- true
        strip.onDirty.Add (fun () -> this.Invalidate())
        this.TabStop <- false
    override this.OnPaint(args:PaintEventArgs) =
        let g = args.Graphics
        for i = 0 to 15 do
            let color = if (strip.isOn(i)) then (strip.getColor()) else Color.FromArgb(32,32,32)
            g.FillRectangle(new SolidBrush(Color.FromArgb(48,48,48)),i*32,0,32,32)
            g.FillRectangle(new SolidBrush(color),i*32+1,1,32-1,32-1)

type StripDataControl( strip:Strip ) as this = 
    inherit Control(Size=new Size(100,32), Margin=Padding.Empty)
    do
        this.DoubleBuffered <- true
        strip.onDirty.Add (fun () -> this.Invalidate())
        this.GotFocus.Add (fun e -> this.Invalidate())
        this.LostFocus.Add (fun e -> this.Invalidate())
    override this.OnPaint(args:PaintEventArgs) =
        let g = args.Graphics
        let borderColor = if this.Focused then Color.FromArgb(192,192,84) else Color.FromArgb(48,48,48)
        g.FillRectangle(new SolidBrush(borderColor),0,0,100,32)
        g.FillRectangle(new SolidBrush(Color.FromArgb(24,24,24)),1,1,98,30)
        g.DrawString(sprintf "OHAI",new Font("Segoe UI",9.f),new SolidBrush(Color.White),0.f,0.f)
    override this.ProcessCmdKey( msg, keys:Keys )=
        match keys with
        | Keys.Left -> strip.translate 1; true
        | Keys.Right -> strip.translate -1; true
        | Keys.Up -> strip.perturb; true
        | Keys.Down -> true
        | Keys.Add -> strip.incBeatStrength(); true
        | Keys.Subtract -> strip.decBeatStrength(); true
        | _ -> false
    member this.setNumber n = strip.setNumber n

type StripFlow( strip:Strip ) as this =
    inherit FlowLayoutPanel(AutoSize=true, Margin=Padding.Empty)
    let data = new StripDataControl( strip )
    let grid = new SixteenGridControl( strip )
    do
        this.Controls.Add data
        this.Controls.Add grid

let flow = new FlowLayoutPanel(FlowDirection=FlowDirection.TopDown, Margin=Padding.Empty, AutoSize=true)

flow.Controls.Add( new StripFlow(strips.[0]) )
flow.Controls.Add( new StripFlow(strips.[1]) )
flow.Controls.Add( new StripFlow(strips.[2]) )
flow.Controls.Add( new StripFlow(strips.[3]) )
f.Controls.Add flow

let setFocusedStripNumber n =
    let control = f.ActiveControl :?> StripDataControl
    control.setNumber n

let key (args:KeyEventArgs) =
    if args.KeyCode = Keys.D1 then setFocusedStripNumber(1)
    if args.KeyCode = Keys.D2 then setFocusedStripNumber(2)
    if args.KeyCode = Keys.D3 then setFocusedStripNumber(3)
    if args.KeyCode = Keys.D4 then setFocusedStripNumber(4)
    if args.KeyCode = Keys.D5 then setFocusedStripNumber(5)
    if args.KeyCode = Keys.D6 then setFocusedStripNumber(6)
    if args.KeyCode = Keys.D7 then setFocusedStripNumber(7)
    if args.KeyCode = Keys.D8 then setFocusedStripNumber(8)
    if args.KeyCode = Keys.D0 then setFocusedStripNumber(0)

f.KeyDown.Add(key)

[<System.STAThread>]
do
    Application.Run(f)

