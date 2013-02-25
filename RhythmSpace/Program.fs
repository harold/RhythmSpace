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
    self.Icon <- new Icon("./headphones.ico")

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
    member this.getTrack() = trackNumber
    member this.getNote() = note
    member this.getInstrument() = instrument
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
                new Strip(Color.DarkGoldenrod, "5", "52", "0"); |]

strips.[0].set(0, true)
strips.[0].set(10, true)

strips.[1].set(4, true)
strips.[1].set(12, true)

strips.[2].set(2, true)
strips.[2].set(6, true)
strips.[2].set(8, true)
strips.[2].set(14, true)

let boxSize = 48
type SixteenGridControl( strip:Strip ) as this =
    inherit Control(Size=new Size(boxSize*16,boxSize), Margin=Padding.Empty)
    do
        this.TabStop <- false
        this.DoubleBuffered <- true
        strip.onDirty.Add (fun () -> this.Invalidate())
    override this.OnPaint(args:PaintEventArgs) =
        let g = args.Graphics
        for i = 0 to 15 do
            let color = if (strip.isOn(i)) then (strip.getColor()) else Color.FromArgb(32,32,32)
            g.FillRectangle(new SolidBrush(Color.FromArgb(48,48,48)),i*boxSize,0,boxSize,boxSize)
            g.FillRectangle(new SolidBrush(color),i*boxSize+1,1,boxSize-1,boxSize-1)
    override this.OnMouseDown(args:MouseEventArgs) =
        let i = (args.X/boxSize)
        strip.set( i, not (strip.isOn(i)) )

type StripDataControl( strip:Strip ) as this = 
    inherit Control(Size=new Size(100,boxSize), Margin=Padding.Empty)
    do
        this.DoubleBuffered <- true
        strip.onDirty.Add (fun () -> this.Invalidate())
        this.GotFocus.Add (fun e -> this.Invalidate())
        this.LostFocus.Add (fun e -> this.Invalidate())
    override this.OnPaint(args:PaintEventArgs) =
        let g = args.Graphics
        let borderColor = if this.Focused then Color.FromArgb(192,192,84) else Color.FromArgb(48,48,48)
        g.FillRectangle(new SolidBrush(borderColor),0,0,100,boxSize)
        g.FillRectangle(new SolidBrush(Color.FromArgb(24,24,24)),1,1,98,boxSize-2)
        g.DrawString(sprintf "Track: %i" (System.Int32.Parse(strip.getTrack())),new Font("Segoe UI",9.f),new SolidBrush(Color.White),0.f,0.f)
        g.DrawString(sprintf "Note: %i" (System.Int32.Parse(strip.getNote())),new Font("Segoe UI",9.f),new SolidBrush(Color.White),0.f,15.f)
        g.DrawString(sprintf "Inst: %i" (System.Int32.Parse(strip.getInstrument())),new Font("Segoe UI",9.f),new SolidBrush(Color.White),1.f,30.f)
    override this.ProcessCmdKey( msg, keys:Keys )=
        match keys with
        | Keys.D1 -> strip.setNumber 1; true
        | Keys.D2 -> strip.setNumber 2; true
        | Keys.D3 -> strip.setNumber 3; true
        | Keys.D4 -> strip.setNumber 4; true
        | Keys.D5 -> strip.setNumber 5; true
        | Keys.D6 -> strip.setNumber 6; true
        | Keys.D7 -> strip.setNumber 7; true
        | Keys.D8 -> strip.setNumber 8; true
        | Keys.D0 -> strip.setNumber 0; true
        | Keys.Left -> strip.translate 1; true
        | Keys.Right -> strip.translate -1; true
        | Keys.Up -> strip.perturb; true
        | Keys.Down -> true
        | Keys.Add -> strip.incBeatStrength(); true
        | Keys.Subtract -> strip.decBeatStrength(); true
        | _ -> false

let table = new TableLayoutPanel(Margin=Padding.Empty, AutoSize=true)
let addStripToTable i strip =
    let stripData = new StripDataControl( strip )
    table.Controls.Add (stripData)
    table.SetRow(stripData, i)
    table.SetColumn(stripData, 0)
    let sixteenGrid = new SixteenGridControl( strip )
    table.Controls.Add (sixteenGrid)
    table.SetRow(sixteenGrid, i)
    table.SetColumn(sixteenGrid, 1)

Seq.iteri (fun i strip -> addStripToTable i strip ) strips
f.Controls.Add table

[<System.STAThread>]
do
    Application.Run(f)
