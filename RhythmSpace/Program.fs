open System.Windows.Forms
open System.Drawing
open Strip

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

let settingsExists = System.IO.File.Exists("./settings.xml")

let strips = if settingsExists then
                Settings.load( u )
             else
                [| new Strip(u, Color.DarkCyan,      2, 48, 0);
                   new Strip(u, Color.DarkMagenta,   4, 50, 0);
                   new Strip(u, Color.DarkGoldenrod, 5, 52, 0); |]

if not settingsExists then
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

let swap i delta =
    let a = strips.[i]
    let b = strips.[(i+strips.Length+delta)%strips.Length]
    let atemp = new System.Collections.BitArray(16)
    let btemp = new System.Collections.BitArray(16)
    for i=0 to 15 do atemp.Set( i, a.isOn(i) )
    for i=0 to 15 do btemp.Set( i, b.isOn(i) )
    a.setall( btemp )
    b.setall( atemp )

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
        g.DrawString(sprintf "Track: %i" (strip.getTrack()),new Font("Segoe UI",9.f),new SolidBrush(Color.White),0.f,0.f)
        g.DrawString(sprintf "Note: %i"  (strip.getNote()),new Font("Segoe UI",9.f),new SolidBrush(Color.White),0.f,15.f)
        g.DrawString(sprintf "Inst: %i"  (strip.getInstrument()),new Font("Segoe UI",9.f),new SolidBrush(Color.White),1.f,30.f)
    override this.ProcessCmdKey( msg, keys:Keys ) =
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
        | Keys.Up -> strip.perturb; true
        | Keys.Down -> true
        | Keys.Left -> strip.translate 1; true
        | Keys.Right -> strip.translate -1; true
        | Keys.Add -> strip.incBeatStrength(); true
        | Keys.Subtract -> strip.decBeatStrength(); true
        | _ -> false
    override this.OnKeyDown( args:KeyEventArgs ) =
        if args.Control then
            match args.KeyCode with
            | Keys.Up   -> swap (Seq.findIndex (fun s -> s=strip) strips) -1; args.Handled <- true
            | Keys.Down -> swap (Seq.findIndex (fun s -> s=strip) strips)  1; args.Handled <- true
            | _ -> ()

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
    Settings.save strips
    Application.Run(f)
