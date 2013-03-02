module Settings
open Strip

type UTF8StringWriter() =
    inherit System.IO.StringWriter()
    override this.Encoding = System.Text.Encoding.UTF8

let save (strips:Strip[]) =
    let s = new UTF8StringWriter()
    let xml = new System.Xml.XmlTextWriter(s)
    xml.Formatting <- System.Xml.Formatting.Indented
    xml.WriteStartDocument()
    xml.WriteStartElement("rhythmspace.settings")
    xml.WriteStartElement("strips")
    for s in strips do
        xml.WriteStartElement("strip")
        xml.WriteAttributeString("color", sprintf "%i %i %i %i" ((s.getColor()).R) ((s.getColor()).G) ((s.getColor()).B) ((s.getColor()).A))
        xml.WriteAttributeString("track", s.getTrack().ToString())
        xml.WriteAttributeString("note", s.getNote().ToString())
        xml.WriteAttributeString("instrument", s.getInstrument().ToString())
        let pattern = new System.Text.StringBuilder()
        for i=0 to 15 do
            pattern.Append(if s.isOn(i) then "x" else ".") |> ignore
        xml.WriteAttributeString("pattern", pattern.ToString())
        xml.WriteEndElement()//strip
    xml.WriteEndElement()//strips
    xml.WriteEndElement()//rhythmspace.settings
    let f = System.IO.File.CreateText("./settings.xml")
    f.Write( s.ToString() )
    f.Flush()
    f.Close()

let getAttr (node:System.Xml.XmlNode) name defaultValue =
    let attr = node.Attributes.GetNamedItem(name)
    if attr<>null then attr.Value else defaultValue

let parseInt i = System.Int32.Parse(i)

let load osc =
    let xml = new System.Xml.XmlDocument()
    xml.Load("./settings.xml")
    let strips = xml.GetElementsByTagName("strip")
    Array.init (strips.Count) (fun i -> 
        let strip = strips.[i]
        let color = getAttr strip "color" "255 255 255 255"
        let ca = color.Split([|' '|])
        let c = System.Drawing.Color.FromArgb(parseInt ca.[3],parseInt ca.[0],parseInt ca.[1],parseInt ca.[2])
        let track = getAttr strip "track" "1"
        let note = getAttr strip "note" "48"
        let instrument = getAttr strip "instrument" "0"
        let pattern = getAttr strip "pattern" "................"
        let p = new System.Collections.BitArray(16)
        for i=0 to 15 do
            if pattern.Chars i = 'x' then p.Set(i, true)
        let s = new Strip(osc, c, parseInt track, parseInt note, parseInt instrument)
        s.setall( p )
        s)
