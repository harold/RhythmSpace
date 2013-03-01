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
        xml.WriteEndElement()//strip
    xml.WriteEndElement()//strips
    xml.WriteEndElement()//rhythmspace.settings
    let f = System.IO.File.CreateText("./settings.xml")
    f.Write( s.ToString() )
    f.Flush()
    f.Close()

let parseInt i = System.Int32.Parse(i)

let load osc =
    let xml = new System.Xml.XmlDocument()
    xml.Load("./settings.xml")
    let strips = xml.GetElementsByTagName("strip")
    Array.init (strips.Count) (fun i -> 
        let strip = strips.[i]
        let colorAttr = strip.Attributes.GetNamedItem("color")
        let trackAttr = strip.Attributes.GetNamedItem("track")
        let noteAttr = strip.Attributes.GetNamedItem("note")
        let instrumentAttr = strip.Attributes.GetNamedItem("instrument")
        let colorArray = colorAttr.Value.Split([|' '|])
        let c = System.Drawing.Color.FromArgb(parseInt colorArray.[3],parseInt colorArray.[0],parseInt colorArray.[1],parseInt colorArray.[2])
        new Strip(osc, c, parseInt trackAttr.Value, parseInt noteAttr.Value, parseInt instrumentAttr.Value))
