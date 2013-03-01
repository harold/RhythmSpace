module Strip

type Strip(osc, color, trackNumber, note, instrument) as this =
    let dirty = new Event<_>()
    let color = ref (color:System.Drawing.Color)
    let data = ref (new System.Collections.BitArray(16))
    let beatStrength = ref 0
    do
        dirty.Publish.Add (fun () -> this.send())
    member this.onDirty = dirty.Publish
    member this.getColor() = !color
    member this.getTrack() = (trackNumber:int)
    member this.getNote() = (note:int)
    member this.getInstrument() = (instrument:int)
    member this.isOn(i) = (!data).[i]
    member this.set(i,v) = (!data).Set(i,v); dirty.Trigger()
    member this.setall( newdata ) = data := newdata; dirty.Trigger()
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
        (osc:OSC.UDPConnection).SendMessage (OSC.Str("/renoise/evaluate"), [OSC.Str(code)])
