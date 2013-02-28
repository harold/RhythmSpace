module Renoise

let template = """
local volume = true;
local volumes = {{0x80,0x30,0x60,0x40,0x70,0x30,0x40,0x50,0x80,0x30,0x60,0x40,0x70,0x30,0x50,0x40}};
local t = renoise.song().selected_pattern.tracks[{0}];
t:clear();
for k,v in pairs({1}) do
    local i = v;
    while i <= #t.lines do
        t.lines[i].note_columns[1].note_value={2};
        t.lines[i].note_columns[1].instrument_value={3};
        if volume then t.lines[i].note_columns[1].volume_value=volumes[v]; end
        --if v%4 ~= 1 then t.lines[i].note_columns[1].delay_value=0x20; end
        i = i + 16
    end
end
"""

let code trackNum linesTable midiNote instrumentNumber =
    System.String.Format( template, [| trackNum; linesTable; midiNote; instrumentNumber |] )
