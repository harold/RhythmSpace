module Renoise

let template = """
local t = renoise.song().selected_pattern.tracks[{0}];
t:clear();
for k,v in pairs({1}) do
    local i = v;
    while i <= #t.lines do
        t.lines[i].note_columns[1].note_value={2};
        t.lines[i].note_columns[1].instrument_value={3};
        i = i + 16
    end
end
"""

let code trackNum linesTable midiNote instrumentNumber =
    System.String.Format( template, [| trackNum; linesTable; midiNote; instrumentNumber |] )
