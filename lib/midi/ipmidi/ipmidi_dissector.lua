
-- CLAUDE GENERATED, NOT VALIDATED


ipmidi_proto = Proto("ipMIDI", "ipMIDI Protocol")

-- Protocol fields
local f_command = ProtoField.uint8("ipmidi.command", "Command", base.HEX)
local f_channel = ProtoField.uint8("ipmidi.channel", "Channel", base.DEC)
local f_note = ProtoField.uint8("ipmidi.note", "Note", base.DEC)
local f_note_name = ProtoField.string("ipmidi.note_name", "Note Name")
local f_velocity = ProtoField.uint8("ipmidi.velocity", "Velocity", base.DEC)
local f_controller = ProtoField.uint8("ipmidi.controller", "Controller", base.DEC)
local f_controller_name = ProtoField.string("ipmidi.controller_name", "Controller Name")
local f_value = ProtoField.uint8("ipmidi.value", "Value", base.DEC)
local f_program = ProtoField.uint8("ipmidi.program", "Program", base.DEC)
local f_pressure = ProtoField.uint8("ipmidi.pressure", "Pressure", base.DEC)
local f_pitch_bend = ProtoField.uint16("ipmidi.pitch_bend", "Pitch Bend", base.DEC)
local f_sysex_data = ProtoField.bytes("ipmidi.sysex", "SysEx Data")
local f_timestamp = ProtoField.uint32("ipmidi.timestamp", "Timestamp", base.DEC)

ipmidi_proto.fields = {
    f_command, f_channel, f_note, f_note_name, f_velocity,
    f_controller, f_controller_name, f_value, f_program,
    f_pressure, f_pitch_bend, f_sysex_data, f_timestamp
}

-- MIDI note names
local note_names = {
    "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
}

local function get_note_name(note_num)
    local octave = math.floor(note_num / 12) - 1
    local note = note_names[(note_num % 12) + 1]
    return string.format("%s%d (%d)", note, octave, note_num)
end

-- Common MIDI controllers
local controller_names = {
    [0] = "Bank Select MSB",
    [1] = "Modulation Wheel",
    [2] = "Breath Controller",
    [4] = "Foot Controller",
    [5] = "Portamento Time",
    [6] = "Data Entry MSB",
    [7] = "Channel Volume",
    [8] = "Balance",
    [10] = "Pan",
    [11] = "Expression",
    [12] = "Effect Control 1",
    [13] = "Effect Control 2",
    [64] = "Sustain Pedal",
    [65] = "Portamento On/Off",
    [66] = "Sostenuto",
    [67] = "Soft Pedal",
    [68] = "Legato Footswitch",
    [69] = "Hold 2",
    [70] = "Sound Controller 1 (Sound Variation)",
    [71] = "Sound Controller 2 (Timbre/Harmonic Content)",
    [72] = "Sound Controller 3 (Release Time)",
    [73] = "Sound Controller 4 (Attack Time)",
    [74] = "Sound Controller 5 (Brightness)",
    [84] = "Portamento Control",
    [91] = "Effects 1 Depth (Reverb)",
    [92] = "Effects 2 Depth (Tremolo)",
    [93] = "Effects 3 Depth (Chorus)",
    [94] = "Effects 4 Depth (Detune)",
    [95] = "Effects 5 Depth (Phaser)",
    [120] = "All Sound Off",
    [121] = "Reset All Controllers",
    [122] = "Local Control",
    [123] = "All Notes Off",
    [124] = "Omni Mode Off",
    [125] = "Omni Mode On",
    [126] = "Mono Mode On",
    [127] = "Poly Mode On"
}

local function get_controller_name(cc)
    return controller_names[cc] or string.format("CC %d", cc)
end

-- Message type names
local message_types = {
    [0x8] = "Note Off",
    [0x9] = "Note On",
    [0xA] = "Poly Aftertouch",
    [0xB] = "Control Change",
    [0xC] = "Program Change",
    [0xD] = "Channel Aftertouch",
    [0xE] = "Pitch Bend",
    [0xF] = "System"
}

-- Parse a single MIDI message
local function parse_midi_message(buffer, offset, tree, running_status)
    if offset >= buffer:len() then
        return offset, running_status
    end
    
    local status_byte = buffer(offset, 1):uint()
    local is_status = (status_byte >= 0x80)
    
    local status, data_offset
    if is_status then
        status = status_byte
        data_offset = offset + 1
    else
        -- Running status
        if not running_status then
            return offset + 1, running_status
        end
        status = running_status
        data_offset = offset
    end
    
    local command = bit.rshift(status, 4)
    local channel = bit.band(status, 0x0F) + 1
    
    -- System messages
    if command == 0xF then
        if status == 0xF0 then
            -- SysEx start
            local sysex_end = data_offset
            while sysex_end < buffer:len() and buffer(sysex_end, 1):uint() ~= 0xF7 do
                sysex_end = sysex_end + 1
            end
            if sysex_end < buffer:len() then
                sysex_end = sysex_end + 1  -- Include 0xF7
            end
            
            local msg_len = sysex_end - offset
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len), "SysEx Message")
            msg_tree:add(f_sysex_data, buffer(offset, msg_len))
            
            return sysex_end, nil  -- SysEx breaks running status
            
        elseif status == 0xF1 then
            -- MIDI Time Code Quarter Frame
            if data_offset < buffer:len() then
                local msg_tree = tree:add(ipmidi_proto, buffer(offset, 2), "MTC Quarter Frame")
                msg_tree:add(f_value, buffer(data_offset, 1))
                return data_offset + 1, nil
            end
            
        elseif status == 0xF2 then
            -- Song Position Pointer
            if data_offset + 1 < buffer:len() then
                local msg_tree = tree:add(ipmidi_proto, buffer(offset, 3), "Song Position")
                local lsb = buffer(data_offset, 1):uint()
                local msb = buffer(data_offset + 1, 1):uint()
                local pos = lsb + (msb * 128)
                msg_tree:add(buffer(data_offset, 2), "Position: " .. pos)
                return data_offset + 2, nil
            end
            
        elseif status == 0xF3 then
            -- Song Select
            if data_offset < buffer:len() then
                local msg_tree = tree:add(ipmidi_proto, buffer(offset, 2), "Song Select")
                msg_tree:add(f_value, buffer(data_offset, 1))
                return data_offset + 1, nil
            end
            
        elseif status >= 0xF8 then
            -- Real-time messages (1 byte)
            local rt_names = {
                [0xF8] = "Timing Clock",
                [0xFA] = "Start",
                [0xFB] = "Continue",
                [0xFC] = "Stop",
                [0xFE] = "Active Sensing",
                [0xFF] = "System Reset"
            }
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, 1), 
                rt_names[status] or "Real-Time Message")
            return offset + 1, running_status  -- Real-time doesn't affect running status
        end
        
        return data_offset, nil
    end
    
    -- Channel messages
    local msg_type = message_types[command] or "Unknown"
    
    if command == 0x8 or command == 0x9 then
        -- Note Off / Note On
        if data_offset + 1 < buffer:len() then
            local note = buffer(data_offset, 1):uint()
            local velocity = buffer(data_offset + 1, 1):uint()
            
            -- Note On with velocity 0 is Note Off
            local actual_type = msg_type
            if command == 0x9 and velocity == 0 then
                actual_type = "Note Off (vel=0)"
            end
            
            local msg_len = is_status and 3 or 2
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len),
                string.format("%s: Ch %d, %s, Vel %d", actual_type, channel, 
                get_note_name(note), velocity))
            
            if is_status then
                msg_tree:add(f_command, buffer(offset, 1)):append_text(" (" .. msg_type .. ")")
                msg_tree:add(f_channel, buffer(offset, 1)):set_generated()
            end
            msg_tree:add(f_note, buffer(data_offset, 1))
            msg_tree:add(f_note_name, get_note_name(note)):set_generated()
            msg_tree:add(f_velocity, buffer(data_offset + 1, 1))
            
            return data_offset + 2, status
        end
        
    elseif command == 0xA then
        -- Polyphonic Aftertouch
        if data_offset + 1 < buffer:len() then
            local note = buffer(data_offset, 1):uint()
            local pressure = buffer(data_offset + 1, 1):uint()
            
            local msg_len = is_status and 3 or 2
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len),
                string.format("Poly Aftertouch: Ch %d, %s, Pressure %d", 
                channel, get_note_name(note), pressure))
            
            if is_status then
                msg_tree:add(f_command, buffer(offset, 1)):append_text(" (Poly Aftertouch)")
                msg_tree:add(f_channel, buffer(offset, 1)):set_generated()
            end
            msg_tree:add(f_note, buffer(data_offset, 1))
            msg_tree:add(f_pressure, buffer(data_offset + 1, 1))
            
            return data_offset + 2, status
        end
        
    elseif command == 0xB then
        -- Control Change
        if data_offset + 1 < buffer:len() then
            local controller = buffer(data_offset, 1):uint()
            local value = buffer(data_offset + 1, 1):uint()
            
            local msg_len = is_status and 3 or 2
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len),
                string.format("Control Change: Ch %d, %s = %d", 
                channel, get_controller_name(controller), value))
            
            if is_status then
                msg_tree:add(f_command, buffer(offset, 1)):append_text(" (Control Change)")
                msg_tree:add(f_channel, buffer(offset, 1)):set_generated()
            end
            msg_tree:add(f_controller, buffer(data_offset, 1))
            msg_tree:add(f_controller_name, get_controller_name(controller)):set_generated()
            msg_tree:add(f_value, buffer(data_offset + 1, 1))
            
            return data_offset + 2, status
        end
        
    elseif command == 0xC then
        -- Program Change
        if data_offset < buffer:len() then
            local program = buffer(data_offset, 1):uint()
            
            local msg_len = is_status and 2 or 1
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len),
                string.format("Program Change: Ch %d, Program %d", channel, program))
            
            if is_status then
                msg_tree:add(f_command, buffer(offset, 1)):append_text(" (Program Change)")
                msg_tree:add(f_channel, buffer(offset, 1)):set_generated()
            end
            msg_tree:add(f_program, buffer(data_offset, 1))
            
            return data_offset + 1, status
        end
        
    elseif command == 0xD then
        -- Channel Aftertouch
        if data_offset < buffer:len() then
            local pressure = buffer(data_offset, 1):uint()
            
            local msg_len = is_status and 2 or 1
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len),
                string.format("Channel Aftertouch: Ch %d, Pressure %d", channel, pressure))
            
            if is_status then
                msg_tree:add(f_command, buffer(offset, 1)):append_text(" (Channel Aftertouch)")
                msg_tree:add(f_channel, buffer(offset, 1)):set_generated()
            end
            msg_tree:add(f_pressure, buffer(data_offset, 1))
            
            return data_offset + 1, status
        end
        
    elseif command == 0xE then
        -- Pitch Bend
        if data_offset + 1 < buffer:len() then
            local lsb = buffer(data_offset, 1):uint()
            local msb = buffer(data_offset + 1, 1):uint()
            local bend = lsb + (msb * 128) - 8192  -- Center is 8192
            
            local msg_len = is_status and 3 or 2
            local msg_tree = tree:add(ipmidi_proto, buffer(offset, msg_len),
                string.format("Pitch Bend: Ch %d, Value %d", channel, bend))
            
            if is_status then
                msg_tree:add(f_command, buffer(offset, 1)):append_text(" (Pitch Bend)")
                msg_tree:add(f_channel, buffer(offset, 1)):set_generated()
            end
            msg_tree:add(f_pitch_bend, buffer(data_offset, 2)):append_text(" (" .. bend .. ")")
            
            return data_offset + 2, status
        end
    end
    
    return offset + 1, running_status
end

-- Main dissector function
function ipmidi_proto.dissector(buffer, pinfo, tree)
    local length = buffer:len()
    if length == 0 then return end
    
    pinfo.cols.protocol = "ipMIDI"
    
    local subtree = tree:add(ipmidi_proto, buffer(), "ipMIDI Protocol")
    
    local offset = 0
    local running_status = nil
    local msg_count = 0
    
    while offset < length do
        local old_offset = offset
        offset, running_status = parse_midi_message(buffer, offset, subtree, running_status)
        
        if offset == old_offset then
            -- Prevent infinite loop
            break
        end
        msg_count = msg_count + 1
    end
    
    pinfo.cols.info = string.format("ipMIDI (%d message%s)", msg_count, msg_count ~= 1 and "s" or "")
end

-- Register the dissector for ipMIDI ports
local udp_port = DissectorTable.get("udp.port")

-- ipMIDI uses 20 ports: 21928-21947
for port = 21928, 21947 do
    udp_port:add(port, ipmidi_proto)
end