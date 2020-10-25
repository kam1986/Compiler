module Decoding

// needed to filter out 0x00 bytes 
let inline GetBtyes str =
    Seq.foldBack 
        ( fun c bytes -> 
            let double = int16 c
            if int16 double > 255s
            then (byte (double >>> 8)) :: (byte double) :: bytes
            else (byte double) :: bytes
        ) str []