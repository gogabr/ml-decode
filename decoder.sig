signature DECODER =
sig
    type config

    val defaultConfig: config

    val decode: (config * AcousticModel.t * Fst.fst * string vector) 
                -> Mfc.mfc list 
                -> string list (* a single variant, split by words *)
end
