
local
    val argv = CommandLine.arguments ()

    val _ = if length argv <> 5
            then
                ( print "Usage: decode config acoustic-model fst wordlist mfc\n"
                ; OS.Process.exit OS.Process.failure)
            else ()
in  
    val isCfg = TextIO.openIn (List.nth (argv, 0))
    val isAm = BinIO.openIn (List.nth (argv, 1))
    val isFst = BinIO.openIn (List.nth (argv, 2))
    val isWl = TextIO.openIn (List.nth (argv, 3))
    val isMfc = BinIO.openIn (List.nth (argv, 4))
end;

print "Opened\n";

let
    fun readWordList is =
        let
            fun collectLines is acc =
                case TextIO.inputLine is of
                    NONE => acc
                  | SOME l => 
                    collectLines is ((hd (String.tokens Char.isSpace l)) :: acc)
        in
            Vector.fromList (rev (collectLines is []))
        end

    val cfg = Decoder.readConfig isCfg;
    val am = AcousticModel.read isAm
    val fst = Fst.readFst isFst
    val wl = readWordList isWl
    val mfcs = (Mfc.subCmn o Mfc.read) isMfc
in
    print (Fst.headerToString (Fst.header fst) ^ "\n");
    print ("nframes: " ^ Int.toString (length mfcs) ^ "\n");
    print ("nwords: " ^ Int.toString (Vector.length wl) ^ "\n");

    let
        val timerReal = Timer.startRealTimer ()
    in
        app (fn w => print (w ^ " ")) 
            (Decoder.decode (Decoder.defaultConfig, am, fst, wl) mfcs);
        print "\n";
        let 
            val realMsec = (Time.toMilliseconds o Timer.checkRealTimer) timerReal
        in
            print ("Time: real " 
                   ^ (LargeInt.toString realMsec)
                   ^ " msec; RTF "
                   ^ (Real.toString (Real.fromLargeInt realMsec / 10.0 / Real.fromInt (length mfcs)))
                   ^ "\n")
        end
    end
end;

TextIO.closeIn isCfg;
BinIO.closeIn isAm;
BinIO.closeIn isFst;
TextIO.closeIn isWl;
BinIO.closeIn isMfc;
