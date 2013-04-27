
fun wrapBinFile reader fname =
    let
        val is = BinIO.openIn fname
    in
        reader is
        before BinIO.closeIn is
    end

fun wrapTextFile reader fname =
    let
        val is = TextIO.openIn fname
    in
        reader is
        before TextIO.closeIn is
    end
                   
local
    val argv = CommandLine.arguments ()

    val _ = if length argv = 0
            then
                ( print "Usage: decode conffile...\n"
                ; OS.Process.exit OS.Process.failure)
            else ()

    (* Treat all arguments as config file names *)
    fun cfgLinesFromInputStream acc is =
        case TextIO.inputLine is of
            NONE => acc
          | SOME l => 
            cfgLinesFromInputStream (String.tokens (fn c => Char.isSpace c
                                                            orelse c = #"=") 
                                                   l
                                     :: acc) is
                                    
    val allCfgLines =
        rev (foldl (fn (ifname, acc) =>
                       wrapTextFile (cfgLinesFromInputStream acc) ifname)
                   []
                   argv)
            
    val defaultConfig = {
        amFname = "final.mdl",
        fstFname = "HCLG.fst",
        wlFname = "words.txt",
        srcFnames = []
    }
                            
    fun processCfgLine (tcl, acc) =
        let
            val {amFname, fstFname, wlFname, srcFnames } = acc
        in
            case tcl of
                ["--acoustic_model", afn] => { amFname = afn, 
                                               fstFname = fstFname,
                                               wlFname = wlFname,
                                               srcFnames = srcFnames}
              | ["--fst", ffn] => { amFname = amFname,
                                    fstFname = ffn,
                                    wlFname = wlFname,
                                    srcFnames = srcFnames }
              | ["--word_list", wfn] => { amFname = amFname,
                                          fstFname = fstFname,
                                          wlFname = wfn,
                                          srcFnames = srcFnames }

              | ["--source", sfn] => { amFname = amFname,
                                            fstFname = fstFname,
                                            wlFname = wlFname,
                                            srcFnames = srcFnames @ [sfn]}
              | _ => acc
        end

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
in
    val config = foldl processCfgLine defaultConfig allCfgLines
    val decoderConfig = Decoder.processConfig allCfgLines
                                                           
    val am = wrapBinFile AcousticModel.read (#amFname config)
    val fst = wrapBinFile Fst.readFst (#fstFname config)
    val wl = wrapTextFile readWordList (#wlFname config)
end;

print "Read model\n";


let
    fun processMfcFile mfcFname =
        let
            val timerReal = Timer.startRealTimer ()
            
            val mfcs = (Mfc.subCmn o KaldiFuns.computeMfccFile) mfcFname
            val nframes = length mfcs

        in
            print (mfcFname ^ "; nframes: " ^ Int.toString nframes ^ "\n");

            app (fn w => print (w ^ " ")) 
                (Decoder.decode (decoderConfig, am, fst, wl) mfcs);
            print "\n";
            let 
                val realMsec = (Time.toMilliseconds o Timer.checkRealTimer) timerReal
            in
                print ("Time: real " 
                       ^ (LargeInt.toString realMsec)
                       ^ " msec; RTF "
                       ^ (Real.toString (Real.fromLargeInt realMsec / 10.0 / Real.fromInt nframes))
                       ^ "\n")
            end
        end
in
    app processMfcFile (#srcFnames config)
end;
