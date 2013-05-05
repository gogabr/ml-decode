
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

    fun readCtl fname =
        let
            fun doLine acc is =
                case TextIO.inputLine is of
                    NONE => acc
                  | SOME l => doLine (String.tokens Char.isSpace l @ acc) is

            val linesRev = wrapTextFile (doLine []) fname
        in
            rev linesRev
        end
                            
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

              | ["--ctl", ctl] =>  { amFname = amFname,
                                     fstFname = fstFname,
                                     wlFname = wlFname,
                                     srcFnames = srcFnames @ readCtl ctl}
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
    fun processFile fname =
        let
            val timerReal = Timer.startRealTimer ()
            
            val mfcs = (Mfc.subCmn o KaldiFuns.computeMfccFile) fname
            val nframes = length mfcs

            val decodeRes = Decoder.decode (decoderConfig, am, fst, wl) mfcs

            val realMsec = (Time.toMilliseconds o Timer.checkRealTimer) timerReal

            (* This assumes that a frame is 10 msec *)
            val rtf = Real.fromLargeInt realMsec / 10.0 / Real.fromInt nframes
        in
            (decodeRes, nframes, realMsec)
        end

    fun doFileWithStats (fname, (minRtf, maxRtf, totFrames, totMsec)) =
        let
            val (decodeRes, nframes, msec) = processFile fname
            val newTotFrames = totFrames + nframes
            val newTotMsec = totMsec + msec
                                           
            fun rtf (nframes, msec) =
                Real.fromLargeInt msec / 10.0 / Real.fromInt nframes
                                                             
            val thisRtf = rtf (nframes, msec)
            val avgRtf = rtf (newTotFrames, newTotMsec)
                             
            val newMinRtf = Real.min (minRtf, thisRtf)
            val newMaxRtf = Real.max (maxRtf, thisRtf)
        in
            print (fname ^ ": " ^ decodeRes ^ "\n");
            print ("nframes " ^ Int.toString nframes 
                   ^ " msec " ^ LargeInt.toString msec 
                   ^ " RTF " ^ Real.toString thisRtf
                   ^ "\n");
            print ("total nframes " ^ Int.toString newTotFrames
                   ^ " msec " ^ LargeInt.toString newTotMsec
                   ^ "\n");
            print ("RTF: " ^ Real.toString newMinRtf ^ " - " ^ Real.toString newMaxRtf
                   ^ ", average " ^ Real.toString avgRtf
                   ^ "\n");
            (newMinRtf, newMaxRtf, newTotFrames, newTotMsec)
        end
in
    foldl doFileWithStats
          (Real.posInf, Real.negInf, 0, 0)
          (#srcFnames config)
end;

print ("Hits: " ^ Int.toString (Util.memoizeHits ()) 
       ^ ", misses " ^ Int.toString (Util.memoizeMisses ()) ^ "\n");
