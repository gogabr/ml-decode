val isAm = BinIO.openIn "/home/gogabr/data/models/final.mdl";
val isFst = BinIO.openIn "/home/gogabr/data/models/HCLG-const.fst";
val isWl = TextIO.openIn "/home/gogabr/data/models/word_table.txt";
val isMfc = BinIO.openIn "/home/gogabr/data/wav8/filmy8.mfcd";
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

    val am = AcousticModel.read isAm
    val fst = Fst.readFst isFst
    val wl = readWordList isWl
    val mfcs = (Mfc.subCmn o Mfc.read) isMfc
in
    print (Fst.headerToString (Fst.header fst) ^ "\n");
    print ("nframes: " ^ Int.toString (length mfcs) ^ "\n");
    print ("nwords: " ^ Int.toString (Vector.length wl) ^ "\n");
    app (fn w => print (w ^ "\n")) 
        (Decoder.decode (Decoder.defaultConfig, am, fst, wl) mfcs);
    print "QQ\n"
end;

BinIO.closeIn isAm;
BinIO.closeIn isFst;
TextIO.closeIn isWl;
BinIO.closeIn isMfc;
