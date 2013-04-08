signature MFC =
sig

type mfc = RealVector.vector

val read: BinIO.instream -> mfc list

val subCmn: mfc list -> mfc list

end
