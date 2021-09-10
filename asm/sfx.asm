 ORG #A000

 DB 0FEH ; header
 DW LoaderBegin 
 DW LoaderEnd-1
 DW LoaderStart

LoaderBegin:
LoaderStart:
 INCBIN "AKG/SFX.AKX"
LoaderEnd:

 DISPLAY "SFX size is ", /D, LoaderEnd-LoaderBegin
