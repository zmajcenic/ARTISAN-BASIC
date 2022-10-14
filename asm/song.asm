 ORG #C000

 DB 0FEH ; header
 DW LoaderBegin 
 DW LoaderEnd-1
 DW LoaderStart

LoaderBegin:
LoaderStart:
 INCBIN "AKG/ALIENALL.AKG"
LoaderEnd:

 DISPLAY "Song size is ", /D, LoaderEnd-LoaderBegin
