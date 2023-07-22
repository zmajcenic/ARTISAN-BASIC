for a in dsk/*.ASC;do echo $a;echo -n $'\032'>>$a;done
for a in dsk/*.BAS;do echo $a;echo -n $'\032'>>$a;done

