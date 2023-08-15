for a in dsk/*.ASC; do printf '\x1a' >> $a;done
for a in dsk/*.BAS; do printf '\x1a' >> $a;done
