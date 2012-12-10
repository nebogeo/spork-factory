for (( i=1000; i <= 1042; i++ ))
do
    code=`cat code$i.txt`
    fitness=`cat fitness$i.txt`
    echo -e "set term png\n
         set title \"$code\"
         set xrange [-105:105]\n
         set yrange [-100:75]\n
         set output \"path$i.png\"\n
         pl \"path$i.txt\" title \"generation ${i:1}: fitness $fitness\" pt 7 lc rgb \"black\", \"light.txt\" title \"light source\" pt 7 ps 2 lc rgb \"#FFFF00\"  \n" | gnuplot;
done

