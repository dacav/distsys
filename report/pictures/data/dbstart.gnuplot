set size 5,5
set rmargin at screen 0.95
set tmargin at screen 0.95
set pointsize 1
set xlabel "probability"
set ylabel "time [ms]"
set xrange [-0.001:]
set yrange [0:0]
set key right box
set term pdfcairo font "Sans,12" size 10in,6in
set output "dbstart.pdf"
set grid
set datafile missing "?"
plot '-' w p title "f > n/2 before starting", '-' w l title "average"
0.055 0
0.055 0
0.055 0
0.06 0
0.06 0
0.06 0
0.07 0
0.07 0
0.07 0
0.07 0
0.045 0
0.045 0
0.045 0
0.045 0
0.075 0
0.075 0
0.065 0
0.065 0
0.065 0
0.065 0
0.05 0
0.05 0
0.04 0
0.04 0
0.08 0
e
0.04 0
0.045 0
0.05 0
0.055 0
0.06 0
0.065 0
0.07 0
0.075 0
0.08 0
