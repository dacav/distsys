set size 5,5
set rmargin at screen 0.95
set tmargin at screen 0.95
set pointsize 1
set xlabel "probability"
set ylabel "time [ms]"
set xrange [-0.001:]
set yrange [0:851907]
set key right box
set term pdfcairo font "Sans,12" size 10in,6in
set output "dbcons.pdf"
set grid
set datafile missing "?"
plot '-' w p title "f > n/2 before consensus", '-' w l title "average"
0.055 642
0.06 1230
0.06 640
0.045 1160
0.035 38480
0.035 14336
0.035 17877
0.065 15992
0.05 16582
0.05 15356
0.05 20473
0.01 362910
0.02 43758
0.02 851907
0.025 262657
0.04 4448
0.03 59464
e
0.01 362910
0.02 447832.5
0.025 262657
0.03 59464
0.035 23564.3333333333
0.04 4448
0.045 1160
0.05 17470.3333333333
0.055 642
0.06 935
0.065 15992
