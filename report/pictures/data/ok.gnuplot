set size 5,5
set rmargin at screen 0.95
set tmargin at screen 0.95
set pointsize 1
set xlabel "probability"
set ylabel "time [ms]"
set xrange [-0.001:]
set yrange [0:62993]
set key right box
set term pdfcairo font "Sans,12" size 10in,6in
set output "ok.pdf"
set grid
set datafile missing "?"
plot '-' w p title "Consensus reached", '-' w l title "average"
0.055 2385
0.005 4274
0.005 3946
0.005 2454
0.005 3672
0.005 1961
0.035 32955
0.035 2771
0.01 2161
0.01 4488
0.01 2617
0.01 2861
0 1890
0 10285
0 6561
0 2781
0 3073
0.02 1955
0.02 6920
0.02 3461
0.025 2073
0.025 62993
0.025 20520
0.025 6539
0.04 2054
0.04 2600
0.015 2523
0.015 7572
0.015 2552
0.015 1942
0.015 2381
0.03 2046
0.03 3676
0.03 9782
0.03 3390
e
0 4918
0.005 3261.4
0.01 3031.75
0.015 3394
0.02 4112
0.025 23031.25
0.03 4723.5
0.035 17863
0.04 2327
0.055 2385
