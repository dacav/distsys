set size 5,5
set rmargin at screen 0.95
set tmargin at screen 0.95
set pointsize 1
set xlabel "probability"
set ylabel "time [ms]"
set xrange [-0.001:]
set yrange [0:216993]
set key right box
set term pdfcairo font "Sans,12" size 10in,6in
set output "ok_k.pdf"
set grid
set datafile missing "?"
plot '-' w p title "Consensus reached (killed coordinator)", '-' w l title "average"
0.005 87246
0.005 56264
0.005 104533
0.005 216993
0.06 4762
0.035 28448
0.01 15351
0.01 87537
0.01 105433
0.01 106305
0 70309
0 71129
0 99863
0 31914
0 98312
0.02 71610
0.02 48223
0.02 48908
0.025 41351
0.025 18776
0.025 47802
0.015 86209
0.015 61988
0.015 65248
0.015 61449
0.03 5978
e
0 74305.4
0.005 116259
0.01 78656.5
0.015 68723.5
0.02 56247
0.025 35976.3333333333
0.03 5978
0.035 28448
0.06 4762
