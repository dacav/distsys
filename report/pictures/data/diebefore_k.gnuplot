set size 5,5
set rmargin at screen 0.95
set tmargin at screen 0.95
set pointsize 1
set xrange [-0.001:]
set yrange [0:958887]
set key right box
set term pdfcairo font "Sans,12" size 10in,6in
set output "diebefore_k.pdf"
set grid
set datafile missing "?"
plot '-' w p title "diebefore_k", '-' w l title "avg"
0.055 12890
0.055 12811
0.005 958887
0.06 8703
0.07 11006
0.045 4261
0.045 35266
0.045 17989
0.045 34213
0.035 39040
0.035 4430
0.035 15445
0.035 23251
0.065 6196
0.05 8120
0.05 4743
0.05 544
0.05 639
0.01 98856
0.02 161217
0.02 89497
0.025 81677
0.025 47806
0.04 20548
0.04 33016
0.04 55652
0.04 4804
0.04 89679
0.015 238239
0.03 33749
0.03 29103
0.03 30840
0.03 6101
e
0.005 958887
0.01 98856
0.015 238239
0.02 125357
0.025 64741.5
0.03 24948.25
0.035 20541.5
0.04 40739.8
0.045 22932.25
0.05 3511.5
0.055 12850.5
0.06 8703
0.065 6196
0.07 11006