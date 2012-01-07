#!/usr/bin/env gnuplot

events = "./" . prefix . "_events.log"
node_count = "./" . prefix . "_node_count.log"
est_node_count = "./" . prefix . "_est_node_count.log"
decision = "./" . prefix . "_decision_count.log"

outfile = "./" . prefix . ".pdf"

set size 5,5
set rmargin at screen 0.95
set tmargin at screen 0.95
set xrange [:];
set yrange [:];
set term pdf
set output outfile
set key left box;
set grid
set pointsize 0.5

load events
plot node_count w l title "node count", est_node_count title "estimated node count", decision w l title "deciding nodes"
