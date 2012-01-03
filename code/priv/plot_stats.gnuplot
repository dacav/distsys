#!/usr/bin/env gnuplot

events = "./" . prefix . "_events.log"
node_count = "./" . prefix . "_node_count.log"
est_node_count = "./" . prefix . "_est_node_count.log"
decision = "./" . prefix . "_decision_count.log"

outfile = "./" . prefix . ".pdf"

set term pdf
set output outfile

load events
plot node_count w l, est_node_count, decision w l
