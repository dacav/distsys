#!/usr/bin/perl -w

use strict;
use warnings;
use feature 'say';

my $STEPS = 120;
my $NODES = 500;



for (my $p = 0; $p <= 0.1; $p += 0.005) {
    my $p_alive = (1 - $p)**($STEPS);
    my $alive = $NODES * $p_alive;
    say "p=$p - $p_alive - At the end alive=$alive";
}
