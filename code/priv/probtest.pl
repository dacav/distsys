#!/usr/bin/perl -w

# Failure Probability test
#
# Gradually failure probability to 1 and measure time needed to reach
# consensus.

use strict;
use warnings;
use feature 'say';

my $DEFAULT_CONF = 'configs/default';
my $BUILD_CONF = 'priv/buildconf.pl';
my $PROB_INCREMENT = 0.01;
my $MAX_FAILS = 5;
my $EXPERIMENTS_PER_PROB = 5;

sub tamper_config {
    my ($prob, $conf_fn) = @_;

    open(my $default_conf, '<', $DEFAULT_CONF);
    my @DC = <$default_conf>;
    close($default_conf);

    for (my $i = 0; $i < @DC; $i ++) {
        if ($DC[$i] =~ /^faulty_fail_prob/) {
            $DC[$i] = 'faulty_fail_prob 1';
        } elsif ($DC[$i] =~ /^faulty_prob/) {
            $DC[$i] = "faulty_prob $prob";
        } elsif ($DC[$i] =~ /^file_prefix/) {
            $DC[$i] = "file_prefix $conf_fn";
        } else {
            chomp $DC[$i];
        }
    }

    return join("\n", @DC);
}

sub launch_erlapp {
    my $conf_name = shift;

    my @vals = `erl -pa ebin -noshell -eval 'file:eval("priv/batch_yuna.erl")' -config $conf_name -s init stop`;
    print for @vals;
}

sub get_history {
    my $conf_name = shift;
    my @history = ();

    say "Reading events from ${conf_name}_events.log";
    open(my $events, '<', "${conf_name}_events.log");
    while (my $row = <$events>) {
        if ($row =~ /^set arrow/) {
            next;
        }
        my ($msg, $t) = $row =~ /^set label "([^"]+)" at (\d+),.*$/;
        push(@history, [$t, $msg]);
    }
    close($events);

    return \@history;
}

sub analyze_history {
    my $history = shift;
    my $start = -1;
    my $outcome;
    my $when = 0;

    foreach my $entry ( @{$history} ) {
        my $evid;

        if ($entry->[1] eq 'start protocol') {
            $start = $entry->[0];
        } elsif ($entry->[1] eq 'stop protocol') {
            $outcome = 'consensus';
            $when = $entry->[0] - $start;
        } else {
            $entry->[1] eq 'f > n/2' or die 'what?';
            if ($start != -1) {
                $when = $entry->[0] - $start;
                $outcome = 'abort';
            } else {
                $outcome = 'nostart';
            }
        }
    }

    return [ $outcome, $when ];
}

sub run_experiment {
    my ($prob, $id) = @_;
    my $conf_name = "pfail_$prob";

    # Building quick configuration with given probability vaue;
    my $conf = tamper_config($prob, $conf_name);

    # From quick configuration to complete Erlang configuration;
    say "Building the configuration...";
    `$BUILD_CONF << EOF > $conf_name.config\n$conf\nEOF`;

    say "Launcing erlang, experiment with p_fail=$prob";
    launch_erlapp($conf_name);
    my $result = analyze_history get_history($conf_name);

    say "Cleaning up...";
    unlink "${conf_name}$_" for qw(.config
                                   _decision_count.log
                                   _est_node_count.log
                                   _events.log
                                   _node_count.log);

    return $result;
}

sub to_point {
    my ($prob, $result) = @_;
    my ($what, $when) = @$result;
    my @out = ($prob, '?', '?', '?');
    my $idx;

    # Plotting on 3 columns:
    #   column 0 -> value of probability (x axis);
    #   column 1 -> time of termination (or '?' if didn't terminate;
    #   column 2 -> time of abortion (if f > n/2 during algorithm) or
    #               '?' either if abortion or start didn't happen;
    #   column 3 -> 0 iff nostart, else '?' (note: in this case $when=0)
    if ($what eq 'consensus') {
        $idx = 1;
    } elsif ($what eq 'abort') {
        $idx = 2;
    } elsif ($what eq 'nostart') {
        $idx = 3;
    } else {
        die "WTF is $what?";
    }
    $out[$idx] = $when;

    return @out if wantarray;
    return \@out;
}

sub main {
    if ($#ARGV < 0) {
        say "Give me the output file for gnuplot, please";
        exit 1;
    }
    my $fn = $ARGV[0];
    my @dataset = ();

    for (my $cycle = 0; $cycle < $EXPERIMENTS_PER_PROB; $cycle ++ ) {
        my $fails = 0;
        my $p = 0;
        do {
            say "Running experiment p=$p cycle=$cycle";
            my $point = to_point($p, run_experiment($p));
            push(@dataset, $point);
            if ($point->[3] ne '?') {
                $fails ++;
                say "We got $fails consecutive failures (cycle=$cycle)";
            }
            $p += $PROB_INCREMENT;
        } while ($fails < $MAX_FAILS || $p >= 1);
    }

    open(my $F, '>', $ARGV[0]);

    say $F 'set xrange [:]';
    say $F 'set yrange [:]';
    say $F 'set key left box';
    say $F 'set term pdf';
    say $F "set output \"${fn}.pdf\"";
    say $F 'set grid';
    say $F 'set datafile missing "?"';

    my %titles = (
        1 => 'consensus reached',
        2 => 'reached f > n/2 before consensus',
        3 => 'reached f > n/2 before starting'
    );

    print $F 'plot ';
    print $F join(', ', map {"'-' with points title \"$titles{$_}\""} (1,2,3));
    print $F "\n";
    for (my $column = 1; $column < 4; $column ++) {
        foreach my $point (@dataset) {
            say $F "@{$point}[0, $column]";
        }
        say $F 'e';
    }
    close $F;
}

&main;
