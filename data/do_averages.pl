#!/usr/bin/perl -w

use strict;
use warnings;
use feature 'say';

sub average {
    my $sum = 0;
    my $N = 0;

    foreach my $v (@_) {
        $sum += $v;
        $N ++;
    }

    return $sum == 0 ? 0 : $sum / $N;
}

sub max {
    my $max = 0;
    foreach my $v (@_) {
        if ($v > $max) {
            $max = $v;
        }
    }
    return $max;
}

sub read_block {
    my ($F) = @_;
    my %vals;

    while (my $row = <$F>) {
        chomp $row;
        last if $row =~ /^e/;
        next if $row =~ /\? *$/;
        next if $row =~ /^#/;
        my ($p, $t) = split(/ /, $row);
        my $vs;

        $vs = $vals{$p};
        if (not defined $vs) {
            $vs = $vals{$p} = [];
        }
        push(@{$vs}, $t);
    }

    return %vals if wantarray;
    return \%vals;
}

sub print_head {
    my ($F, $name, $max_y) = @_;

    say $F 'set size 5,5';
    say $F 'set rmargin at screen 0.95';
    say $F 'set tmargin at screen 0.95';
    say $F 'set pointsize 1';
    say $F 'set xlabel "probability"';
    say $F 'set ylabel "time [ms]"';
    say $F 'set xrange [-0.001:]';
    say $F "set yrange [0:${max_y}]";
    say $F 'set key right box';
    say $F 'set term pdfcairo font "Sans,10" size 10in,6in';
    say $F "set output \"$name.pdf\"";
    say $F 'set grid';
    say $F 'set datafile missing "?"';
}

sub load {
    my ($fn, $parts, @order) = @_;
    open my $F, '<', $fn;

    # skip header
    while (my $row = <$F>) {
        if ($row =~ /^plot/) {
            last;
        }
    }
    foreach my $part (@order) {
        $parts->{$part} = read_block $F;
    }

    close $F;
}

my %parts;
my @order = qw(ok dbcons dbstart ok_k dbcons_k dbstart_k);
my %titles = (
    'ok' => "Consensus reached",
    'dbcons' => "f > n/2 before consensus",
    'dbstart' => "f > n/2 before starting",
    'ok_k' => "Consensus reached (killed coordinator)",
    'dbcons_k' => "f > n/2 before consensus (killed coordinator)",
    'dbstart_k' => "f > n/2 before starting (killed coordinator)"
);
load $ARGV[0], \%parts, @order;

foreach my $part (@order) {
    open my $O, '>', "$part.gnuplot";

    my $P = $parts{$part};
    my %Avgs;
    my $title = $titles{$part};

    my @maxmax = ();
    foreach my $prob (keys %$P) {
        my $vals = $P->{$prob};
        my $avg = average @$vals;

        my $var = average map { abs($_ - $avg) ** 2 } @$vals;
        #my @new_vals = grep { $_ > $avg + $var } @$vals;

        #$vals = $P->{$prob} = \@new_vals;
        #$avg = average @$vals;

        $Avgs{$prob} = $avg;

        push(@maxmax, max @$vals);
    }
    my $M = max @maxmax;

    say "Max is $M";

    print_head $O, $part, $M;
    say $O "plot '-' w p title \"$title\", '-' w l title \"average\"";
    foreach my $prob (keys %$P) {
        foreach my $t (@{$P->{$prob}}) {
            say $O "$prob $t";
        }
    }
    say $O 'e';
    foreach my $prob (sort keys %$P) {
        say $O "$prob $Avgs{$prob}";
    }

    close $O;
}

