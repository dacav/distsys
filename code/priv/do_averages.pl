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

sub read_block {
    my ($F) = @_;
    my %vals;
    my %out;

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

    foreach my $k (keys %vals) {
        my $vs = $vals{$k};
        my $avg = average @$vs;
        $out{$k} = $avg;
    }

    return %out if wantarray;
    return \%out;
}

open my $F, '<', $ARGV[0];

# skip header
while (my $row = <$F>) {
    if ($row =~ /^plot/) {
        last;
    }
}

my $ok = read_block $F;
#my $fail = read_block $F;
#my $nostart = read_block $F;

foreach my $k (sort keys %$ok) {
    say "$k $ok->{$k}";
}

close $F;

