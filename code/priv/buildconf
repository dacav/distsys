#!/usr/bin/perl -w

use Getopt::Long;

use strict;
use warnings;

my $REQ_INTEGER = [\&is_pinteger, 'integer'];
my $REQ_RATIO = [\&is_ratio, 'a ratio [0, 1]'];
my $REQ_PROBAB = [\&is_ratio, 'a probability value'];
my $REQ_PROBDIST = [\&is_mfa, 'Erlang MFA'];
my $REQ_FILENAME = [\&is_filename, 'valid - and decent - file name'];
my $REQ_ATOM = [\&is_atom, 'atom'];

my %REQUIRED = (
    # Requiring elaboration
    'npeers'      => $REQ_INTEGER,
    'tbeacon'     => $REQ_INTEGER,
    'beaconwait'  => $REQ_INTEGER,
    'tfail'       => $REQ_INTEGER,
    'tcleanup'    => $REQ_INTEGER,
    'tgossip'     => $REQ_INTEGER,
    'statpeers'   => $REQ_RATIO,
    'file_prefix' => $REQ_FILENAME,
    'keeper'      => $REQ_ATOM,

    # Just to be dumped
    'faulty_prob' => $REQ_PROBAB,
    'faulty_fail_prob' => $REQ_PROBAB,
    'deliver_mindel' => $REQ_INTEGER,
    'deliver_maxdel' => $REQ_INTEGER,
    'deliver_dist' => $REQ_PROBDIST,
);

sub print_kv ($$$) {
    my ($H, $k, $withcomma) = @_;
    my $v = $H->{$k};

    print "\t{$k, $v}", $withcomma ? ',' : '', "\n";
}

sub build_config (\%) {
    my $params = shift;
    my $tmp;

    print "[{ yuna, [\n";

    print_kv($params, 'faulty_prob', 1);
    print_kv($params, 'faulty_fail_prob', 1);

    print_kv($params, 'deliver_mindel', 1);
    print_kv($params, 'deliver_maxdel', 1);
    print_kv($params, 'deliver_dist', 1);

    $tmp = $params->{file_prefix};
    print "\t{log_args, { standard_error,\n",
          "\t             \"${tmp}\" }\n",
          "\t},\n";

    $tmp = $params->{keeper};
    print "\t{keeper, $tmp},\n";

    print "\t{keeper_args, {{",
          $params->{tfail}, ', ',
          $params->{tcleanup}, ', ',
          $params->{tgossip}, "},\n\t",
          "\t       ", $params->{npeers}, ",\n\t",
          "\t       ", $params->{statpeers}, ",\n\t",
          "\t       ", $params->{tbeacon}, ",\n\t",
          "\t       ", $params->{beaconwait}, "}\n\t",
          "}\n";

    print "]}].\n";
}

sub is_ratio () {
    my $val = shift;
    return ($val >= 0 && $val <= 1);
}

sub is_pinteger ($) {
    return int( $_[0] =~ /^\d+$/ );
}

sub is_mfa ($) {
    return int( $_[0] =~ /^{\s*[a-z_]+\s*,\s*[a-z_]+\s*,\s*\[.*\]\s*}$/ );
}

sub is_filename ($) {
    return int( $_[0] =~ /^[a-zA-Z0-9-._]+$/ );
}

sub is_atom ($) {
    return int( $_[0] =~ /^[a-z_]+$/ );
}

sub check_missing (\%\%) {
    my $required = shift;
    my $obtained = shift;
    my $good = 1;

    foreach my $k (keys %{$required}) {
        if (not defined $obtained->{$k}) {
            $good = 0;
            print STDERR "Missing parameter: $k\n";
        }
    }

    return $good;
}


sub ignore ($$) {
    my ($row, $cause) = @_;
    print STDERR "Warning: ignoring '$row' ($cause)\n";
}

sub get_params {
    my $params = {};

    while (my $row = <STDIN>) {
        chomp $row;
        my @kv = split(/ /, $row);

        if (@kv < 2) {
            ignore($row, 'wrong format');
            next;
        }
        if (@kv > 2) {
            @kv = ($kv[0], join(' ', @kv[1 .. @kv - 1]));
        }

        if (not defined $REQUIRED{$kv[0]}) {
            ignore($row, 'unknown constant');
            next;
        }

        my ($check, $type) = @{$REQUIRED{$kv[0]}};
        if ($check != 0 and not &{$check}($kv[1])) {
            ignore($kv[1], "must be $type");
        } else {
            $params->{$kv[0]} = $kv[1];
        }
    }

    return $params;
}

sub build_skeleton ($) {
    my $fname = shift;

    open(my $F, '>', $fname) or die $!;
    foreach my $k (keys %REQUIRED) {
        print $F "$k \n";
    }
    close($F);
}

sub main {

    my $skeleton = '';
    GetOptions('skel=s' => \$skeleton);

    if ($skeleton) {
        build_skeleton $skeleton;
    } else {
        my $params = &get_params;
        if (not check_missing(%REQUIRED, %{$params})) {
            exit(1);
        }
        build_config %{$params};
    }

    exit(0);
}

&main;
