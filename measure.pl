#!/usr/bin/perl

use IO::CaptureOutput qw(capture_exec);

if (@ARGV < 2) {
    die <<USAGE;
Usage: measure.pl <interp> <limit>
    Prints timings of interpreters stacked n high, n varying from 0 to <limit>
    interp: one of bubs, thyer, ref
USAGE
}

if (not -f "interp") {
    system "ghc --make -O interp";
}

my ($interp, $limit, $output) = @ARGV;

$output ||= 'VInt 9';

for my $i (0..$limit) {
    my ($stdout, $stderr) = capture_exec("/usr/bin/time -f %U ./interp $interp $i");
    chomp $stdout;
    if ($stdout ne $output) {
        die "Interpreter failure at level $i: expecting '$output', got '$stdout'";
    }
    print $stderr;
}
