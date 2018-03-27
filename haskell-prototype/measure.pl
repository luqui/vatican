#!/usr/bin/perl

if (@ARGV < 2) {
    die <<USAGE;
Usage: measure.pl <interp> <limit>
    Prints timings of interpreters stacked n high, n varying from 0 to <limit>
    interp: one of bubs, thyer, ref
USAGE
}

system "ghc --make -O InterpreterStack";

my ($interp, $limit, $output) = @ARGV;

$output ||= 'VInt 9';

for my $i (0..$limit) {
    system "/usr/bin/time ./InterpreterStack $interp $i" and die;
}
