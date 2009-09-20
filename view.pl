#!/usr/bin/perl

my $accum = '';
while (<>) {
    $accum .= $_;
    if (/^}/) {
        open my $fh, '| dot -T png -o tmp.png';
        print $fh $accum;
        close $fh;

        system 'eog tmp.png';
        $accum = '';
    }
}
