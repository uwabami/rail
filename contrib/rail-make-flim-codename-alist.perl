#!/usr/bin/perl

$count = 0;
$maxln = 0;
@elen  = ();
@ecode = ();
@jcode = ();

# Pass 1: get data from VERSION
while (<>) {
    my $letter1 = substr( $_, 0, 1 );
    if ( $letter1 eq "-" || $letter1 eq "0" || $letter1 != 0 ) {
        chomp;
        split "\t+";
        $letter1 = substr( $_[1], 0, 1 );
        next if ( $letter1 eq '-' || $letter1 eq '(' );
        my $ecode = $_[1];
        my $jcode = $_[2];
        my $elen = length( $ecode );
        if ( $ecode =~ m/\033/ ) {
            $elen -= 6;
        }
        chomp $jcode;
        push( @ecode, $ecode );
        push( @jcode, $jcode );
        push( @elen,  $elen );
        $maxln = ( $elen > $maxln ) ? $elen : $maxln;
        $count++;
    }
}

# Pass 2: make rail-table-flim.el

print ";; -*- coding: iso-2022-8bit-ss2; -*-\n\n";
print "(defvar rail-flim-codename-alist\n";

for $i ( 0 .. $count - 1 ) {
    my $ecode = shift( @ecode );
    my $jcode = shift( @jcode );
    my $elen  = shift( @elen );
    if ( $i == 0 ) {
        print "  '((\"";
        $first = 0;
    } else {
        print "\n    (\"";
    }
    print $ecode, '"', ' ' x ( $maxln - $elen + 1 ), '. "', $jcode, '")';
}

print "))\n\n(provide 'rail-table-flim)\n";
