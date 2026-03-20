#!/usr/bin/perl

$count = 0;
$maxln = 0;
@elen  = ();
@ecode = ();
@jcode = ();

# Pass 1: get data from etc/VERSION
while (<>) {
    split;
    if ( $_[0] eq "**" || $_[0] != 0 ) {
        # maybe it is a bug on etc/VERSION, so fix it
        my $ecode = ( $_[1] eq "USUGUMU" ) ? "USUGUMO" : $_[1];
        my $jcode = $_[3];
        my $elen = length( $ecode );
        chomp $jcode;
        push( @ecode, $ecode );
        push( @jcode, $jcode );
        push( @elen,  $elen );
        $maxln = ( $elen > $maxln ) ? $elen : $maxln;
        $count++;
    }
}

# Pass 2: make rail-table-mule.el

print "(defvar rail-mule-codename-alist\n";

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

print "))\n\n(provide 'rail-table-mule)\n";
