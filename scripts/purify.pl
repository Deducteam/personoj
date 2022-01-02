#!/usr/bin/perl
use strict;
use warnings;

=pod

=head1 Theory specification sanitiser

This script sanitises a theory specification. The flag C<-n> selects negative
lines (lines preceded with a -) while the flac C<-p> selects positive lines
(not preceded with a -). If no flag is given, all lines are printed and
sanitised.

=cut

use Getopt::Std;
getopts('np');
our($opt_n, $opt_p);

map {
	$_ =~ s/#.*$//;
	$_ =~ s/^\s+//;
	$_ =~ s/\s+$//;
	if ($opt_n && /^-/) {
		# Print negative theories
		$_ =~ s/^-(.*)$/$1/;
		print "$_\n";
	} elsif ($opt_p) {
		# Print positive theories
		print "$_\n" if /^[^-]/;
	} elsif (! ($opt_n || $opt_p)) {
		# Print all theories
		$_ =~ s/^-?(.*)$/$1/;
		print "$_\n";
	}
} <>;
