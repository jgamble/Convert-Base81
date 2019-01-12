#!perl
use warnings;
use strict;

use Test::More tests => 8;

use Convert::Base81 qw(base81_encode base81_decode);

my @codings = (
	[qq(\x01\x02\x03\x04\x05\x06\x07) x 8, q(0CWaodvuz) x 8],
	[q(01234560123456), q(7P_No6C^&7P_No6C^&)],
	[q(789abcd789abcd), q(8VX~burkS8VX~burkS)],
	[q(0123456789abcd), q(7P_No6C^&8VX~burkS)],
);

my $tno = 1;

for my $pair (@codings)
{
	my ($text, $encoded) = @$pair;

	my $test_encode = base81_encode($text);
	ok($test_encode eq $encoded, "${tno}a: '$text' encoded into '$test_encode', not '$encoded'");

	my $test_decode = base81_decode($encoded);
	ok($test_decode eq $text, "${tno}b: '$encoded' encoded into '$test_decode', not '$text'");

	$tno += 1;
}
