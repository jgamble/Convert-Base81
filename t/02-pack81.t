#!perl -T
use 5.010001;
use strict;
use warnings;
use Test::More tests => 20;

use Convert::Base81 qw(:pack :unpack);

my $b3chars = "01-";
my $b9chars = "012345678";
my $b27chars = "0123456789abcdefghijklmnopq";

my @tests3 = (
	["a", q(1100)],
	["b", q(1101)],
	["a0", q(11000000)],
	["a1", q(11000001)],
	["Zed", q(10--11111110)],
	["MG0_Blue", q(0-1101-10000--1-010-1-0--00-1111)],
	["Purdu3!!", q(0--1-00-1---1110-00-0010-0---0--)],
);

my @tests9 = (
	["a", q(40)],
	["b", q(41)],
	["a0", q(4000)],
	["a1", q(4001)],
	["MG0_Blue", q(2417008512526244)],
	["Purdu3!!", q(2762584362036868)],
);

my @tests27 = (
	["a", q(c0)],
	["StopAndG0", q(9f4nh14md1l0)],
	["zyxwvutsrqpo", q(kfk5jfb2if1qhejn)],
	["~~Wright~~", q(qqp5hmpfef5qqi)],
	["MG0_Blue", q(7al0pj7kimc)],
	["Purdu3!!", q(8f7qd663koo)],
);

my $tno = 1;

#
# Base 3 character set string to base81 and back.
#
for my $pair (@tests3)
{
	my($k, $v) = @$pair;

	my $b81str = b3_pack81($b3chars, $v);
	ok($b81str eq $k, "${tno}a: Base81 string should be '$k', but is '$b81str'");

	my $b3str = b3_unpack81($b3chars, $k);
	ok($b3str eq $v, "${tno}b: Base3 string should be '$v', but is '$b3str'");
	$tno++;
}

#
# Base 9 character set string to base81 and back.
#
#for my $pair (@tests9)
#{
	#my($k, $v) = @$pair;
#
	#my $b81str = b9_pack81($b9chars, $v);
	#ok($b81str eq $k, "${tno}a: Base81 string should be '$k', but is '$b81str'");
#
	#my $b9str = b9_unpack81($b9chars, $k);
	#ok($b9str eq $v, "${tno}b: Base9 string should be '$v', but is '$b9str'");
	#$tno++;
#}

#
# Base 27 character set string to base81 and back.
#
for my $pair (@tests27)
{
	my($k, $v) = @$pair;

	my $b81str = b27_pack81($b27chars, $v);
	ok($b81str eq $k, "${tno}a: Base81 string should be '$k', but is '$b81str'");

	#my $b27str = b27_unpack81($b27chars, $k);
	#ok($b27str eq $v, "${tno}b: Base27 string should be '$v', but is '$b27str'");
	$tno++;
}


