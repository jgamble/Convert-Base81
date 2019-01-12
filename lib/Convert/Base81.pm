package Convert::Base81;

use 5.016001;
use warnings;
use strict;

use Carp;
#use Smart::Comments q(###);

our $VERSION = '1.00';

use Exporter qw(import);

our %EXPORT_TAGS = (
	pack => [ qw(b3_pack81 b9_pack81 b27_pack81) ],
	unpack => [ qw(b3_unpack81 b9_unpack81 b27_unpack81) ],
);

our @EXPORT_OK = (
	qw(base81_check base81_encode base81_decode),
	@{ $EXPORT_TAGS{pack} },
	@{ $EXPORT_TAGS{unpack} },
);

#
# Add an :all tag automatically.
#
$EXPORT_TAGS{all} = [@EXPORT_OK];

=head1 NAME

Convert::Base81 - Encoding and decoding to and from Base 81 strings

=head1 SYNOPSIS

    use Convert::Base81;
 
    my $encoded = Convert::Base81::encode($data);
    my $decoded = Convert::Base81::decode($encoded);

or

    use Convert::Base81 qw(base81_encode base81_decode);
 
    my $encoded = base81_encode($data);
    my $decoded = base81_decode($encoded);

=head1 DESCRIPTION

This module implements a I<Base81> conversion for encoding binary
data as text. This is done by interpreting each group of seven bytes
as a 56-bit integer, which is then converted to a nine-digit base 81
representation using the alphanumeric characters 0-9, A-Z, and a-z, in
addition to the punctuation characters !, #, $, %, &, *, +, -, ;, <, =, >,
?, @, ^, _, `, |, and ~, in that order.

This creates a string that is nine sevenths (1.2857) larger than the original
data, making it more efficient than L<MIME::Base64>'s 3-to-4 ratio (1.3333)
but slightly less so than the efficiency of L<Convert::Base85> or
L<Convert::Ascii85>'s 4-to-5 ratio (1.25).

It does have the advantage of a natural ternary system: if your data has
values composed of only three, or nine, or twenty-seven characters, its
size can be compressed instead of expanded, and this module has functions
that will do that.

    use Convert::Base81 qw(b3_pack81 b3_unpack81);

    my $input_string = q(rrgrbgggggrrgbrrbbbbrbrgggrggggg);
    my $b81str = b3_pack81("rgb", $input_string);

The returned string will be one-fourth the size of the original. Equivalent
functions exist for 9-digit and 27-digit values, which will return strings
one-half and three-fourths the size of the original, respectively.

=cut

#
# character    value
#  0..9:        0..9
#  A..Z:        10..35
#  a..z:        36..61
#  punc:        62..80
#
# Or, in a 9x9 tabular form, displaying the trits (0, 1, -):
#
#               |    0      1     2      3      4      5      6      7      8
#               +-------------------------------------------------------------
# ('0'..'8')  0 | 0000   0001  000-   0010   0011   001-   00-0   00-1   00--
# ('9'..'H')  9 | 0100   0101  010-   0110   0111   011-   01-0   01-1   01--
# ('I'..'Q') 18 | 0-00   0-01  0-0-   0-10   0-11   0-1-   0--0   0--1   0---
# ('R'..'Z') 27 | 1000   1001  100-   1010   1011   101-   10-0   10-1   10--
# ('a'..'i') 36 | 1100   1101  110-   1110   1111   111-   11-0   11-1   11--
# ('j'..'r') 45 | 1-00   1-01  1-0-   1-10   1-11   1-1-   1--0   1--1   1---
# ('s'..'!') 54 | -000   -001  -00-   -010   -011   -01-   -0-0   -0-1   -0--
# ('#'..'<') 63 | -100   -101  -10-   -110   -111   -11-   -1-0   -1-1   -1--
# ('='..'~') 72 | --00   --01  --0-   --10   --11   --1-   ---0   ---1   ----
#
#
# Take a number from 0 to 80, and turn it into a character.
#
my @b81_encode = ('0' .. '9', 'A' .. 'Z', 'a' .. 'z',
	'!', '#', '$', '%', '&', '*', '+', '-', ';',
	'<', '=', '>', '?', '@', '^', '_', '`', '|', '~');

#
# Take the ord() of a character, and return the number (from 0 to 80)
# for it. Wrong characters return -1.
#
my @b81_decode = (
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
	-1, 62, -1, 63, 64, 65, 66, -1, -1, -1, 67, 68, -1, 69, -1, -1,
	 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, 70, 71, 72, 73, 74,
	75, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, 76, 77,
	78, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
	51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, 79, -1, 80, -1);

=head3 base81_check

Examine a string for characters that fall outside the Base 81 character set.

Returns the first character position that fails the test, or -1 if no characters fail.

    if (my $d = base81_check($base81str) != -1)
    {
        carp "Incorrect character at position $d; cannot decode input string";
        return "";
    }

=cut

sub base81_check
{
	my($str) = @_;
	my(@chars) = split(//, $str);

	for my $j (0 .. $#chars)
	{
		my $o = ord($chars[$j]);
		return $j if ($o > 0x7f or $b81_decode[$0] == -1);
	}
	return -1;
}


#for my $j (2 .. 18)
#{
#	my $e = 8 * $j;
#	my $x = int(1 + log(2 ** $e)/log(81));
#
#	print " read $j write $x has a ", sprintf("%1.4f", $x/$j), " ratio.\n";
#}
#
# >> read 7 write 9 has a   1.2857 ratio;
#    read 15 write 19 has a much better 1.2666 ratio, but we'd need 128-bit integers.
#
#

=head3 base81_encode

=head3 Convert::Base81::encode

Converts input data to Base81 test.

This function may be exported as C<base81_encode> into the caller's namespace.

=cut

sub encode
{
	my($plain) = @_;
	my @mlist;

	#
	# Extra zero bytes to bring the length up to a multiple of seven.
	#
	my $extra = -length($plain) % 7;
	$plain .= "\0" x -$extra;

	for my $str7 (unpack '(a7)*', $plain)
	{
		my $total7 = 0;
		my @tmplist = (0) x 9;

		map{$total7 = ($total7 << 8) + $_} unpack('c*', $str7);

		#
		### total7 = sprintf('0x%0x', $total7)
		#
		for my $j (reverse 0 .. 8)
		{
			last if ($total7 == 0);
			$tmplist[$j] = $total7 % 81;
			$total7 = int $total7/81;
		}
		push @mlist, @tmplist;
	}

	return join "",	map{$b81_encode[$_]} @mlist;
}

*base81_encode = \&encode;

=head3 base81_decode

=head3 Convert::Base81::decode

Converts the Base81-encoded string back to bytes. Spaces, linebreaks, and other
whitespace are stripped from the string if present.

This function may be exported as C<base81_decode> into the caller's namespace.

=cut

sub decode
{
	my ($encoded) = @_;

	$encoded =~ tr[ \t\r\n\f][]d;

	my @mlist;
	my $extra = -length($encoded) % 9;

	if ($extra != 0)
	{
		warn "The encoded string length is not a multple of nine, and may be in error.";
		warn "Padding with zeros to the proper length.";
		$encoded .= '0' x $extra;
	}

	for my $str9 (unpack '(a9)*', $encoded)
	{
		my $total9 = 0;
		my @tmplist = (0) x 7;

		map{$total9 = $total9 * 81 + $_} map{$b81_decode[$_]} unpack('c*', $str9);

		#
		### Read string: $str9
		### total =  sprintf("0x%0x", $total9)
		#
		for my $j (reverse 0 .. 6)
		{
			last if ($total9 == 0);
			$tmplist[$j] = $total9 & 0xff;
			$total9 >>= 8;
		}
		push @mlist, @tmplist;
	}

	return join "",	map{chr($_)} @mlist;
}

*base81_decode = \&decode;

=head2 the 'pack' tag

If your data falls into a domain of characters of 3, 9, or 27, then the Base81
format can compress your data to 1/4, 1/2, or 3/4, its original size.


=head3 b3_pack81

    $three_chars = "01-";

    b3_pack81($three_chars, $inputstring);

or

    b3_pack81($three_chars, \@inputarray);

Transform a string (or array) consisting of three and only three
characters into a Base 81 string.

    $packedstr = b3_pack81("01-", "01-0-1011000---1");

or

    $packedstr = b3_pack81("01-", [qw(0 1 - 0 - 1 0 1 1 0 0 0 - - - 1)]);

=cut

sub b3_pack81
{
	my($c3, $data) = @_;
	my @blist;

	#
	# Set up the conversion hash and convert the column list
	# into two-bit values.
	#
	my $ctr = 0;
	my %convert3 = map{ $_ => $ctr++} split //, $c3;

	if (ref $data eq 'ARRAY')
	{
		@blist = map{$convert3{$_}} @{ $data };
	}
	else
	{
		@blist = map{$convert3{$_}} split //, $data;
	}

	push @blist, (substr $c3, 0, 1) x (-scalar(@blist) % 4);

	my $str = "";

	for my $j (1 .. scalar(@blist) >> 2)
	{
		my($z, $y, $x, $w) = splice(@blist, 0, 4);
		$str .= $b81_encode[27*$z + 9*$y + 3*$x + $w];
	}

	return $str;
}

=head3 b9_pack81

    b9_pack81("012345678", $inputstring);

or

    b9_pack81("012345678", \@inputarray);

Transform a string (or array) consisting of three and only three
characters into a Base 81 string.

    $packedstr = b9_pack81("012345678", "6354822345507611");

or

    $packedstr = b9_pack81("012345678", [qw(6 3 5 4 8 2 2 3 4 5 5 0 7 6 1 1)]);

=cut

sub b9_pack81
{
	my($c9, $data) = @_;
	my @blist;

	#
	# Set up the conversion hash and collect the input data.
	#
	my $ctr = 0;
	my %x9 = map{ $_ => $ctr++} split //, $c9;

	if (ref $data eq 'ARRAY')
	{
		@blist = map{$x9{$_}} @{ $data };
	}
	else
	{
		@blist = map{$x9{$_}} split //, $data;
	}

	#
	# Pad by a zero character if the data length is odd.
	#
	push @blist, substr $c9, 0, 1 if (scalar(@blist) % 2);

	my $str = "";

	for my $j (1 .. scalar(@blist) >> 1)
	{
		my($z, $y) = splice(@blist, 0, 2);
		$str .= $b81_encode[9*$z + $y];
	}

	return $str;
}

=head3 b27_pack81

    b27_pack81($twenty7_chars, $inputstring);

or

    b27_pack81($twenty7_chars, \@inputarray);

Transform a string (or array) consisting of only twenty-seven
characters into a Base 81 string.

    $base27str = join("", ('a' .. 'z', '_'));
    $packedstr = b27_pack81($base27str, "anxlfqunxpkswqmei_qh_zkr");

or

    $packedstr = b27_pack81($base27str, [qw(a n x l f q u n x p k s w q m e i _ q h _ z k r)]);

=cut

sub b27_pack81
{
	my($c27, $data) = @_;
	my @blist;
	my @clist;

	#
	# Set up the conversion hash and collect the input data.
	#
	my $ctr = 0;
	my %x27 = map{ $_ => $ctr++} split //, $c27;

	if (ref $data eq 'ARRAY')
	{
		@blist = map{$x27{$_}} @{ $data };
	}
	else
	{
		@blist = map{$x27{$_}} split //, $data;
	}

	#
	# Save any leftover characters in advance of taking four at a time.
	#
	my @tail = splice(@blist, scalar @blist - scalar(@blist) % 4);

	my $hold = 0;

	#
	#   z0  y0    z1  y1  z2  y2   z3   y3
	# |ooo p|pp qq|q rrr|sss t|tt uu|u vvv|
	# |     |     |     |     |     |     |
	#
	#
	# Take in four base-27 characters, write out three base-81 characters.
	#
	while (my(@x4) = splice(@blist, 0, 4))
	{
		my $x = 19683 * $x4[0] + 729 * $x4[1] + 27 * $x4[2] + $x4[3]; 
		my @mods;

		for (1 .. 3)
		{
			unshift @mods, $b81_encode[$x % 81];
			$x = int $x/81;
		}

		push @clist, @mods;
	}

	if (scalar @tail)
	{
		my $x = $tail[0] * 3;

		if (scalar @tail >= 2)
		{
			$x += $tail[1]/9;
			push @clist, $b81_encode[$x];
			$x = ($tail[1] % 9) * 9;
		}

		if (scalar @tail == 3)
		{
			$x += $tail[2]/3;
			push @clist, $b81_encode[$x];
			$x = $tail[2] % 3;
		}
		push @clist, $b81_encode[$x];
	}

	return join "", @clist;
}

=head2 the 'unpack' tag

Naturally, data packed must needs be unpacked, and the following three functions
perform that duty. Note that they only return strings, not arrays.

=head3 b3_unpack81

    $data = b3_unpack81($three_chars, $packedstring);

Transform a string (or array) consisting of three and only three
characters into a Base 81 string.

    $str = b3_unpack81($three_chars, "0028401725032021");

=cut

sub b3_unpack81
{
	my($c3, $base81str) = @_;

	$base81str =~ tr[ \t\r\n\f][]d;

	my @char81 = split(//, $base81str);
	my @val81 = map{$b81_decode[ord($_)]} @char81;

	#
	# Set up the conversion array on the fly.
	#
	my(@convert3) = split(//, $c3);
	my $c_idx = 0;
	my @clist;

	for my $x (@val81)
	{
		push @clist, $convert3[int($x/27)];
		push @clist, $convert3[int(($x % 27)/9)];
		push @clist, $convert3[int(($x % 9)/3)];
		push @clist, $convert3[$x % 3];
		$c_idx++;
	}

	return join "", @clist;
}

=head3 b9_unpack81

    $data = b9_unpack81($nine_chars, $packedstring);

Transform a string (or array) consisting of three and only three
characters into a Base 81 string.

    my $nine_chars = "012345678";

    $str = b9_unpack81($nine_chars, "0028401725032021");

=cut

sub b9_unpack81
{
	my($c9, $base81str) = @_;

	$base81str =~ tr[ \t\r\n\f][]d;

	my @char81 = split(//, $base81str);
	my @val81 = map{$b81_decode[ord($_)]} @char81;

	#
	# Set up the conversion array on the fly because
	# the don't-care character is changeable.
	#
	my(@x9) = split(//, $c9);
	my $c_idx = 0;
	my @clist;

	for my $x (@val81)
	{
		push @clist, $x9[int($x/9)];
		push @clist, $x9[$x % 9];
		$c_idx++;
	}

	return join "", @clist;
}


=head3 b27_unpack81

    $data = b27_unpack81($twenty7_chars, $packedstring);

Transform a string (or array) using only twenty seven
characters into a Base81 string.

    $twenty7_chars = join("", ('a' .. 'z', '&'));

    $str = b27_unpack81($twenty7_chars, "d`+qxW?q");

=cut

sub b27_unpack81
{
	my($c27, $base81str) = @_;

	$base81str =~ tr[ \t\r\n\f][]d;

	my @char81 = split(//, $base81str);
	my @val81 = map{$b81_decode[ord($_)]} @char81;

#print STDERR "\n";
#print STDERR map{sprintf("%2s, ", $_)} @char81;
#print STDERR "\n";
#print STDERR map{sprintf("%2d, ", $_)} @val81;
#print STDERR "\n";

	my @tail = splice(@val81, scalar @val81 - scalar(@val81) % 3);
	my(@x27) = split(//, $c27);
	my @clist;

	#
	# Take in 3 base-81 characters, write out four base-27 characters.
	#
	while (my(@x3) = splice(@val81, 0, 3))
	{
		my $x = 6561 * $x3[0] + 81 * $x3[1] + $x3[2]; 
		my @mods;

		for (1 .. 4)
		{
			unshift @mods, $x27[$x % 27];
			$x = int $x/27;
		}

		push @clist, @mods;
	}

	if (scalar @tail)
	{
		my $x = $tail[0];
		push @clist, $x27[int $x/3];
		$x = ($x % 3) * 9;

		if (scalar @tail == 2)
		{
			$x += int $tail[1]/9;
			push @clist, $x27[$x];
			$x = ($tail[1] % 9) * 3;
		}
		push @clist, $x27[$x];
	}

	return join "", @clist;
}

1;

__END__


=head1 FUNCTIONS


=head1 SEE ALSO

L<http://en.wikipedia.org/wiki/Ascii85>

L<MIME::Base64>

=head1 AUTHOR

John M. Gamble C<< <jgamble at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-convert-base81 at rt.cpan.org>,
or through the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Convert-Base81>.
I will be notified, and then you'll automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Convert::Base81

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2018 John M. Gamble.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

