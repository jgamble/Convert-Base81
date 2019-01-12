#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'Convert::Base81' ) || print "Bail out!
";
}

diag( "Testing Convert::Base81 $Convert::Base81::VERSION, Perl $], $^X" );
