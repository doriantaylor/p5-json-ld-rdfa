#!perl -T
use 5.006;
use strict;
use warnings;
use Test::More;

plan tests => 1;

BEGIN {
    use_ok( 'JSON::LD::RDFa' ) || print "Bail out!\n";
}

diag( "Testing JSON::LD::RDFa $JSON::LD::RDFa::VERSION, Perl $], $^X" );
