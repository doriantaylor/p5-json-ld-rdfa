#!perl

use strict;
use warnings FATAL => 'all';

use Test::More;

plan tests => 1;

use_ok('JSON::LD::RDFa::Context');


#my $in = { '@base' => 'http://foo.bar/', '@version' => 1.1 };
my $uri = 'http://foo.bar/';

my $ctx = JSON::LD::RDFa::Context->process($uri);
