#!perl

use strict;
use warnings FATAL => 'all';

use Test::More;

plan tests => 3;

use_ok('JSON::LD::RDFa::Types');

my $js = JSON::LD::RDFa::Types::to_JSONLD('{ "@context": { } }');

is_deeply($js, { '@context' => {} }, 'json-ld coerces ok');

my $ctx = JSON::LD::RDFa::Types::to_JSONLDContexts
    ({ '@base' => 'http://foo.bar/' });

is_deeply($ctx, [{ '@base' => URI->new('http://foo.bar/') }],
          'context coerces ok');

#require Data::Dumper;
#diag Data::Dumper::Dumper($ctx);


