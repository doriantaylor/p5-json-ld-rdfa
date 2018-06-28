#!perl

use strict;
use warnings FATAL => 'all';

use Test::More;

plan tests => 1;

use_ok('JSON::LD::RDFa::Context');

my $uri = 'https://json-ld.org/test-suite/tests/expand-0002-in.jsonld';

my $thing = JSON::decode_json(<<'DURR');
{
  "@context": {
    "t1": "http://example.com/t1",
    "t2": "http://example.com/t2",
    "term1": "http://example.com/term1",
    "term2": "http://example.com/term2",
    "term3": "http://example.com/term3",
    "term4": "http://example.com/term4",
    "term5": "http://example.com/term5"
  },
  "@id": "http://example.com/id1",
  "@type": "t1",
  "term1": "v1",
  "term2": {"@value": "v2", "@type": "t2"},
  "term3": {"@value": "v3", "@language": "en"},
  "term4": 4,
  "term5": [50, 51]
}
DURR

my $json = JSON::LD::RDFa::Context->new(base => $uri)->expand($thing);

#require Data::Dumper;
#warn Data::Dumper::Dumper($json);
my $proc = JSON->new->utf8->canonical->convert_blessed->pretty;
diag($proc->encode($json));

