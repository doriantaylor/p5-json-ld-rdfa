#!perl

use strict;
use warnings FATAL => 'all';

use Test::More;

plan tests => 1;

use_ok('JSON::LD::RDFa');

my $json = JSON::decode_json(<<'JSON');
{
  "@context": {
    "@base": "http://foo.com/",
    "@language": "en",
    "@vocab": "http://www.w3.org/1999/xhtml/vocab#",
    "bibo": "http://purl.org/ontology/bibo/",
    "dct": "http://purl.org/dc/terms/",
    "foaf": "http://xmlns.com/foaf/0.1/"
  },
  "@id": "",
  "@type": "foaf:Document",
  "bibo:authorList": { "@list": [ "Joe Bobson", "Bob Joeson" ] },
  "dct:title": { "@set": ["hi", { "@value": "bonjour", "@language": "fr" }] },
  "dct:hasPart": [
    {
        "@id": "clowns",
        "@type": "bibo:DocumentPart",
        "dct:title": "something awesome",
        "dct:hasPart": {
            "@id" : "derp",
            "@type": "bibo:DocumentPart",
            "dct:title": "honk" }
      },
      {
          "@id": "special",
          "@type": "bibo:DocumentPart",
          "dct:title": "hiyaaaaaa"
      }
  ]
}
JSON

my $converter = JSON::LD::RDFa->new;

my $doc = $converter->convert($json);

warn $doc->toString(1);


