package JSON::LD::RDFa::Types;

use Types::XSD::Lite qw(Language);
use Types::Standard -types, qw(slurpy);
use Type::Utils -all;
use Type::Library -base,
    -declare => qw(URIRef MaybeURIRef MaybeLang
                   Namespace NamespaceMap
                   JSONLDVersion JSONLDContext JSONLDContexts);
use JSON;
use URI;
use URI::Namespace;
use URI::NamespaceMap;

=head3 URIRef, MaybeURIRef

=cut

class_type URIRef, { class => 'URI' };

declare MaybeURIRef, as Maybe[URIRef];

sub _coerce_uri { s/^\s*(.*?)\s*$/$1/sm; URI->new($_) }

coerce URIRef,      from Value, via \&_coerce_uri;
coerce MaybeURIRef, from Value, via \&_coerce_uri;

=head3 MaybeLang

=cut

declare MaybeLang, as Maybe[Language];

=head3 Namespace

=cut

class_type Namespace, { class => 'URI::Namespace' };

coerce Namespace, from Defined, via { URI::Namespace->new($_) };

=head3 NamespaceMap

=cut

class_type NamespaceMap, { class => 'URI::NamespaceMap' };

coerce NamespaceMap, from HashRef, via { URI::NamespaceMap->new($_) };

=head3 JSONLD

=cut

# we aren't going to do anything too ambitious with this

sub _coerce_jsonld {
    my $text = ref $_ ? $_ : \$_;
    my $json = JSON->new;
    $json->utf8(1) if utf8::is_utf8($$text);
    $json->decode($$text);
}

declare JSONLD, as HashRef|ArrayRef;

coerce JSONLD, from Value, via \&_coerce_jsonld;
coerce JSONLD, from ScalarRef, via \&_coerce_jsonld;

=head3 JSONLDVersion

=cut

declare JSONLDVersion, as Enum[1, 1.1];
coerce JSONLDVersion, from Value, via { $_ + 0.0 };

=head3 JSONLDContext

=cut

declare JSONLDContext, as Dict[
    '@base'     => Optional[MaybeURIRef],
    '@vocab'    => Optional[MaybeURIRef],
    '@version'  => Optional[JSONLDVersion],
    '@language' => Optional[MaybeLang],
    slurpy Any], coercion => 1;

=head3 JSONLDContexts

=cut

declare JSONLDContexts, as ArrayRef[JSONLDContext|URIRef], coercion => 1;

coerce JSONLDContexts, from Value,   via { [to_URIRef($_)] };
coerce JSONLDContexts, from HashRef, via { [to_JSONLDContext($_)] };

1;
