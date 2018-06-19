package JSON::LD::RDFa::Types;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Types::XSD::Lite qw(Language);
use Types::Standard -types, qw(slurpy);
use Type::Utils -all;
use Type::Library -base,
    -declare => qw(URIRef MaybeURIRef MaybeLang
                   Namespace NamespaceMap JSONLD JSONLDVersion
                   JSONLDString JSONLDContext JSONLDContexts
                   JSONLDRemoteContext JSONLDKeyword JSONLDContainerDef
              );
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

=head3 JSONLDString

=cut

declare JSONLDString, as Str, where { /^\s*[\[\{]/ };

=head3 JSONLDContext

=cut

declare JSONLDContext, as Dict[
    '@base'     => Optional[MaybeURIRef],
    '@vocab'    => Optional[MaybeURIRef],
    '@version'  => Optional[JSONLDVersion],
    '@language' => Optional[MaybeLang],
    slurpy Any], coercion => 1;

# coerce JSONLDContext, from JSONLDString, via { to_JSONLD($_) };

=head3 JSONLDRemoteContext

=cut

declare JSONLDRemoteContext, as Dict[
    '@context' => URIRef|JSONLDContext, slurpy Any], coercion => 1;

=head3 JSONLDContexts

=cut

declare JSONLDContexts, as ArrayRef[Maybe[JSONLDContext|URIRef]], coercion => 1;

coerce JSONLDContexts, from Undef,        via { [undef] };
coerce JSONLDContexts, from HashRef,      via { [to_JSONLDContext($_)] };
coerce JSONLDContexts, from Value,        via { [to_URIRef($_)] };
# coerce JSONLDContexts, from JSONLDString, via { [to_JSONLDContext($_)] };

declare JSONLDKeyword, as Enum[
    map { '@' . $_ } qw(base container context graph id index language list
                        nest none prefix reverse set type value version vocab)];

my @TESTS = (
    [qw(graph id)],
    [qw(graph index)],
    [qw(graph id set)],
    [qw(graph index set)],
    [qw(id set)],
    [qw(index set)],
    [qw(set type)],
);

declare JSONLDContainerDef, as ArrayRef[JSONLDKeyword], where {
    my @c = sort map { substr $_, 1 } @$_;
    return 1 if @c == 1 and
        grep { $c[0] eq $_ } qw(graph id index language list set type);

    for my $t (@TESTS) {
        return 1 if @c == @$t and @c == grep { $c[$_] eq $t->[$_] } (0..$#c);
    }

    return;
};

1;
