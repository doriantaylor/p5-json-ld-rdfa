package JSON::LD::RDFa;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Moo;
use JSON;

use Type::Params    qw(compile Invocant);
use Types::Standard qw(slurpy Any Maybe Optional ArrayRef HashRef CodeRef
                       Dict Bool);

use JSON::LD::RDFa::Context;
use JSON::LD::RDFa::Error;
use JSON::LD::RDFa::Types qw(URIRef is_URIRef MaybeURIRef MaybeLang
                             NamespaceMap is_JSONLDKeyword JSONLDVersion
                             JSONLDContextObj JSONLDContexts
                             is_JSONLDContainerDef JSONLDWildCard);

with 'Role::Markup::XML';

=head1 NAME

JSON::LD::RDFa - Turn JSON-LD into (X)HTML+RDFa

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

  # initialize the generator
  my $generator = JSON::LD::RDFa->new(%opts);

  # convert $json, either text or object
  my $doc = $generator->convert($json);

  # $doc is an XML::LibXML::Document

  print $doc->toString(1);

=head1 METHODS

=head2 new %PARAMS

=cut

# a subroutine which will take a URI and produce JSON-LD
sub _no_deref {
    JSON::LD::RDFa::Error->throw(
        'This instance doesn\'t support remote contexts!');
}

has deref => (
    is      => 'ro',
    isa     => Maybe[CodeRef],
    coerce  => sub { defined $_ ? $_ : \&_no_deref },
    default => sub { \&_no_deref },
);

=head2 convert $JSONLD [, %OPTIONS ]

Generate an L<XML::LibXML::Document> from JSON-LD input, which may be
a valid JSON string, a reference to such a string, or a Perl data
structure representing valid JSON-LD. There are additional options:

=over 4

=item uri

The URI of the document. Assumed to be a L<URI> object, and will be
coerced into one.

=item context

This can either be a context object, the URI of a remote context, or
an C<ARRAY> reference containing a mix of the prior two. Note that by
design, this module does not dereference remote documents. To
dereference you must pass in your own L</deref> function.

=item deref

This is a C<CODE> reference that is expected to return the contents of
a JSON object, either parsed or unparsed.

B<THIS IS NOT IMPLEMENTED YET.> I am particularly concerned about how
to decouple the dereferencing code in a way that appropriately handles
cycles of references. Nevertheless a working function would probably
look something like this:

  sub deref_json {
    my $uri = shift;

    # LWP (HTTP) is just *one* way to dereference remote JSON-LD
    # content; application developers will undoubtedly have others

    my $ua = LWP::UserAgent->new;

    # for instance we can insist on a content type
    $ua->default_header(
      Accept => 'application/ld+json;q=1.0, application/json;q=1.0, */*;q=0');

    my $resp = $ua->get($uri);

    # this function is `eval`ed and any terminal errors are bubbled up
    die $resp->code unless $resp->is_success;

    # for big content we can return a SCALAR reference so as not to
    # duplicate it in memory
    my $json = $resp->content_ref;

    # since cycles are a big no-no we collect any redirects...
    my @redir;
    do {
      push @redir, $resp->request->uri;
    } while ($resp = $resp->previous);

    # ...and return them along with the content (reference).
    return ($json, @redir);
  }

=cut

sub convert {
    state $check = Type::Params::compile(
        Invocant, JSONLDWildCard,
        slurpy Dict[uri     => Optional[MaybeURIRef],
                    context => Optional[JSONLDContextObj],
                    deref   => Optional[CodeRef] ]);

    my ($self, $json, $params) = $check->(@_);

    # normalize params
    my $uri     = $params->{uri};
    my $deref   = $params->{deref} || $self->deref;
    my $context = $params->{context} ||
        JSON::LD::RDFa::Context->new(base => $uri, deref => $deref);

    # clone only when a live context is supplied
    my %p = (clone => 1, merge => 1); #!$params->{context});
    # override these in case a context was supplied
    $p{base}  = $uri if $uri;
    $p{deref} = $params->{deref} if $params->{deref};
    my @graph = $context->expand($json, %p);

    # XXX here we want the context to merge but in a way that
    # accumulates, not overwrites its contents

    #warn $context->language;
    warn Data::Dumper::Dumper(\@graph);

    # now generate the document

    # okay first off i don't know what to do with multiple resources
    # (yet)

    # second i don't know what to do with named graphs since rdfa
    # doesn't support them

    my $ns = $context->ns;

    my %outp = (ns => $ns, uri => $uri, vocab => $context->vocab,
                lang => $context->language);
    if (my $s = $graph[0]) {
        my @types = sort map { $ns->abbreviate($_) } @{$s->{'@type'} || []};
        $outp{attr} = { typeof => \@types } if @types;
        $outp{uri} = $s->{'@id'} if !defined $outp{uri} and defined $s->{'@id'};

        # now we construct the page like the mockup we did
    }

    # anyway
    my ($body, $doc) = $self->_XHTML(%outp);

    $doc;
}

sub _get_label {
    my ($self, $predicate, @candidates) = @_;
    @candidates = @{$candidates[0]} if ref $candidates[0] eq 'ARRAY';

    # discard all resources

    # partition into language vs type (plain is xsd:string per rdf 1.1)

    # datatypes other than xsd:string get priority

    # sort the language nodes by conneg (if there are any)

    # 
}

=head1 SEE ALSO

=over 4

=item

L<JSON>

=item

L<Role::Markup::XML>

=back

=head1 AUTHOR

Dorian Taylor, C<< <dorian at cpan.org> >>

=head1 BUGS

Please report bugs to L<the issues section on the GitHub
repository|https://github.com/doriantaylor/p5-json-ld-rdfa/issues>.

=head1 LICENSE AND COPYRIGHT

Copyright 2018 Dorian Taylor.

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at
L<http://www.apache.org/licenses/LICENSE-2.0> .

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

=cut

1; # End of JSON::LD::RDFa
