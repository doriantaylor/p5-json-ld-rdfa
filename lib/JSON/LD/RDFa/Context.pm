package JSON::LD::RDFa::Context;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Moo;

use Type::Params    qw(compile Invocant);
use Types::Standard qw(slurpy Any Maybe Optional ArrayRef HashRef CodeRef
                       Dict Bool);

use JSON::LD::RDFa::Error;
use JSON::LD::RDFa::Types qw(URIRef MaybeURIRef MaybeLang
                             JSONLDVersion JSONLDContexts NamespaceMap);

use JSON              ();
use Clone             ();
use Scalar::Util      ();
use URI::NamespaceMap ();

=head1 NAME

JSON::LD::RDFa::Context - A JSON-LD context

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

  my $context = JSON::LD::RDFa::Context->parse($context);

=head1 DESCRIPTION

This is a general-purpose JSON-LD context. It resides in this
namespace for now, even though it could easily be called
C<JSON::LD::Context>. It may indeed be called that one day.

=head1 METHODS

=head2 new

=over 4

=item base

The base URI, if set.

=cut

has base => (
    is     => 'rw',
    isa    => MaybeURIRef,
    coerce => 1,
);

=item vocab

The default vocabulary, if set.

=cut

has vocab => (
    is     => 'rw',
    isa    => MaybeURIRef,
    coerce => 1,
);

=item language

The language, if set.

=cut

has language => (
    is     => 'rw',
    isa    => MaybeLang,
    coerce => 1,
);

=item version

The JSON-LD version, either 1.0 or 1.1.

=cut

has version => (
    is     => 'rw',
    isa    => JSONLDVersion,
    coerce => 1,
);

=item ns

This is a conventional L<URI::NamespaceMap>.

=cut

has ns => (
    is      => 'ro',
    isa     => NamespaceMap,
    coerce  => 1,
    default => sub { URI::NamespaceMap->new },
);

=item terms

These are any terms that are not namespaces.

=cut

has terms => (
    is      => 'rwp',
    isa     => HashRef,
    default => sub { {} },
);

=item deref

A subroutine reference that takes a URI as input and returns either a
valid JSON-LD string or already-parsed object.

=cut

# a subroutine which will take a URI and produce JSON-LD
sub _no_deref {
    JSON::LD::RDFa::Error->throw(
        'This instance doesn\'t support remote contexts!');
}

has deref => (
    is      => 'ro',
    isa     => CodeRef,
    default => sub { \&_no_deref },
);

# to prevent loops, stash remote contexts which have already been seen
has _seen => (
    is       => 'ro',
    isa      => HashRef,
    default  => sub { {} },
);

=head2 process $CONTEXT [, %ETC]

Process and return a new context object. This method can be called
either as a constructor or as an instance method. When called as an
instance method, it will return a new context merged with the
existing instance. All internal members will also be cloned.

Note that C<$CONTEXT> here is assumed to be JSON which has already
been parsed: either a C<HASH> reference, a scalar representing a
remote context URI (or L<URI> object proper), or an C<ARRAY> reference
containing some quantity of the former two.

In addition to the JSON-LD content, there are three key-value
parameters:

=over 4

=item base

Supply a base URI if one isn't defined in the context's payload. This
would be, for instance, the URI from which the JSON-LD document was
retrieved. This can be a string or L<URI> object (it will be coerced).

=item deref

Supply a C<CODE> reference to retrieve remote contexts, which this
module does not do by itself. What follows is an extremely naïve
example of the kind of code which is expected to do the job:

  sub deref {
      my $self = shift;
      my $resp = LWP::UserAgent->new->get(shift);

      if ($resp->is_success) {
          # see the relevant doc for this way of handling redirects
          $self->mark_assert($resp->request->uri) if $resp->previous;

          # return value will be parsed but you may want to check that
          # it's JSON...
          return $resp->content;
      }

      # otherwise undef
  }

The function is called as if it is an instance method of the context,
which gives it access to members like L</

=item clone

Deep-clone the object's members. Defaults to I<true>. Set it to a
false value to reuse the same ones.

=back

=cut

sub _maybe_clone {
    my ($thing, $clone) = @_;
    return $thing unless $clone;
    return $thing->clone
        if Scalar::Util::blessed($thing) and $thing->can('clone');
    Clone::clone($thing);
}

# https://json-ld.org/spec/latest/json-ld-api/#context-processing-algorithm
sub process {
    # one-time compilation of input sanitation function
    state $check = compile(Invocant, JSONLDContexts, slurpy Dict[
        base  => Optional[MaybeURIRef],
        deref => Optional[CodeRef],
        mode  => Optional[JSONLDVersion],
        clone => Optional[Bool],
        slurpy Any]);

    # sanitize input
    my ($self, $ctxs, $opts) = $check->(@_);

    # deal with optional parameters
    my $base  = $opts->{base};
    my $deref = $opts->{deref};
    my $clone = exists($opts->{clone}) ? $opts->{clone} : ($opts->{clone} = 1);

    # the only special case is when this thing is called as a
    # constructor; otherwise if we just call it as an instance method
    # in a tail recursion it should pretty much do what we want

    my $class = ref $self || $self;
    my %p;

    # if this is called from an instance, clone all the existing bits
    if (ref $self) {
        # the basics
        $p{base}     = _maybe_clone($self->base,  $clone) if $self->base;
        $p{vocab}    = _maybe_clone($self->vocab, $clone) if $self->vocab;
        $p{language} = $self->language if $self->language;
        $p{version}  = $self->version  if $self->version;
        $p{deref}    = $self->deref    if $self->deref;

        # deal with the namespace map
        $p{ns} = $self->ns;
        if ($clone) {
            my $ns = $p{ns} = URI::NamespaceMap->new;
            while (my ($k, $v) = $self->ns->each_map) {
                $ns->add_mapping($k, $v->as_string);
            }
        }

        # deal with the remaining term map
        $p{terms} = _maybe_clone($self->terms, $clone);
    }

    # overwrite these members
    $p{base}  = $base  if $base;
    $p{deref} = $deref if $deref;

    # now we shift off the first context and process it
    my $ctx = shift @$ctxs or return $class->new(%p);

    # dereference the context if it is remote
    if (Scalar::Util::blessed($ctx) and $ctx->isa('URI')) {
        $ctx = URI->new_abs($ctx, $p{base}) if $p{base};

        # XXX lol
        JSON::LD::RDFa::Error::Unimplemented->throw('No remote contexts yet');

        # if this returns an ARRAY make sure it is unshifted onto the
        # front of the context list
    }

    # deal with base
    # deal with version
    # deal with vocab
    # deal with language

    # now we initialize the new context
    my $new = $class->new(%p);

    # parse out term definitions other than keywords

    state %kw = map { "\@$_" => 1 } qw(base version vocab language);
    my %done;
    for my $term (grep { !($kw{$_} || $done{$_}) } keys %$ctx) {
        $new->_create_term_definition($ctx, $term, \%done);
    }

    # we don't need to clone if there are other contexts as 
    @$ctxs ? $new->process($ctxs, %$opts, clone => 0) : $new;
}

# https://json-ld.org/spec/latest/json-ld-api/#create-term-definition
sub _create_term_definition {
    my ($self, $context, $term, $defined) = @_;
    $defined ||= {};
}

# https://json-ld.org/spec/latest/json-ld-api/#iri-expansion
sub _iri_expansion {
}

=head2 seen $URI [, $MARK ]

Check if a remote context URI has already been seen.

=cut

sub seen {
    state $check = compile(Invocant, URIRef, Maybe[Bool]);
    my ($self, $uri, $mark) = $check->(@_);

    # canonicalize and remove fragment
    $uri = $uri->canonical;
    $uri->fragment(undef) if $uri->can('fragment');

    my $seen = $self->_seen;
    $mark ? $seen->{$uri} = 1 : $seen->{$uri};
}

=head2 mark $URI

Mark a remote context URI as seen.

=cut

sub mark {
    $_[0]->seen($_[1], 1);
}

=head2 mark_assert $URI

Mark a remote context URI as seen, or throw an error if it has already
been seen.

=cut

sub mark_assert {
    my ($self, $uri) = @_;
    JSON::LD::RDFa::Error::Cycle->throw
          (sprintf '%s has already encountered %s', ref $self, $uri)
              if $self->seen($uri);
    $self->mark($uri);
}

=head1 SEE ALSO

=over 4

=item

L<JSON>

=item

L<JSON-LD 1.1|https://json-ld.org/spec/latest/json-ld/>

=item

L<JSON-LD 1.1 Processing Algorithms and API|https://json-ld.org/spec/latest/json-ld-api/>

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
