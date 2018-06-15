package JSON::LD::RDFa::Context;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Moo;
use JSON;
use Scalar::Util ();

use Type::Params    qw(compile Invocant);
use Types::Standard qw(slurpy Any Maybe Optional ArrayRef HashRef CodeRef
                       Dict Bool);

use JSON::LD::RDFa::Error;
use JSON::LD::RDFa::Types qw(URIRef MaybeURIRef MaybeLang
                             JSONLDVersion JSONLDContexts NamespaceMap);

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
    init_arg => undef,
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

sub process {
    state $check = compile(Invocant, JSONLDContexts, slurpy Dict[
        base  => Optional[MaybeURIRef],
        deref => Optional[CodeRef],
        clone => Optional[Bool],
        slurpy Any]);

    my ($self, $ctxs, $opts) = $check->(@_);
    $opts->{clone} = 1 unless exists $opts->{clone};

    require Data::Dumper;
    warn Data::Dumper::Dumper($opts);

    # the only special case is when this thing is called as a
    # constructor; otherwise if we just call it as an instance method
    # in a tail recursion it should pretty much do what we want

    my $class = ref $self || $self;
    my %p;

    if (ref $self) {
    }

    $class->new(%p);
}

=head2 seen $URI [, $MARK ]

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

=cut

sub mark {
    $_[0]->seen($_[1], 1);
}

=head2 mark_assert $URI

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
