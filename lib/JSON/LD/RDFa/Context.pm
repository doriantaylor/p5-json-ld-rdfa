package JSON::LD::RDFa::Context;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Moo;

use Type::Params    qw(compile Invocant);
use Types::Standard qw(slurpy Any Maybe Optional ArrayRef HashRef CodeRef
                       Dict Bool);

use JSON::LD::RDFa::Error;
use JSON::LD::RDFa::Types qw(URIRef is_URIRef MaybeURIRef MaybeLang
                             NamespaceMap is_JSONLDKeyword JSONLDVersion
                             JSONLDContexts is_JSONLDContainerDef);

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
    my $is_remote;
    if (Scalar::Util::blessed($ctx) and $ctx->isa('URI')) {
        $ctx = URI->new_abs($ctx, $p{base}) if $p{base};

        # XXX lol
        JSON::LD::RDFa::Error::Unimplemented->throw('No remote contexts yet');

        # if this returns an ARRAY make sure it is unshifted onto the
        # front of the context list
    }

    # deal with base
    if (exists $ctx->{'@base'} and not $is_remote) {
        my $b = $ctx->{'@base'};
        # XXX mehhhhh this logic isn't very good
        if ($base) {
            $p{base} = URI->new_abs($b, $base);
        }
        else {
            JSON::LD::RDFa::Error::Invalid->throw
                  ("Invalid base URI ($b)") unless $b->scheme;
            $p{base} = _maybe_clone($b, $clone);
        }
    }

    # deal with version
    if (exists $ctx->{'@version'}) {
        my $v = $ctx->{'@version'};
        JSON::LD::RDFa::Error::Invalid->throw("Invalid version ($v)")
              unless defined $v and $v == 1.1;
        # XXX no way to transmit the API level yet but whatev
        $p{version} = 1.1;
    }

    # deal with vocab
    if (exists $ctx->{'@vocab'}) {
        JSON::LD::RDFa::Error::Invalid->throw('Invalid @vocab')
              unless is_MaybeURIRef($p{vocab} = $ctx->{'@vocab'});
        $p{vocab} = URI->new_abs($p{vocab}, $p{base})
            if (defined $p{vocab} and defined $p{base});
    }

    # deal with language
    if (exists $ctx->{'@language'}) {
        $p{language} = $ctx->{'@language'};
    }

    # now we initialize the new context
    my $new = $class->new(%p);

    # parse out term definitions other than keywords

    # "Initialization of state variables in list context currently forbidden"
    state %kw;
    %kw = map { '@' . $_ => 1 } qw(base version vocab language);

    my %done;
    for my $term (grep { !$kw{$_} } keys %$ctx) {
        next if $done{$term}; # done gets updated in situ
        $new->_create_term_definition($ctx, $term, \%done);
    }

    # we don't need to clone if there are other contexts as 
    @$ctxs ? $new->process($ctxs, %$opts, clone => 0) : $new;
}

# https://json-ld.org/spec/latest/json-ld-api/#create-term-definition
sub _create_term_definition {
    my ($self, $context, $term, $defined) = @_;
    $defined ||= {};

    # 4.2.2.1
    if (exists $defined->{$term}) {
        return if $defined->{$term};
        JSON::LD::RDFa::Error::Cycle->throw("cyclic IRI mapping: $term");
    }
    # 4.2.2.2
    $defined->{$term} = 0;

    # 4.2.2.3
    state %kw;
    %kw ||= map { '@' . $_ => 1 }
        qw(base container context graph id index language list nest none
           prefix reverse set type value version vocab);
    JSON::LD::RDFa::Error::Conflict->new("Keyword redefinition: $term")
          if $kw{$term};

    # 4.2.2.4
    my $terms = $self->terms;
    delete $terms->{$term};
    # my addition
    $self->ns->remove_mapping($term);

    # 4.2.2.5
    my $val = _maybe_clone($context->{$term}, 1);

    # 4.2.2.6
    if (!defined $val or (ref $val eq 'HASH' and exists $val->{'@id'}
                              and !defined $val->{'@id'})) {
        $terms->{$term} = undef;
        return;
    }

    # 4.2.2.7
    my $simple; # it is never explained what $simple does
    if (!ref $val or is_URIRef($val)) {
        $val = { '@id' => $val };
        $simple = 1;
    }

    # 4.2.2.8
    JSON::LD::RDFa::Error::Invalid->throw("Invalid term definition $term")
          unless ref $val eq 'HASH';

    # 4.2.2.9
    my %definition;

    # 4.2.2.10
    if (exists $val->{'@type'}) {
        my $type = $val->{'@type'};

        # 4.2.2.10.1
        JSON::LD::RDFa::Error::Invalid->throw("Invalid type mapping: $type")
              unless defined $type and (!ref $type or is_URIRef($type));

        # 4.2.2.10.2
        $type = $self->_iri_expansion
            ($type, vocab => 1, context => $context, defined => $defined);
        JSON::LD::RDFa::Error::Invalid->throw("Invalid type mapping: $type")
              unless defined $type
                  and (is_URIRef($type) or $type =~ /^\@(id|vocab)$/);

        # 4.2.2.10.3
        $definition{type} = $type;
    }

    # 4.2.2.11
    if (exists $val->{'@reverse'}) {
        # 4.2.2.11.1
        JSON::LD::RDFa::Error::Invalid->throw("Invalid reverse property: $term")
              if grep { exists $val->{$_} } qw(@id @nest);

        # 4.2.2.11.2
        my $rev = $val->{'@reverse'};
        JSON::LD::RDFa::Error::Invalid->throw("Invalid IRI mapping: $rev")
              unless defined $rev and (!ref $rev or is_URIRef($rev));

        # 4.2.2.11.3
        $rev = $self->_iri_expansion
            ($rev, vocab => 1, context => $context, defined => $defined);
        $rev = URI->new($rev) unless ref $rev;
        JSON::LD::RDFa::Error::Invalid->throw("Invalid IRI mapping: $rev")
              unless defined $rev and $rev =~ /:/;
        $definition{iri} = $rev;

        # 4.2.2.11.4
        if (exists $val->{'@container'}) {
            my $c = $definition{container} = $val->{'@container'};
            JSON::LD::RDFa::Error::Invalid->throw
                  ("Invalid container mapping: $c")
                      unless !defined $c or $c =~ /^\@(set|index)$/;
        }

        # 4.2.2.11.5
        $definition{reverse} = 1;

        # 4.2.2.11.6
        $terms->{$term} = \%definition;
        $defined->{$term} = 1;
        return;
    }

    # 4.2.2.12
    $definition{reverse} = 0;

    # 4.2.2.13
    if (exists $val->{'@id'} and $val->{'@id'} ne $term) {
        my $id = $val->{'@id'};

        # 4.2.2.13.1
        JSON::LD::RDFa::Error::Invalid->throw("Invalid IRI mapping: $id")
              unless !ref $id or is_URIRef($id);

        # 4.2.2.13.2
        $id = $self->_iri_expansion
            ($id, vocab => 1, context => $context, defined => $defined);
        $id = URI->new($id) unless ref $id;
        JSON::LD::RDFa::Error::Invalid->throw("Invalid IRI mapping: $id")
              unless is_JSONLDKeyword($id) or is_URIRef($id);
        JSON::LD::RDFa::Error::Invalid->throw('Invalid keyword alias: @context')
              if $id eq '@context';
        $definition{iri} = $id;

        # 4.2.2.13.3
        $simple = 1 unless $term =~ /:/;
        if ($id =~ /[:\/?#\[\]@]$/) {
            $definition{prefix} = 1;
            # this is my addition
            $self->ns->add_mapping($term => $id);
        }
    }
    # 4.2.2.14 (not sure if this is an elsif)
    elsif ($term =~ /^(.*?):(.*?)$/) {
        # narrator: it was an elsif
        my ($p, $s) = ($1, $2);
        # 4.2.2.14.1
        $self->_create_term_definition($context, $p, $defined)
            if $s !~ m!/! and exists $context->{$p};
        # 4.2.2.14.2
        if (my $i = $terms->{$p}{iri}) {
            $definition{iri} = URI->new($i . $s);
        }
        else {
            # 4.2.2.14.3
            $definition{iri} = URI->new($term);
        }
    }
    else {
        # 4.2.2.15
        JSON::LD::RDFa::Error::Invalid->throw("Invalid IRI mapping: $term")
              unless $self->vocab;
        $definition{iri} = URI->new_abs($term, $self->vocab);
    }

    # 4.2.2.16
    if (exists $val->{'@container'}) {
        my $c = $val->{'@container'};
        $c = [$c] unless ref $c;

        # 4.2.2.16.1 lol go look at JSONLDContainerDef
        JSON::LD::RDFa::Error::Invalid->throw('Invalid container mapping')
              unless ref $c eq 'ARRAY' and is_JSONLDContainerDef($c);

        # 4.2.2.16.2
        JSON::LD::RDFa::Error::Invalid->throw('Invalid container mapping')
              if $self->version == 1.0
                  and (@$c != 1 or $c->[0] !~ /^\@(graph|id|type)$/);
        # 4.2.2.16.3
        $definition{container} = $c;
    }

    # 4.2.2.17
    if (exists $val->{'@context'}) {
        # 4.2.2.17.1
        JSON::LD::RDFa::Error::Invalid->throw('Invalid term definition')
              if $self->version == 1.0;
        # 4.2.2.17.2
        my $ctx = $val->{'@context'};
        try {
            # 4.2.2.17.3
            $ctx = $self->process($ctx, clone => 1);
        } catch {
            # rethrow
            JSON::LD::RDFa::Error::Invalid->throw
                  ("Invalid scoped context: $_");
        };
        # 4.2.2.17.4
        $definition{context} = $ctx;
    }

    # 4.2.2.18
    if (exists $val->{'@language'}) {
        # 4.2.2.18.1
        my $lang = $val->{'@language'};
        JSON::LD::RDFa::Error::Invalid->throw("Invalid language mapping: $lang")
              unless !defined $lang or !ref $lang;
        # 4.2.2.18.1
        $lang = lc $lang if defined $lang;
        $definition{language} = $lang;
    }

    # 4.2.2.19
    if (exists $val->{'@nest'}) {
        my $n = $val->{'@nest'};
        # 4.2.2.19.1
        JSON::LD::RDFa::Error::Invalid->throw('Invalid term definition')
              if $self->version == 1.0;
        # 4.2.2.19.2 NOTE the doc says 'defined' not 'definition; this
        # is almost certainly a mistake
        JSON::LD::RDFa::Error::Invalid->throw("Invalid \@nest value: $n")
              if !defined $n or ref $n or ($n =~ /^@(.*?)$/ and $1 ne 'nest');
        $definition{nest} = $n;
    }

    # 4.2.2.20
    if (exists $val->{'@prefix'}) {
        # 4.2.2.20.1
        JSON::LD::RDFa::Error::Invalid->throw('Invalid term definition')
              if $self->version == 1.0 or $term =~ /:/;
        my $p = $val->{'@prefix'};
        # 4.2.2.20.2
        JSON::LD::RDFa::Error::Invalid->throw("Invalid \@prefix value: $p")
              unless defined $p and ($p =~ /^(0|1)$/ or JSON::is_bool($p));
        $p = $definition{prefix} = 0 + $p; # de-boolean this

        # this is my addition
        $self->ns->add_mapping($term, $definition{iri})
            if $p and $definition{iri};
    }

    # 4.2.2.21
    JSON::LD::RDFa::Error::Invalid->throw("Invalid term definition: $term")
          if grep {
              $_ !~ /^\@(id|reverse|container|context|nest|prefix|type)$/
          } keys %$val;

    # 4.2.2.22
    $terms->{$term} = \%definition;
    $defined->{$term} = 1;

    return;
}

# https://json-ld.org/spec/latest/json-ld-api/#iri-expansion
sub _iri_expansion {
    my ($self, $value, %opts) = @_;
    my $def = $opts{defined} ||= {};
    # 4.3.2.1
    return $value if !defined $value or $value =~ /^\@/;
    # 4.3.2.2
    my $ctx = $opts{context};
    $self->_create_term_definition($ctx, $value, $def)
        if $ctx and $ctx->{$value} and !$def->{$value};

    my $terms = $self->terms;
    if (my $t = $terms->{$value}) {
        # 4.3.2.3, 4.3.2.4
        my $i = $t->{iri};
        return $i if defined $i and ($i =~ /^@/ or $opts{vocab});
    }

    # 4.3.2.5
    if ($value =~ /^(.*?):(.*?)$/) {
        # 4.3.2.5.1
        my ($p, $s) = ($1, $2);
        # 4.3.2.5.2
        return $value if $p eq '_' or $s =~ m!^//!;
        # 4.3.2.5.3
        $self->_create_term_definition($ctx, $p, $def)
            if $ctx and $ctx->{$p} and !$def->{$p};
        # 4.3.2.5.4
        return URI->new($terms->{$p}{iri} . $s)
            if $terms->{$p} and $terms->{$p}{iri};
        # 4.3.2.5.5
        return $value;
    }

    # 4.3.2.6
    return URI->new($self->vocab . $value) if $self->vocab and $opts{vocab};

    # 4.3.2.7
    return URI->new_abs($value, $self->base) if $self->base and $opts{relative};

    # 4.3.2.8
    $value;
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
