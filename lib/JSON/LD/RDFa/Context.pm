package JSON::LD::RDFa::Context;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Moo;

use Type::Params    qw(compile Invocant);
use Types::Standard qw(slurpy Any Maybe Optional ArrayRef HashRef CodeRef
                       Dict Bool);

use JSON::LD::RDFa::Error;
use JSON::LD::RDFa::Types qw(URIRef is_URIRef MaybeURIRef is_MaybeURIRef
                             MaybeLang NamespaceMap is_JSONLDKeyword
                             JSONLDVersion JSONLDWildCard JSONLDContexts
                             MaybeJSONLDTerm is_JSONLDContainerDef);

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
    coerce  => sub { defined $_ ? $_ : \&_no_deref },
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

sub _disassemble_context {
    my ($self, $p, $clone) = @_;
    # the basics
    $p->{base}     = _maybe_clone($self->base,  $clone) if $self->base;
    $p->{vocab}    = _maybe_clone($self->vocab, $clone) if $self->vocab;
    $p->{language} = $self->language if $self->language;
    $p->{version}  = $self->version  if $self->version;
    $p->{deref}    = $self->deref    if $self->deref;

    # deal with the namespace map
    $p->{ns} = $self->ns;
    if ($clone) {
        my $ns = $p->{ns} = URI::NamespaceMap->new;
        while (my ($k, $v) = $self->ns->each_map) {
            $ns->add_mapping($k, $v->as_string);
        }
    }

    # deal with the remaining term map
    my $terms = $p->{terms} ||= {};
    while (my ($k, $v) = each %{_maybe_clone($self->terms, $clone)}) {
        $terms->{$k} = $v;
    }

    # meh
    $p;
}

# https://json-ld.org/spec/latest/json-ld-api/#context-processing-algorithm
sub process {
    # one-time compilation of input sanitation function
    state $check = compile(Invocant, JSONLDContexts, slurpy Dict[
        base    => Optional[MaybeURIRef],
        deref   => Optional[CodeRef],
        mode    => Optional[JSONLDVersion],
        clone   => Optional[Bool],
        merge   => Optional[Bool],
        clobber => Optional[Bool],
        slurpy Any]);

    # sanitize input
    my ($self, $ctxs, $opts) = $check->(@_);

    # deal with optional parameters
    my $base  = $opts->{base};
    my $deref = $opts->{deref};
    my $clone = exists($opts->{clone}) ? $opts->{clone} : ($opts->{clone} = 1);
    my $merge = $opts->{merge};
    my $clobber = $opts->{clobber};

    # the only special case is when this thing is called as a
    # constructor; otherwise if we just call it as an instance method
    # in a tail recursion it should pretty much do what we want

    my $class = ref $self;
    my %p;

    if ($class) {
        # if this is called from an instance, clone all the existing bits
        $self->_disassemble_context(\%p, $clone);
    }
    else {
        # otherwise this is a class method
        $class = $self;
    }

    # overwrite these if applicable
    $p{base}  = $base  if $base;
    $p{deref} = $deref if $deref;

    my $out;
    for my $ctx (@$ctxs) {
        $out = $self->_process_one($ctx, %p, clone => $clone);
        if ($merge) {
            # scalars
            for my $method (qw(base vocab language version)) {
                next unless !defined($self->$method) || $clobber;
                my $val = _maybe_clone($out->$method, $clone);
                $self->$method($val) if defined $val;
            }

            # namespaces
            my $ns = $self->ns;
            while (my ($p, $u) = $out->ns->each_map) {
                next unless !$ns->namespace_uri($p) || $clobber;
                $ns->add_mapping($p, _maybe_clone($u, $clone));
            }

            # terms
            my $st = $out->terms;
            my $tt = $self->terms;
            for my $term (keys %$st) {
                next unless !$tt->{$term} || $clobber;
                $st->{$term} = _maybe_clone($st->{$term}, $clone);
            }
        }
    }

    $out;
}

sub _process_one {
    my ($self, $ctx, %p) = @_;

    my $class = ref $self || $self;
    my $clone = delete $p{clone};
    my $base  = $p{base};

    # dereference the context if it is remote
    my $is_remote;
    if (Scalar::Util::blessed($ctx)) {
        if ($ctx->isa('URI')) {
            $ctx = URI->new_abs($ctx, $p{base}) if $p{base};

            # XXX lol
            JSON::LD::RDFa::Error::Unimplemented->throw
                  ('No remote contexts yet');

            # if this returns an ARRAY make sure it is unshifted onto the
            # front of the context list
        }
        elsif ($ctx->isa(__PACKAGE__)) {
            $ctx->_disassemble_context(\%p, $clone);
            return $class->new(%p);
        }
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
    my %kw = map { '@' . $_ => 1 } qw(base version vocab language);

    my %done;
    for my $term (grep { !$kw{$_} } keys %$ctx) {
        next if $done{$term}; # done gets updated in situ
        $new->_create_term_definition($ctx, $term, \%done);
    }

    $new;
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
    my %kw = map { '@' . $_ => 1 }
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
        $type = URI->new($type) if defined $type and $type =~ /:/;
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
        #warn $i;
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

=head2 expand $JSON [, %PARAMS ]



=cut

# https://json-ld.org/spec/latest/json-ld-api/#expansion-algorithms

# note the algorithm proper is separate from the interface because
# there is an epilogue

sub _expansion {
    my ($self, $element, $property, $frame, $clone) = @_;
    # 5.1.2.1
    return unless defined $element;

    # 5.1.2.2
    $frame = 0 if defined $property and $property eq '@default';

    # we reuse this a lot so this saves typing
    my $ref = ref $element;

    # 5.1.2.3
    if (!$ref or Scalar::Util::blessed($element)) {
        # note we treat blessed elements as scalars
        return if !defined $property or $property eq '@graph';
        return $self->_value_expansion($element, $property);
    }

    # do this once near the top
    my %def = %{defined $property ? $self->terms->{$property} || {} : {}};

    # 5.1.2.4
    if ($ref eq 'ARRAY') {
        # 5.1.2.4.1
        my @result;
        # 5.1.2.4.2
        for my $item (@$element) {
            # 5.1.2.4.2.1
            $item = $self->_expansion($item, $property, $frame, $clone);
            # 5.1.2.4.2.2
            my $iref = ref $item || '';
            my $list = $property and
                ($property eq '@list'
                     or grep { $_ eq '@list' } @{$def{container} || []});
            JSON::LD::RDFa::Error::Invalid->throw('list of lists')
                  if $list and
                      ($iref eq 'ARRAY' or
                           ($iref eq 'HASH' and exists $item->{'@list'}));
            # 5.1.2.4.2.3
            push @result, ($iref eq 'ARRAY' ? @$item : $item) if defined $item;
        }
        # 5.1.2.4.3
        return \@result;
    }

    # 5.1.2.5
    JSON::LD::RDFa::Error::Invalid->throw("Element $ref must be a HASH")
          unless $ref eq 'HASH';

    # 5.1.2.6
    if (exists $element->{'@context'}) {
        $self = $self->process($element->{'@context'}, clone => 0);
        # redefine def shorthand
        %def = %{defined $property ? $self->terms->{$property} || {} : {}};
    }

    # 5.1.2.7 this is confusing af
    for my $key (sort keys %$element) {
        next unless $self->_iri_expansion($key, vocab => 1) eq '@type';
        my $val = $element->{$key};
        my $vr  = ref $val || '';
        next unless !$vr or $vr eq 'ARRAY' or Scalar::Util::blessed($val);

        # 5.1.2.7.1
        for my $type (sort @{$vr eq 'ARRAY' ? $val : [$val]}) {
            next unless my $d = $self->terms->{$type};
            next unless $d->{context};
            $self = $self->process($d->{context});
        }
        # redefine def shorthand
        %def = %{defined $property ? $self->terms->{$property} || {} : {}};
    }

    # 5.1.2.8, 5.1.2.9
    my $result = $self->_expand_dict($element, $property, $frame, $clone);

    # 5.1.2.10
    if (exists $result->{'@value'}) {
        # 5.1.2.10.1
        my $bad = (exists $result->{'@type'} and exists $result->{'@language'});
        unless ($bad) {
            my %tmp = %$result;
            map { delete $tmp{$_} } qw(@value @language @type @index);
            $bad = 1 if keys %tmp;
        }
        JSON::LD::RDFa::Error::Invalid->throw('invalid value object') if $bad;

        # 5.1.2.10.2
        if (!defined $result->{'@value'}) {
            # XXX we could return here since no additional processing occurs
            undef $result;
            return;
            # the spec says set to null but that will make it hard to deal
            # with. really the spec should just instruct to return null at
            # 10.2, 13, 14.1 and 14.2, since it makes no sense to process
            # any farther. nevertheless we will follow the spec to the
            # letter.
        }
        # 5.1.2.10.3
        elsif (exists $result->{'@language'} and
                   not _is_quasi_scalar($result->{'@value'})) {
            JSON::LD::RDFa::Error::Invalid->throw
                  ('invalid language-tagged value');
        }
        # 5.1.2.10.4
        elsif (defined $result->{'@type'}
                   and not _is_quasi_scalar($result->{'@type'})) {
            #warn Data::Dumper::Dumper($result);
            JSON::LD::RDFa::Error::Invalid->throw('invalid typed value');
        }
    }
    # 5.1.2.11
    elsif (exists $result->{'@type'} and ref $result->{'@type'} ne 'ARRAY') {
        $result->{'@type'} = [$result->{'@type'}];
    }
    # 5.1.2.12
    elsif (exists $result->{'@set'} or exists $result->{'@list'}) {
        # 5.1.2.12.1
        my $sk = scalar keys %$result;
        JSON::LD::RDFa::Error::Invalid->throw('invalid set or list object')
              unless $sk == 1 or $sk == 2 and exists $result->{'@index'};
        # 5.1.2.12.2

        # XXX THIS COULD BE AN ARRAYREF OR UNDEF OR SOMETHING ELSE
        $result = $result->{'@set'} if exists $result->{'@set'};
        # you could actually return from here
    }
    # 5.1.2.13
    elsif (keys %$result == 1 and exists $result->{'@language'}) {
        undef $result;
    }
    # 5.1.2.14
    elsif ((!defined $property or $property eq '@graph')
               and ref $result eq 'HASH') {
        my $sk = keys %$result;
        # 5.1.2.14.1, 5.1.2.14.2
        undef $result if $sk == 0 or exists $result->{'@value'}
                or exists $result->{'@list'}
                    or (!$frame and $sk == 1 and exists $result->{'@id'});
    }

    # 5.1.2.15
    return $result;
}

# https://json-ld.org/spec/FCGS/json-ld-api/20180607/#alg-expand-each-key-value
# https://json-ld.org/spec/latest/json-ld-api/#alg-expand-each-key-value
# this is hairy and it recurses so it should be separated out
sub _expand_dict {
    my ($self, $element, $property, $frame, $clone, $result) = @_;
    $frame  ||= 0;
    $result ||= {};

    my @nests;

    # 5.1.2.9
    for my $key (sort keys %$element) {
        my $kdef  = $self->terms->{$key} || {};
        my $value = $element->{$key};
        # 5.1.2.9.1
        next if $key eq '@context';
        # 5.1.2.9.2
        my $expkey = $self->_iri_expansion($key, vocab => 1);
        # 5.2.1.9.3
        next unless defined $expkey and $expkey =~ /^(@|.*:)/;
        # 5.2.1.9.4
        my $expval;
        if ($expkey =~ /^@/) {
            # 5.2.1.9.4.1
            JSON::LD::RDFa::Error::Invalid->throw
                  ("Invalid reverse property map: $property => $expkey")
                      if $property and $property eq '@reverse';
            # 5.1.2.9.4.2
            JSON::LD::RDFa::Error::Conflict->throw
                  ("Colliding keyword: $expkey") if exists $result->{$expkey};

            # NOTE these keys are necessarily disjoint so i would
            # normally make this a dispatch table but eh /shrug

            # 5.1.2.9.4.3
            if ($expkey eq '@id') {
                my $ok = _is_quasi_scalar($value);
                $ok ||= (_is_empty_hash($value) or is_JSONLDTerms($value))
                    if $frame;
                JSON::LD::RDFa::Error::Invalid->throw
                      ("Invalid \@id value: $value") unless $ok;

                $expval = $self->_iri_expansion($value, relative => 1);
            }

            # 5.1.2.9.4.4
            if ($expkey eq '@type') {
                my $ok = _is_quasi_scalar($value) || is_JSONLDTerms($value);
                $ok = $ok || _is_empty_hash($value) if $frame;
                JSON::LD::RDFa::Error::Invalid->throw('invalid type value')
                      unless $ok;

                # XXX this is bad. see:
                # https://github.com/json-ld/json-ld.org/issues/662

                # $expval = [$value] unless ref $value eq 'ARRAY';
                # $expval = [map { $self->_iri_expansion(
                #     $_, vocab => 1, relative => 1) } @$expval];

                # this is slightly better
                if (ref $value eq 'ARRAY') {
                    $expval = [map { $self->_iri_expansion(
                        $_, vocab => 1, relative => 1) } @$value];
                }
                else {
                    $expval = $self->_iri_expansion
                        ($value, vocab => 1, relative => 1);
                }
            }
            # 5.1.2.9.4.5
            if ($expkey eq '@graph') {
                $expval = $self->_expansion($value, '@graph', $frame, $clone);
                $expval = [$expval] unless ref $expval eq 'ARRAY';
            }
            # 5.1.2.9.4.6
            if ($expkey eq '@value') {
                # XXX add frame logic
                JSON::LD::RDFa::Error::Invalid->throw
                      ('Invalid value object value')
                          unless _is_quasi_scalar($value);
                unless (defined ($expval = $value)) {
                    $result->{'@value'} = undef;
                    next;
                }
            }
            # 5.1.2.9.4.7
            if ($expkey eq '@language') {
                # XXX add frame logic
                JSON::LD::RDFa::Error::Invalid->throw
                      ('Invalid language-tagged string')
                          unless _is_quasi_scalar($value);
                # XXX this is wrong!!!
                #$expval = ref $value eq 'ARRAY' ? $value : [$value];
                $expval = $value;
            }
            # 5.1.2.9.4.8
            if ($expkey eq '@index') {
                JSON::LD::RDFa::Error::Invalid->throw
                      ('Invalid language-tagged string')
                          unless _is_quasi_scalar($value);
                $expval = ref $value eq 'ARRAY' ? $value : [$value];
            }
            # 5.1.2.9.4.9
            if ($expkey eq '@list') {
                # 5.1.2.9.4.9.1
                next if !defined $property or $property eq '@graph';
                # 5.1.2.9.4.9.2
                $expval = $self->_expansion($value, $property, $frame, $clone);
                # 5.1.2.9.4.9.3
                JSON::LD::RDFa::Error::Conflict->throw('list of lists')
                      if _is_list_object($expval);
            }
            # 5.1.2.9.4.10
            $expval = $self->_expansion($value, $property, $frame, $clone)
                if $expkey eq '@set';
            # 5.1.2.9.4.11
            if ($expkey eq '@reverse') {
                JSON::LD::RDFa::Error::Invalid->throw('Invalid @reverse value')
                      unless ref $value eq 'HASH';
                # 5.1.2.9.4.11.1
                $expval = $self->_expansion($value, '@reverse', $frame, $clone);
                # 5.1.2.9.4.11.2
                my $rev = $expval->{'@reverse'};
                if ($rev) {
                    # XXX this might not be a dictionary
                    while (my ($k, $v) = each %$rev) {
                        # 5.1.2.9.4.11.2.1
                        my $x = $result->{$k} ||= [];
                        # 5.1.2.9.4.11.2.2
                        push @$x, $v;
                    }
                }
                if ($rev and keys %$expval > 1 or !$rev and keys %$expval) {
                    # 5.1.2.9.4.11.3.1, 5.1.2.9.4.11.3.2
                    my $revmap = $result->{'@reverse'} ||= {};
                    # 5.1.2.9.4.11.3.3
                    while (my ($p, $items) = each %$rev) {
                        next if $p eq '@reverse';
                        # 5.1.2.9.4.11.3.3.1
                        for my $i (@$items) {
                            # 5.1.2.9.4.11.3.3.1.1
                            JSON::LD::RDFa::Error::Invalid->throw
                                  ('invalid reverse property value')
                                      if _is_value_object($i)
                                          or _is_list_object($i);
                            # 5.1.2.9.4.11.3.3.1.2
                            my $x = $revmap->{$p} ||= [];
                            # 5.1.2.9.4.11.3.3.1.3
                            push @$x, $i;
                        }
                    }
                }
                # 5.1.2.9.4.11.4
                next;
            }
            # 5.1.2.9.4.12
            if ($expkey eq '@nest') {
                push @nests, $key;
                next;
            }
            # 5.1.2.9.4.13
            if ($frame and is_JSONLDFrameKeyword($expkey)) {
                $expval = $self->_expansion($value, $property, $frame, $clone);
            }
            # 5.1.2.9.4.14
            $result->{$expkey} = $expval if defined $expval;
            # 5.1.2.9.4.15
            next;
        }
        # 5.1.2.9.5
        my $termctx = $kdef->{context} || $self;
        # 5.1.2.9.6
        my %ctrmap  = map { $_ => 1 } @{$kdef->{container} || []};
        # 5.1.2.9.7
        if ($ctrmap{'@language'} and ref $value eq 'HASH') {
            # 5.1.2.9.7.1
            $expval = [];
            # 5.1.2.9.7.2
            for my $lang (sort keys %$value) {
                my $lval = $value->{$lang};
                # 5.1.2.9.7.2.1
                $lval = [$lval] unless ref $lval eq 'ARRAY';
                # 5.1.2.9.7.2.2
                for my $i (@$lval) {
                    # 5.1.2.9.7.2.2.1
                    JSON::LD::RDFa::Error::Invalid->throw
                          ('invalid language map value')
                              unless !defined $i or _is_quasi_scalar($i);
                    # 5.1.2.9.7.2.2.2
                    if (defined $i) {
                        my %v = ('@value' => $i);
                        $lang = $termctx->_iri_expansion($lang);
                        $v{'@language'} = $lang if $lang and $lang ne '@none';
                        push @$expval, \%v;
                    }
                }
            }
        }
        # 5.1.2.9.8
        elsif (grep { $ctrmap{$_} } qw(@index @type @id)
                   and ref $value eq 'HASH') {
            # 5.1.2.9.8.1
            $expval = [];
            # 5.1.2.9.8.2
            for my $index (sort keys %$value) {
                my $ival = $value->{$index};
                my %idef = %{$termctx->terms->{$index} || {}};
                # 5.1.2.9.8.2.1
                my $mapctx = $ctrmap{'@type'} && $idef{context}
                    ? $termctx->process($idef{context}) : $termctx;
                # 5.1.2.9.8.2.2
                my $iexp = $mapctx->_iri_expansion($index, vocab => 1);
                # 5.1.2.9.8.2.3
                $ival = [$ival] unless ref $ival eq 'ARRAY';
                # 5.1.2.9.8.2.4
                $ival = $mapctx->_expansion($ival, $key, $frame, $clone);
                # 5.1.2.9.8.2.5
                for my $item (@$ival) {
                    # 5.1.2.9.8.2.5.1
                    if ($ctrmap{'@graph'} and !_is_graph_object($item)) {
                        my $x = ref $item eq 'ARRAY' ? $item : [$item];
                        $item = { '@graph' => $x };
                    }
                    # 5.1.2.9.8.2.5.2
                    if ($ctrmap{'@index'} and not exists $item->{'@index'}
                            and $iexp ne '@none') {
                        # XXX is the item a hashref?
                        $item->{'@index'} = $index;
                    }
                    # 5.1.2.9.8.2.5.3
                    elsif ($ctrmap{'@id'} and not exists $item->{'@id'}) {
                        $item->{'@id'} = $iexp unless $iexp eq '@none';
                    }
                    # 5.1.2.9.8.2.5.4
                    elsif ($ctrmap{'@type'}) {
                        my $t = defined $item->{'@type'}
                            ? $item->{'@type'} : [];
                        my @types = @{ref $t eq 'ARRAY' ? @$t : [$t]};
                        push @types, $iexp unless $iexp eq '@none'
                            or grep { $_ eq $iexp } @types;
                        $item->{'@type'} = \@types;
                    }
                    # 5.1.2.9.8.2.5.5
                    push @$expval, $item;
                }
            }
        }
        else {
            # 5.1.2.9.9
            $expval = $termctx->_expansion($value, $key, $frame);
        }

        # 5.1.2.9.10
        next unless defined $expval;

        # 5.1.2.9.11
        if ($ctrmap{'@list'} and not _is_list_object($expval)) {
            $expval = [$expval] unless ref $expval eq 'ARRAY';
            $expval = { '@list' => $expval };
        }

        # 5.1.2.9.12
        if ($ctrmap{'@graph'}) {
            $expval = [$expval] unless ref $expval eq 'ARRAY';
            # 5.1.2.9.12.1
            $expval = [map { [
                _is_graph_object($_) ? $_ : { '@graph' => $_ } ] } @$expval];
        }
        # 5.1.2.9.13
        elsif ($kdef->{reverse}) {
            # 5.1.2.9.13.1, 5.1.2.9.13.2
            my $revmap = $result->{'@reverse'} ||= {};
            # 5.1.2.9.13.3
            $expval = [$expval] unless ref $expval eq 'ARRAY';
            # 5.1.2.9.13.4
            for my $item (@$expval) {
                # 5.1.2.9.13.4.1
                JSON::LD::RDFa::Error::Invalid->throw
                      ('invalid reverse property value')
                          if _is_value_object($item) or _is_list_object($item);
                # 5.1.2.9.13.4.2
                my $irev = $revmap->{$expkey} ||= [];
                # 5.1.2.9.13.4.3
                push @$irev, $item;
            }
        }
        # 5.1.2.9.14
        else {
            # 5.1.2.9.14.1
            my $mem = $result->{$expkey} ||= [];
            # XXX this is not mentioned but it should probably happen
            $expval = [$expval] unless ref $expval eq 'ARRAY';
            # 5.1.2.9.14.2
            push @$mem, @$expval;
        }
    }

    # XXX the following makes no sense to have in that big loop, even
    # though that's how the spec reads.

    # 5.1.2.9.15
    for my $nkey (@nests) {
        # 5.1.2.9.15.1
        my $nv = $element->{$nkey};
        for my $nval (@{ref $nv eq 'ARRAY' ? @$nv : [$nv]}) {
            # 5.1.2.9.15.2.1
            my $ok;
            if (ref $nval eq 'HASH') {
                my %nvk = map { $self->_iri_expansion($_, vocab => 1) => 1 }
                    keys %$nval;
                $ok = 1 unless $nvk{'@value'};
            }

            JSON::LD::RDFa::Error::Invalid->throw('invalid @nest value')
                  unless $ok;
            # 5.1.2.9.15.2.2
            $self->_expand_dict($nval, $property, $frame, $clone, $result);
        }
    }

    # XXX see remedy on 5.1.2.9.4.4
    # $result->{'@type'} = [$result->{'@type'}]
    #     if defined $result->{'@type'}
    #             and not defined $result->{'@value'}
    #                 and ref $result->{'@type'} ne 'ARRAY';

    wantarray ? %$result : $result;
}

# XXX move these to ::Types?

sub _is_empty_hash {
    defined $_[0] and ref $_[0] eq 'HASH' and not keys %{$_[0]};
}

sub _is_quasi_scalar {
    defined $_[0] and (!ref $_[0] or Scalar::Util::blessed($_[0]));
}

sub _is_list_object {
    defined $_[0] and ref $_[0] eq 'HASH' and exists $_[0]{'@list'};
}

sub _is_value_object {
    defined $_[0] and ref $_[0] eq 'HASH' and exists $_[0]{'@value'};
}

sub _is_graph_object {
    defined $_[0] and ref $_[0] eq 'HASH' and exists $_[0]{'@graph'};
}

#https://json-ld.org/spec/FCGS/json-ld-api/20180607/#value-expansion
sub _value_expansion {
    # 5.2.2
    my ($self, $value, $property) = @_;

    my %def = %{$self->terms->{$property} || {}};

    my $scalar = _is_quasi_scalar($value);

    # 5.2.2.1
    if (defined $def{type} and $def{type} eq '@id' and $scalar) {
        return { '@id' => $self->_iri_expansion($value, relative => 1) };
    }
    # 5.2.2.2
    elsif (defined $def{type} and $def{type} eq '@vocab' and $scalar) {
        return {
            '@id' => $self->_iri_expansion($value, vocab => 1, relative => 1) };
    }

    # 5.2.2.3
    my $result = { '@value' => $value };
    # 5.2.2.4
    if (defined $def{type} and !grep { $def{type} } qw(@id @vocab)) {
        $result->{'@type'} = $def{type};
    }
    # 5.2.2.5
    elsif ($scalar) {
        # 5.2.2.5.1
        if (exists $def{language}) {
            $result->{'@language'} = $def{language} if defined $def{language};
        }
        # 5.2.2.5.2
        elsif (my $lang = $self->language) {
            $result->{'@language'} = $lang;
        }
    }

    # 5.2.2.6
    $result;
}

sub expand {
    state $check = Type::Params::compile(
        Invocant, JSONLDWildCard,
        slurpy Dict[base     => Optional[MaybeURIRef],
                    deref    => Optional[CodeRef],
                    clone    => Optional[Bool],
                    merge    => Optional[Bool],
                    property => Optional[MaybeJSONLDTerm],
                    frame    => Optional[Bool]]);

    my ($self, $json, $params) = $check->(@_);

    unless (ref $self) {
        my %p;
        $p{base}  = $params->{base}  if defined $params->{base};
        $p{deref} = $params->{deref} if defined $params->{deref};
        $p{clone} = $params->{clone} if defined $params->{clone};
        $self = $self->new(%p);
    }

    my $out = $self->_expansion($json, @{$params}{qw(property frame clone)});

    # 5.1.2 epilogue: make sure $out is an arrayref
    if (ref $out eq 'HASH' and keys %$out == 1 and $out->{'@graph'}) {
        $out = $out->{'@graph'};
    }
    elsif (!defined $out) {
        $out = [];
    }
    else {
        $out = [$out];
    }

    # may as well add this too
    wantarray ? @$out : $out;
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
