package JSON::LD::RDFa::Context;

use 5.012;
use strict;
use warnings FATAL => 'all';

use Moo;
use JSON;

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

=head2 function1

=cut

sub function1 {
}

=head2 function2

=cut

sub function2 {
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
