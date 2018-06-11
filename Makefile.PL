use 5.006;
use strict;
use warnings;
use inc::Module::Install;

name     'JSON-LD-RDFa';
all_from 'lib/JSON/LD/RDFa.pm';
author   q{Dorian Taylor <dorian@cpan.org>};
license  'apache';

perl_version '5.006';

tests_recursive('t');

resources (
   #homepage   => 'http://yourwebsitehere.com',
   #IRC        => 'irc://irc.perl.org/#JSON-LD-RDFa',
   license    => 'http://www.apache.org/licenses/LICENSE-2.0',
   #repository => 'git://github.com/Dorian Taylor/JSON-LD-RDFa.git',
   #repository => 'https://bitbucket.org/Dorian Taylor/JSON-LD-RDFa',
   bugtracker => 'http://rt.cpan.org/NoAuth/Bugs.html?Dist=JSON-LD-RDFa',
);

configure_requires (
   'Module::Install' => '0',
);

build_requires (
   'Test::More' => '0',
);

requires (
   #'ABC'              => '1.6',
   #'Foo::Bar::Module' => '5.0401',
);

install_as_cpan;
auto_install;
WriteAll;
