use 5.006;
use strict;
use warnings;
use inc::Module::Install;

name     'JSON-LD-RDFa';
all_from 'lib/JSON/LD/RDFa.pm';
author   q{Dorian Taylor <dorian@cpan.org>};
license  'apache';

perl_version '5.012';

tests_recursive('t');

resources (
    license    => 'http://www.apache.org/licenses/LICENSE-2.0',
    homepage   => 'https://github.com/doriantaylor/p5-json-ld-rdfa',
    repository => 'git://git@github.com:doriantaylor/p5-json-ld-rdfa.git',
    bugtracker => 'https://github.com/doriantaylor/p5-json-ld-rdfa/issues',
);

configure_requires (
   'Module::Install' => '0',
);

build_requires (
   'Test::More' => '0',
);

requires (
    'Moo'               => '2.003004',
    'Type::Tiny'        => '1.002002',
    'JSON'              => '2.97001',
    'Role::Markup::XML' => '0.08',
);

install_as_cpan;
auto_install;
WriteAll;
