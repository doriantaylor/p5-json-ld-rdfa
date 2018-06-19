package JSON::LD::RDFa::Error;

use strict;
use warnings FATAL => 'all';

use Moo;

extends 'Throwable::Error';

# these are from https://json-ld.org/spec/latest/json-ld-api/#jsonlderrorcode
our @ERRORS = (
    'colliding keywords',
    'compaction to list of lists',
    'conflicting indexes',
    'cyclic IRI mapping',
    'invalid @id value',
    'invalid @index value',
    'invalid @nest value',
    'invalid @prefix value',
    'invalid @reverse value',
    'invalid @version value',
    'invalid base IRI',
    'invalid container mapping',
    'invalid default language',
    'invalid IRI mapping',
    'invalid keyword alias',
    'invalid language map value',
    'invalid language mapping',
    'invalid language-tagged string',
    'invalid language-tagged value',
    'invalid local context',
    'invalid remote context',
    'invalid reverse property',
    'invalid reverse property map',
    'invalid reverse property value',
    'invalid scoped context',
    'invalid set or list object',
    'invalid term definition',
    'invalid type mapping',
    'invalid type value',
    'invalid typed value',
    'invalid value object',
    'invalid value object value',
    'invalid vocab mapping',
    'keyword redefinition',
    'list of lists',
    'loading document failed',
    'loading remote context failed',
    'multiple context link headers',
    'processing mode conflict',
    'recursive context inclusion'
);

# haven't decided what to do with em yet so i'll just do this for now

package JSON::LD::RDFa::Error::Cycle;

use Moo;
extends 'JSON::LD::RDFa::Error';

use Moo;
package JSON::LD::RDFa::Error::Conflict;

use Moo;
extends 'JSON::LD::RDFa::Error';

package JSON::LD::RDFa::Error::Invalid;

use Moo;
extends 'JSON::LD::RDFa::Error';

package JSON::LD::RDFa::Error::Unimplemented;

use Moo;
extends 'JSON::LD::RDFa::Error';

1;
