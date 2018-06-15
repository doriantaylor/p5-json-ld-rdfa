# JSON::LD::RDFa - Turn JSON-LD into (X)HTML+RDFa

## Rationale

The main motivation behind this module is to separate the concerns of
rendering _only_ the semantic content of a Web resource, and rendering
a Web page. In your Web app, you write your response handler code to
produce JSON-LD (or, rather, the JSON-LD
[internal representation](https://json-ld.org/spec/latest/json-ld/#dfn-internal-representation)),
and this module embeds it into (X)HTML+RDFa.

A deeper observation, perhaps, is that like (X|HT)ML, JSON-LD is an
(at least partially) ordered tree. This structure can be "borrowed" to
eliminate many of the ambiguities associated with rendering RDF data.
Indeed, the goal of this module is to render a consistent, isomorphic
markup structure for a given JSON-LD input, and provide the ability to
tune that structure enough to produce high-enough quality markup to be
viewed directly for the purpose of development, as well as re-ingested
and further manipulated downstream.

## Copyright & License

Copyright (C) 2018 Dorian Taylor

Licensed under the Apache License, Version 2.0 (the "License"); you
may not use this file except in compliance with the License. You may
obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0 .

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
implied.  See the License for the specific language governing
permissions and limitations under the License.

