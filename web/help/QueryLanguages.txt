---+ RDF Query Languages

ClioPatria supports two query languages:
[[SPARQL][http://www.w3.org/TR/rdf-sparql-query/]] and
[[SeRQL][http://www.openrdf.org]. For both languages we provide an
interactive service that presents the results as a human-readable HTML
table and an API that presents its result as RDF/XML or XML according to
the HTTP protocol definition for the query language.

ClioPatria's SPARQL endpoint is intended to support clients that are
designed for to work with a SPARQL endpoint. E.g., if an application
requires a vocabulary browser and such a browser is available as a
JavaScript library that formulates SPARQL queries, ClioPatria's SPARQL
endpoints allow reuse of this component. In our experience, SPARQL often
needs additional application logic that is located near the data to
provide a task-specific API that drives the user interface. Locating
this logic near the data is required to avoid protocol and latency
overhead.  RDF-based application logic is a perfect match for Prolog and
the RDF data is much easier queried through the Prolog RDF libraries
than through SPARQL.

For both SPARQL and SeRQL, queries are translated to a complex Prolog
goal calling rdf/3 to resolve edges in the graph and calls to predicates
from rdfql/rdfql_runtime.pl that realise constraints imposed by the
SeRQL =WHERE= clause and SPARQL =FILTER= clauses.

---++ SPARQL Support

SPARQL support is based on the SPARQL specification, versioned April 6,
2006.  Status:

    * No optimization of literal operations.  Because many SPARQL
    operations require type-translations that depend on the literal
    found, many of these operations are slow.  The engine
    does optimize graph expressions.
    * Incomplete value-testing, notably on xsd:dateTime
    * Incomplete ORDER BY support.  Only ascending and all values
    are compared lexically.
    * Passes current test-suite, except tests affected by the above
    or acknowledged as errornous.

---++ SeRQL Support

SeRQL support and compatibility is based on development version
20040820, with additional support for the new 1.2 syntax and some of the
built-in functions. Both SeRQL and the HTTP API are fully defined in the
Sesame documentation.
