# jsonpath-generator

Generate (a subset of) all valid jsonpath expressions from a given json object.

For now, it supports a subset of jsonpath composed of
* indexing operators, including wildcards
* field selection operators

## Usage

The project uses Scala/SBT.


```
sbt run <path-to-some-file.json>
```
