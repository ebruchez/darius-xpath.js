## What is this?

A port of the XPath processor of [Saxon-CE][1] to [Scala.js][2].

Only the XPath processor is ported. The full XSLT processor files are present but do not compile.

## Status

The port is not complete yet, in particular:

- types, in particular date/time types need regex support
- the `doc()` function is missing
- nothing is really tested

## Licenses

The Saxon files and their translations are under the MPL license. Other files are under the Apache 2 license.


[1]: http://www.saxonica.com/ce/index.xml
[2]: http://www.scala-js.org/