## What is this?

A port of the XPath processor of [Saxon-CE][1] to [Scala.js][2].

Only the XPath processor is ported. The full XSLT processor files are present but do not compile.

## Try it out

The demo app is [here][2].

## Status

The port is not complete yet, in particular:

- some regex support is missing so types like date/time don't work
- the `doc()` function is missing
- nothing is really tested

This should be considered a demo, nothing more for now.

## Building and running locally

Use `sbt fastOptJS` and `sbt fullOptJS`, then load `index-dev.html` or `index.html`.

## Licenses

The Saxon files and their translations are under the MPL license. Other files are under the Apache 2 license.

[1]: http://www.saxonica.com/ce/index.xml
[2]: http://ebruchez.github.io/saxon.scala.js/
[3]: http://www.scala-js.org/