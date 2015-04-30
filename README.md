## What is Darius XPath?

An XPath 2.0 processor based on [Saxon-CE][1] running in the Web browser via [Scala.js][2].

Only the XPath processor is ported. The full XSLT processor files are present but do not compile.

## Try it out

Check the [demo app][3]. It does the following:

- uses a Web worker to perform XML parsing, XPath compilation, and XPath execution
- XML parsing in the worker is done using [Darius XML][4]

## Status

The port is not complete yet, in particular:

- some regex support is missing so types like date/time don't work
- the `doc()` function is missing
- nothing is really tested

This should be considered a demo, nothing more for now.

## Building and running locally

Use `sbt fastOptJSCombine` and `sbt fullOptJSCombine`, then load `index-dev.html` or `index.html`.

## Licenses

The Saxon files and their translations are under the MPL license. Other files are under the Apache 2 license.

[1]: http://www.saxonica.com/ce/index.xml
[2]: http://www.scala-js.org/
[3]: http://ebruchez.github.io/darius-xpath.js/
[4]: https://github.com/ebruchez/darius-xml.js
