package client.net.sf.saxon.ce.functions

import client.net.sf.saxon.ce.expr.StaticProperty
import client.net.sf.saxon.ce.om.Sequence
import client.net.sf.saxon.ce.pattern.AnyNodeTest
import client.net.sf.saxon.ce.pattern.NodeKindTest
import client.net.sf.saxon.ce.`type`.AnyItemType
import client.net.sf.saxon.ce.`type`.AtomicType
import client.net.sf.saxon.ce.`type`.ItemType
import client.net.sf.saxon.ce.value.EmptySequence
import client.net.sf.saxon.ce.value.SequenceType
import java.util.HashMap
//remove if not needed
import scala.collection.JavaConversions._

object StandardFunction {

  var EMPTY: Sequence = EmptySequence.getInstance

  /**
   * Categories of functions, bit significant
   */
  val CORE = 1

  val XSLT = 2

  val USE_WHEN = 4

  /**
   * Register a system function in the table of function details.
   *
   *
   * @param name          the function name
   * @param skeleton      instance of the class used to implement the function
   * @param signature      information about the argument types, expressed using a micro-syntax as follows.
   * There is a sequence of space-separated entries, one per argument plus one (the first) for the result type.
   * If the argument is optional, this
   * starts with "?". The rest of the string is either "*", meaning any type allowed, or an item type followed
   * by an occurrence indicator. The item type is for example i=integer, a=atomic, d=double, s=string, n=numeric,
   * N=node, D=document, E=element, I=item, dt=dateTime, dur=duration, tim=time, dtd=dayTimeDuration, q=qName, u=anyUri.
   * There can also be flags: a string starting with "$".
   * @return the entry describing the function. The entry is incomplete, it does not yet contain information
   *         about the function arguments.
   */
  def register(name: String, skeleton: SystemFunction, signature: String): Entry = {
    val e = makeEntry(name, skeleton)
    functionTable.put(name, e)
    if (signature != "") {
      val args = signature.split("\\s")
      var first = true
      for (arg <- args) {
        if (arg.startsWith("$")) {
          for (i <- 1 until arg.length) {
            val c = arg.charAt(i)
            c match {
              case 'f' => e.contextItemAsFirstArgument = true
              case 's' => e.sameItemTypeAsFirstArgument = true
              case 't' => e.applicability = XSLT
              case 'u' => e.applicability = XSLT | USE_WHEN
              case 'x' => e.maxArguments = Integer.MAX_VALUE
            }
          }
        } else if (arg == "*") {
          if (first) {
            e.resultType = SequenceType.ANY_SEQUENCE
          } else {
            e.mandatoryArg(SequenceType.ANY_SEQUENCE)
          }
        } else {
          var optional = false
          var it: String = null
          if (arg.startsWith("?")) {
            optional = true
            arg = arg.substring(1)
          }
          var card = StaticProperty.EXACTLY_ONE
          if (arg.endsWith("*")) {
            card = StaticProperty.ALLOWS_ZERO_OR_MORE
            it = arg.substring(0, arg.length - 1)
          } else if (arg.endsWith("?")) {
            card = StaticProperty.ALLOWS_ZERO_OR_ONE
            it = arg.substring(0, arg.length - 1)
          } else if (arg.endsWith("+")) {
            card = StaticProperty.ALLOWS_ONE_OR_MORE
            it = arg.substring(0, arg.length - 1)
          } else {
            it = arg
          }
          var t: ItemType = null
          if (it == "I") {
            t = AnyItemType.getInstance
          } else if (it == "N") {
            t = AnyNodeTest.getInstance
          } else if (it == "D") {
            t = NodeKindTest.DOCUMENT
          } else if (it == "E") {
            t = NodeKindTest.ELEMENT
          } else if (it == "a") {
            t = AtomicType.ANY_ATOMIC
          } else if (it == "i") {
            t = AtomicType.INTEGER
          } else if (it == "d") {
            t = AtomicType.DOUBLE
          } else if (it == "dec") {
            t = AtomicType.DECIMAL
          } else if (it == "n") {
            t = AtomicType.NUMERIC
          } else if (it == "s") {
            t = AtomicType.STRING
          } else if (it == "b") {
            t = AtomicType.BOOLEAN
          } else if (it == "dt") {
            t = AtomicType.DATE_TIME
          } else if (it == "dat") {
            t = AtomicType.DATE
          } else if (it == "tim") {
            t = AtomicType.TIME
          } else if (it == "dur") {
            t = AtomicType.DURATION
          } else if (it == "dtd") {
            t = AtomicType.DAY_TIME_DURATION
          } else if (it == "q") {
            t = AtomicType.QNAME
          } else if (it == "u") {
            t = AtomicType.ANY_URI
          } else {
            throw new IllegalArgumentException(it)
          }
          val st = SequenceType.makeSequenceType(t, card)
          if (first) {
            e.resultType = st
          } else if (optional) {
            e.optionalArg(st)
          } else {
            e.mandatoryArg(st)
          }
        }
        first = false
      }
    }
    e
  }

  /**
   * Make a table entry describing the signature of a function, with a reference to the implementation class.
   *
   *
   * @param name         the function name
   * @param skeleton     instance of the class used to implement the function
   * @return the entry describing the function. The entry is incomplete, it does not yet contain information
   *         about the function arguments.
   */
  def makeEntry(name: String, skeleton: SystemFunction): Entry = {
    val e = new Entry()
    val hash = name.indexOf('#')
    e.name = if (hash < 0) name else name.substring(0, hash)
    e.skeleton = skeleton
    e.minArguments = 0
    e.maxArguments = 0
    e.applicability = CORE
    e.argumentTypes = Array.ofDim[SequenceType](0)
    e.sameItemTypeAsFirstArgument = false
    e
  }

  private var functionTable: HashMap[String, Entry] = new HashMap[String, Entry](200)

  register("abs", new Rounding(Rounding.ABS), "n? n? $s")

  register("adjust-date-to-timezone", new Adjust(), "dat? dat? ?dtd?")

  register("adjust-dateTime-to-timezone", new Adjust(), "dt? dt? ?dtd?")

  register("adjust-time-to-timezone", new Adjust(), "tim? tim? ?dtd?")

  register("avg", new Average(), "a? a*")

  register("base-uri", new BaseURI(), "u? N? $f")

  register("boolean", new BooleanFn(BooleanFn.BOOLEAN), "b *")

  register("ceiling", new Rounding(Rounding.CEILING), "n? n? $s")

  register("codepoint-equal", new CodepointEqual(), "b? s? s?")

  register("codepoints-to-string", new CodepointsToString(), "s i*")

  register("compare", new Compare(), "i? s? s? ?s")

  register("concat", new Concat(), "s a? $x")

  register("contains", new Contains(Contains.CONTAINS), "b s? s? ?s")

  register("count", new Count(), "i *")

  register("current", new Current(), "I $t")

  register("current-date", new CurrentDateTime(), "dat")

  register("current-dateTime", new CurrentDateTime(), "dt")

  register("current-time", new CurrentDateTime(), "tim")

  register("current-group", new CurrentGroup(), "* $t")

  register("current-grouping-key", new CurrentGroupingKey(), "a? $t")

  register("dateTime", new DateTimeConstructor(), "dt? dat? tim?")

  register("day-from-date", new Component(Component.DAY), "i? dat?")

  register("day-from-dateTime", new Component(Component.DAY), "i? dt?")

  register("days-from-duration", new Component(Component.DAY), "i? dur?")

  register("deep-equal", new DeepEqual(), "b * * ?s")

  register("distinct-values", new DistinctValues(), "a* a* ?s")

  register("doc", new Doc(), "D? s?")

  register("doc-available", new DocAvailable(), "b s?")

  register("document", new DocumentFn(), "N* * ?N $t")

  register("document-uri", new NamePart(NamePart.DOCUMENT_URI), "u? N*")

  register("empty", new Exists(Exists.EMPTY), "b *")

  register("ends-with", new Contains(Contains.ENDS_WITH), "b s? s? ?s")

  register("element-available", new Available(Available.ELEMENT_AVAILABLE), "b s $u")

  register("element-with-id", new Id(), "E* s* ?N")

  register("encode-for-uri", new EscapeURI(EscapeURI.ENCODE_FOR_URI), "s s?")

  register("escape-html-uri", new EscapeURI(EscapeURI.HTML_URI), "s s?")

  register("error", new Error(), "I? ?q? ?s ?I")

  register("exists", new Exists(Exists.EXISTS), "b *")

  register("floor", new Rounding(Rounding.FLOOR), "n? n? $s")

  register("format-date", new FormatDate(), "s dat? s ?s? ?s? ?s? $t")

  register("format-dateTime", new FormatDate(), "s dt? s ?s? ?s? ?s? $t")

  register("format-number", new FormatNumber(), "s n? s ?s $t")

  register("format-time", new FormatDate(), "s tim? s ?s? ?s? ?s? $t")

  register("function-available", new Available(Available.FUNCTION_AVAILABLE), "b s ?i $u")

  register("generate-id", new NamePart(NamePart.GENERATE_ID), "s N? $tf")

  register("hours-from-dateTime", new Component(Component.HOURS), "i? dt?")

  register("hours-from-duration", new Component(Component.HOURS), "i? dur?")

  register("hours-from-time", new Component(Component.HOURS), "i? tim?")

  register("id", new Id(), "E* s* ?N")

  register("implicit-timezone", new CurrentDateTime(), "dtd")

  register("in-scope-prefixes", new InScopePrefixes(), "s* E")

  register("index-of", new IndexOf(), "i* a* a ?s")

  register("insert-before", new Insert(), "* * i *")

  register("iri-to-uri", new EscapeURI(EscapeURI.IRI_TO_URI), "s s?")

  register("key", new KeyFn(), "N* s a* ?N $t")

  register("lang", new Lang(), "b s? ?N")

  register("last", new Last(), "i")

  register("local-name", new NamePart(NamePart.LOCAL_NAME), "s N? $f")

  register("local-name-from-QName", new Component(Component.LOCALNAME), "s? q?")

  register("lower-case", new ForceCase(ForceCase.LOWERCASE), "s s?")

  register("matches", new Matches(), "b s? s ?s")

  register("max", new Minimax(Minimax.MAX), "a? a* ?s")

  register("min", new Minimax(Minimax.MIN), "a? a* ?s")

  register("minutes-from-dateTime", new Component(Component.MINUTES), "i? dt?")

  register("minutes-from-duration", new Component(Component.MINUTES), "i? dur?")

  register("minutes-from-time", new Component(Component.MINUTES), "i? tim?")

  register("month-from-date", new Component(Component.MONTH), "i? dat?")

  register("month-from-dateTime", new Component(Component.MONTH), "i? dt?")

  register("months-from-duration", new Component(Component.MONTH), "i? dur?")

  register("name", new NamePart(NamePart.NAME), "s N? $f")

  register("namespace-uri", new NamePart(NamePart.NAMESPACE_URI), "u N? $f")

  register("namespace-uri-for-prefix", new NamespaceForPrefix(), "u? s? E")

  register("namespace-uri-from-QName", new Component(Component.NAMESPACE), "u? q?")

  register("node-name", new NamePart(NamePart.NODE_NAME), "q? N?")

  register("not", new BooleanFn(BooleanFn.NOT), "b *")

  register("normalize-space", new NormalizeSpace(), "s ?s?")

  register("normalize-space#0", new NormalizeSpace(), "s")

  register("normalize-space#1", new NormalizeSpace(), "s s?")

  register("normalize-unicode", new NormalizeUnicode(), "s s? ?s")

  register("number", new NumberFn(), "d a? $f")

  register("position", new Position(), "i")

  register("prefix-from-QName", new Component(Component.PREFIX), "s? q?")

  register("QName", new QNameFn(), "q s? s")

  register("regex-group", new RegexGroup(), "s i $t")

  register("remove", new Remove(), "* * i $s")

  register("replace", new Replace(), "s s? s s ?s")

  register("resolve-QName", new ResolveQName(), "q? s? E")

  register("resolve-uri", new ResolveURI(), "u? s? ?s")

  register("reverse", new Reverse(), "* *")

  register("root", new Root(), "N? ?N? $f")

  register("round", new Rounding(Rounding.ROUND), "n? n? $s")

  register("round-half-to-even", new Rounding(Rounding.HALF_EVEN), "n? n? i $s")

  register("seconds-from-dateTime", new Component(Component.SECONDS), "dec? dt?")

  register("seconds-from-duration", new Component(Component.SECONDS), "dec? dur?")

  register("seconds-from-time", new Component(Component.SECONDS), "dec? tim?")

  register("starts-with", new Contains(Contains.STARTS_WITH), "b s? s? ?s")

  register("string", new StringFn(), "s I? $f")

  register("string-length", new StringLength(), "i")

  register("string-length#0", new StringLength(), "i")

  register("string-length#1", new StringLength(), "i s?")

  register("string-join", new StringJoin(), "s s* s")

  register("string-to-codepoints", new StringToCodepoints(), "i* s?")

  register("subsequence", new Subsequence(), "* * d ?d $s")

  register("substring", new Substring(), "s s? d ?d")

  register("substring-after", new SubstringAfterBefore(SubstringAfterBefore.AFTER), "s s? s? ?s")

  register("substring-before", new SubstringAfterBefore(SubstringAfterBefore.BEFORE), "s s? s? ?s")

  register("sum", new Sum(), "a? a* ?a")

  register("system-property", new SystemProperty(), "s s $u")

  register("timezone-from-date", new Component(Component.TIMEZONE), "dtd? dat?")

  register("timezone-from-dateTime", new Component(Component.TIMEZONE), "dtd? dt?")

  register("timezone-from-time", new Component(Component.TIMEZONE), "dtd? tim?")

  register("translate", new Translate(), "s s? s s")

  register("tokenize", new Tokenize(), "s* s? s ?s")

  register("trace", new Trace(), "* * s")

  register("type-available", new Available(Available.TYPE_AVAILABLE), "b s $u")

  register("upper-case", new ForceCase(ForceCase.UPPERCASE), "s s?")

  register("unparsed-text", new UnparsedText(UnparsedText.UNPARSED_TEXT), "s? s? ?s $t")

  register("unparsed-text-available", new UnparsedText(UnparsedText.UNPARSED_TEXT_AVAILABLE), "b s s $t")

  register("year-from-date", new Component(Component.YEAR), "i? dat?")

  register("year-from-dateTime", new Component(Component.YEAR), "i? dt?")

  register("years-from-duration", new Component(Component.YEAR), "i? dur?")

  /**
   * Get the table entry for the function with a given name
   *
   * @param name the name of the function. This is an unprefixed local-name for functions in the
   *             system namespace
   * @param arity the number of arguments
   * @return if the function name is known, an Entry containing information about the function. Otherwise,
   *         null
   */
  def getFunction(name: String, arity: Int): Entry = {
    val e = functionTable.get(name + '#' + arity)
    if (e != null) {
      return e
    }
    functionTable.get(name)
  }

  /**
   * An entry in the table describing the properties of a function
   */
  class Entry {

    /**
     * The name of the function: a local name in the case of functions in the standard library
     */
    var name: String = _

    /**
     * The class containing the implementation of this function (always a subclass of SystemFunction)
     */
    var skeleton: SystemFunction = _

    /**
     * The minimum number of arguments required
     */
    var minArguments: Int = _

    /**
     * The maximum number of arguments permitted
     */
    var maxArguments: Int = _

    /**
     * The  type of the result of the function
     */
    var resultType: SequenceType = _

    /**
     * Flags indicating which host languages the function is applicable to
     */
    var applicability: Int = _

    /**
     * Flag indicating the function result is the same item type as the first argument
     */
    var sameItemTypeAsFirstArgument: Boolean = _

    /**
     * Flag indicating that the first argument defaults to the context item
     */
    var contextItemAsFirstArgument: Boolean = _

    /**
     * An array holding the types of the arguments to the function
     */
    var argumentTypes: Array[SequenceType] = _

    /**
     * Add information to a function entry about the argument types of the function
     *
     * @param type          the sequence type of the argument
     */
    def mandatoryArg(`type`: SequenceType) {
      val e = this
      e.minArguments += 1
      e.maxArguments += 1
      val argNr = e.argumentTypes.length
      val st2 = Array.ofDim[SequenceType](e.argumentTypes.length + 1)
      System.arraycopy(e.argumentTypes, 0, st2, 0, e.argumentTypes.length)
      e.argumentTypes = st2
      e.argumentTypes(argNr) = `type`
    }

    def optionalArg(`type`: SequenceType) {
      val e = this
      e.maxArguments += 1
      val argNr = e.argumentTypes.length
      val st2 = Array.ofDim[SequenceType](e.argumentTypes.length + 1)
      System.arraycopy(e.argumentTypes, 0, st2, 0, e.argumentTypes.length)
      e.argumentTypes = st2
      e.argumentTypes(argNr) = `type`
    }
  }
}
