/**
 * Copyright 2015 Orbeon, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.orbeon.darius.xpath.orbeon

import org.orbeon.darius.xpath.expr.{Binding, VariableReference, XPathContext}
import org.orbeon.darius.xpath.functions.{ConstructorFunctionLibrary, FunctionLibrary, FunctionLibraryList}
import org.orbeon.darius.xpath.om.{NamespaceResolver, StructuredQName}
import org.orbeon.darius.xpath.sxpath.AbstractStaticContext
import org.orbeon.darius.xpath.trans.XPathException
import org.orbeon.darius.xpath.value.{QNameValue, SequenceType}

import scala.collection.JavaConverters._
import scala.collection.immutable

// Similar to Saxon JAXPXPathStaticContext. JAXPXPathStaticContext holds a reference to an XPathVariableResolver, which
// is not desirable as variable resolution occurs at runtime. So here instead we create a fully shareable context.
class ShareableXPathStaticContext(
    config           : Configuration,
    namespaceMapping : immutable.Map[String, String],
    functionLibrary  : FunctionLibrary
) extends AbstractStaticContext {

    // This also creates an Executable
    setConfiguration(config)

    // Add function library
    getFunctionLibrary.asInstanceOf[FunctionLibraryList].addFunctionLibrary(functionLibrary)
    getFunctionLibrary.asInstanceOf[FunctionLibraryList].addFunctionLibrary(ConstructorFunctionLibrary.getInstance)

    // Return the names of global variables referenced by the expression after it has been parsed
    private var boundVariables = Set.empty[StructuredQName]
    def referencedVariables: Iterable[StructuredQName] = boundVariables

    def declareVariable(qname: QNameValue) = throw new IllegalStateException                       // never used in Saxon
    def declareVariable(namespaceURI: String, localName: String) = throw new IllegalStateException // shouldn't be called in our case

    def bindVariable(qName: StructuredQName): VariableReference = {
        // Q: Can this be called multiple time with the same name, and if so should we return the same VariableReference?
        boundVariables += qName
        new VariableReference(new VariableBinding(qName))
    }

    // Per Saxon: "used to represent the run-time properties and methods associated with a variable: specifically, a
    // method to get the value of the variable".
    class VariableBinding(qName: StructuredQName) extends Binding {
        def isGlobal = true
        def getLocalSlotNumber = -1 // "If this is a local variable held on the local stack frame"

        def getVariableQName = qName
        def getRequiredType = SequenceType.ANY_SEQUENCE

        // Saxon does something similar but different in XPathVariable, where it stores variables in the the dynamic
        // context. That uses slots however, which means we cannot resolve variables fully dynamically. So I think
        // our approach below is ok.
        def evaluateVariable(context: XPathContext) = {
//            val variableResolver = context.getController.getUserData(classOf[ShareableXPathStaticContext].getName, "variableResolver").asInstanceOf[VariableResolver]
//            variableResolver(qName, context)
          ??? //ORBEON
        }
    }

    // Namespace resolver
    private object NSResolver extends NamespaceResolver {
        def getURIForPrefix(prefix: String, useDefault: Boolean) =
            if (prefix == "") {
                if (useDefault)
                    getDefaultElementNamespace
                else
                    ""
            } else
                namespaceMapping(prefix)


        def iteratePrefixes =
            namespaceMapping.keys.iterator.asJava
    }

    def getURIForPrefix(prefix: String): String = {
        val uri = NSResolver.getURIForPrefix(prefix, useDefault = false)
        if (uri == null)
            throw new XPathException(s"Prefix $prefix has not been declared")
        uri
    }

    def getNamespaceResolver: NamespaceResolver = NSResolver
}
