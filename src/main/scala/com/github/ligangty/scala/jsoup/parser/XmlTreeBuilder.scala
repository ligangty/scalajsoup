package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes._

/**
 * Use the <code>XmlTreeBuilder</code> when you want to parse XML without any of the HTML DOM rules being applied to the
 * document.
 * <p>Usage example: {{{Document xmlDoc = Jsoup.parse(html, baseUrl, Parser.xmlParser());}}}</p>
 *
 */
class XmlTreeBuilder extends TreeBuilder {

  protected override def initialiseParse(input: String, baseUri: String, errors: ParseErrorList) {
    super.initialiseParse(input, baseUri, errors)
    stack.append(doc)
    doc.outputSettings.syntax(Document.OutputSettings.Syntax.xml)
  }

  protected def process(token: Token): Boolean = {
    // start tag, end tag, doctype, comment, character, eof
    token.tokType match {
      case Token.TokenType.StartTag =>
        insert(token.asStartTag)
      case Token.TokenType.EndTag =>
        popStackToClose(token.asEndTag)
      case Token.TokenType.Comment =>
        insert(token.asComment)
      case Token.TokenType.Character =>
        insert(token.asCharacter)
      case Token.TokenType.Doctype =>
        insert(token.asDoctype)
      case Token.TokenType.EOF => ()
      case _ =>
        fail("Unexpected token type: " + token.tokType)
    }
    true
  }

  private def insertNode(node: Node) {
    currentElement.appendChild(node)
  }

  private[parser] def insert(startTag: Token.StartTag): Element = {
    val tag: Tag = Tag(startTag.name)
    val el: Element = new Element(tag, baseUri, startTag.attributes)
    insertNode(el)
    if (startTag.isSelfClosing) {
      tokeniser.acknowledgeSelfClosingFlag()
      if (!tag.isKnownTag) {
        tag.setSelfClosing()
      }
    } else {
      stack.append(el)
    }
    el
  }

  private[parser] def insert(commentToken: Token.Comment) {
    val comment: Comment = new Comment(commentToken.getData, baseUri)
    var insert: Node = comment
    if (commentToken.bogus) {
      val data: String = comment.getData
      if (data.length > 1 && (data.startsWith("!") || data.startsWith("?"))) {
        val declaration: String = data.substring(1)
        insert = new XmlDeclaration(declaration, comment.baseUri, data.startsWith("!"))
      }
    }
    insertNode(insert)
  }

  private[parser] def insert(characterToken: Token.Character) {
    val node: Node = new TextNode(characterToken.getData, baseUri)
    insertNode(node)
  }

  private[parser] def insert(d: Token.Doctype) {
    val doctypeNode: DocumentType = new DocumentType(d.getName, d.getPublicIdentifier, d.getSystemIdentifier, baseUri)
    insertNode(doctypeNode)
  }

  /**
   * If the stack contains an element with this tag's name, pop up the stack to remove the first occurrence. If not
   * found, skips.
   *
   * @param endTag endTag
   */
  private def popStackToClose(endTag: Token.EndTag) {
    val elName: String = endTag.name
    var firstFound: Element = null
    import scala.util.control.Breaks._
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        val next = stack(pos)
        if (next.nodeName().equals(elName)) {
          firstFound = next
          break()
        }
      }
    }
    if (firstFound == null) {
      return
    }; // not found, skip
    breakable {
      for (pos <- (stack.size - 1).to(0,-1)) {
        val next = stack(pos)
        stack.remove(pos)
        if (next == firstFound) {
          break()
        }
      }
    }
  }

  private[parser] def parseFragment(inputFragment: String, baseUri: String, errors: ParseErrorList): Seq[Node] = {
    initialiseParse(inputFragment, baseUri, errors)
    runParser()
    doc.getChildNodes
  }
}
