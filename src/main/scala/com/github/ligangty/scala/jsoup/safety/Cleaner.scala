package com.github.ligangty.scala.jsoup.safety

import com.github.ligangty.scala.jsoup.helper.Validator.notNull
import com.github.ligangty.scala.jsoup.nodes._
import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.select.{NodeTraversor, NodeVisitor}

/**
 * The whitelist based HTML cleaner. Use to ensure that end-user provided HTML contains only the elements and attributes
 * that you are expecting; no junk, and no cross-site scripting attacks!
 * <p>
 * The HTML cleaner parses the input as HTML and then runs it through a white-list, so the output HTML can only contain
 * HTML that is allowed by the whitelist.
 * </p>
 * <p>
 * It is assumed that the input HTML is a body fragment; the clean methods only pull from the source's body, and the
 * canned white-lists only allow body contained tags.
 * </p>
 * <p>
 * Rather than interacting directly with a Cleaner object, generally see the {@code clean} methods in {@link org.jsoup.Jsoup}.
 * </p>
 *
 * @constructor Create a new cleaner, that sanitizes documents using the supplied whitelist.
 * @param whitelist white-list to clean with
 */
class Cleaner(private var whitelist: Whitelist) {

  notNull(whitelist)

  /**
   * Creates a new, clean document, from the original dirty document, containing only elements allowed by the whitelist.
   * The original document is not modified. Only elements from the dirt document's <code>body</code> are used.
   * @param dirtyDocument Untrusted base document to clean.
   * @return cleaned document.
   */
  def clean(dirtyDocument: Document): Document = {
    notNull(dirtyDocument)
    val clean: Document = Document.createShell(dirtyDocument.baseUri)
    if (dirtyDocument.body != null) {
      copySafeNodes(dirtyDocument.body, clean.body)
    }
    clean
  }

  /**
   * Determines if the input document is valid, against the whitelist. It is considered valid if all the tags and attributes
   * in the input HTML are allowed by the whitelist.
   * <p>
   * This method can be used as a validator for user input forms. An invalid document will still be cleaned successfully
   * using the {@link #clean(Document)} document. If using as a validator, it is recommended to still clean the document
   * to ensure enforced attributes are set correctly, and that the output is tidied.
   * </p>
   * @param dirtyDocument document to test
   * @return true if no tags or attributes need to be removed; false if they do
   */
  def isValid(dirtyDocument: Document): Boolean = {
    notNull(dirtyDocument)
    val clean: Document = Document.createShell(dirtyDocument.baseUri)
    val numDiscarded: Int = copySafeNodes(dirtyDocument.body, clean.body)
    numDiscarded == 0
  }

  /**
   * Iterates the input and copies trusted nodes (tags, attributes, text) into the destination.
   */
  private class CleaningVisitor private[Cleaner](private val root: Element, private var destination: Element) extends NodeVisitor {

    private[Cleaner] var numDiscarded: Int = 0

    def head(source: Node, depth: Int) {
      source match {
        case sourceEl: Element =>
          if (whitelist.isSafeTag(sourceEl.tagName)) {
            // safe, clone and copy safe attrs
            val meta: Cleaner.ElementMeta = createSafeElement(sourceEl)
            val destChild: Element = meta.el
            destination.appendChild(destChild)
            numDiscarded += meta.numAttribsDiscarded
            destination = destChild
          } else if (source ne root) {
            // not a safe tag, so don't add. don't count root against discarded.
            numDiscarded += 1
          }
        case sourceText: TextNode =>
          val destText: TextNode = new TextNode(sourceText.getWholeText, source.baseUri)
          destination.appendChild(destText)
        case sourceData: DataNode if whitelist.isSafeTag(source.parent.nodeName()) =>
          val destData: DataNode = new DataNode(sourceData.getWholeData, source.baseUri)
          destination.appendChild(destData)
        case _ => numDiscarded += 1 // else, we don't care about comments, xml proc instructions, etc

      }
    }

    def tail(source: Node, depth: Int) {
      if (source.isInstanceOf[Element] && whitelist.isSafeTag(source.nodeName())) {
        destination = destination.parent // would have descended, so pop destination stack
      }
    }
  }

  private def copySafeNodes(source: Element, dest: Element): Int = {
    val cleaningVisitor: Cleaner#CleaningVisitor = new this.CleaningVisitor(source, dest)
    val traversor: NodeTraversor = new NodeTraversor(cleaningVisitor)
    traversor.traverse(source)
    cleaningVisitor.numDiscarded
  }

  private def createSafeElement(sourceEl: Element): Cleaner.ElementMeta = {
    val sourceTag: String = sourceEl.tagName
    val destAttrs: Attributes = new Attributes
    val dest: Element = new Element(Tag(sourceTag), sourceEl.baseUri, destAttrs)
    var numDiscarded: Int = 0
    val sourceAttrs: Attributes = sourceEl.attributes
    for (sourceAttr <- sourceAttrs) {
      if (whitelist.isSafeAttribute(sourceTag, sourceEl, sourceAttr)) {
        destAttrs.put(sourceAttr)
      } else {
        numDiscarded += 1
      }
    }
    val enforcedAttrs: Attributes = whitelist.getEnforcedAttributes(sourceTag)
    destAttrs.addAll(enforcedAttrs)
    new Cleaner.ElementMeta(dest, numDiscarded)
  }

}

private object Cleaner {

  private class ElementMeta {

    private[safety] var el: Element = null
    private[safety] var numAttribsDiscarded: Int = 0

    private[safety] def this(el: Element, numAttribsDiscarded: Int) {
      this()
      this.el = el
      this.numAttribsDiscarded = numAttribsDiscarded
    }
  }

}