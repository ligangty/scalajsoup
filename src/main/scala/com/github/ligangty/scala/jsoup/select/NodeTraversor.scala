package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.nodes.Node

/**
 * Depth-first node traversor. Use to iterate through all nodes under and including the specified root node.
 * <p/>
 * This implementation does not use recursion, so a deep DOM does not risk blowing the stack.
 */
class NodeTraversor {
  private var visitor: NodeVisitor = null

  /**
   * Create a new traversor.
   * @param visitor a class implementing the { @link NodeVisitor} interface, to be called when visiting each node.
   */
  def this(visitor: NodeVisitor) {
    this()
    this.visitor = visitor
  }

  /**
   * Start a depth-first traverse of the root and all of its descendants.
   * @param root the root node point to traverse.
   */
  def traverse(root: Node) {
    var node: Node = root
    var depth: Int = 0
    import scala.util.control.Breaks._
    breakable {
      while (node != null) {
        visitor.head(node, depth)
        if (node.childNodeSize > 0) {
          node = node.getChildNode(0)
          depth += 1
        } else {
          while (node.nextSibling == null && depth > 0) {
            visitor.tail(node, depth)
            node = node.parentNode
            depth -= 1
          }
          visitor.tail(node, depth)
          if (node eq root) break
          node = node.nextSibling
        }
      }
    }
  }
}
