package com.sakthipriyan.tverifier

import scala.collection.mutable.{ ArrayBuffer, Stack }

object TautologyVerifier extends App {
  print("Welcome to Tautology Verifier. To exit press Ctrl + D. \nEnter Statement: ")
  for (ln <- io.Source.stdin.getLines) {
    print("Tautology: " + new TautologyVerifier(ln).isTautology + "\nEnter Statement: ")
  }
}

class TautologyVerifier(val statement: String) {
  val tree = buildTree()
  val variables = statement.replaceAll("[^a-z]", "").toSet.toList

  def buildTree(): Node = {
    val infix = statement.replaceAll("\\s+", "").toList
    val postfix = getPostFix(infix)
    val nodeStack = new Stack[Node]

    for ((c, i) <- postfix.zipWithIndex) {
      (c match {
        case '|' => {
          val right = nodeStack.pop
          val left = nodeStack.pop
          Some(OrOperatorNode(left, right))
        }
        case '&' => {
          val right = nodeStack.pop
          val left = nodeStack.pop
          Some(AndOperatorNode(left, right))
        }
        case '!' => None
        case a: Char => {
          if (i > 0 && postfix(i - 1) == '!')
            Some(VariableNode(a, true))
          else
            Some(VariableNode(a))
        }
      }).map(node => nodeStack.push(node))
    }
    nodeStack.pop
  }

  def getPostFix(chars: List[Char]): List[Char] = {
    val buffer = new ArrayBuffer[Char]
    val stack = new Stack[Char]
    for (char <- chars) {
      char match {
        case '(' | '&' | '|' => stack.push(char)
        case ')' => {
          var ch = stack.pop
          while (ch != '(') {
            buffer += ch
            ch = stack.pop
          }
        }
        case a: Char => buffer += a
      }
    }
    while (!stack.isEmpty) {
      buffer += stack.pop
    }
    buffer.toList
  }

  def isTautology: Boolean = {
    for (input <- buildInputs) {
      if (!tree.evaluate(input)) return false
    }
    return true
  }

  def buildInputs: List[Map[Char, Boolean]] = {
    val variableCount = variables.size
    val inputCount = Math.pow(2, variableCount).toInt
    (for (i <- 0 until inputCount) yield (
      for (j <- 0 until variableCount)
        yield (variables(j), if ((i >> j) % 2 == 0) false else true)).toMap).toList
  }
}

abstract class Node {
  def evaluate(map: Map[Char, Boolean]): Boolean
}

case class OrOperatorNode(val left: Node, val right: Node) extends Node {
  def evaluate(map: Map[Char, Boolean]) = left.evaluate(map) || right.evaluate(map)
}

case class AndOperatorNode(val left: Node, val right: Node) extends Node {
  def evaluate(map: Map[Char, Boolean]) = left.evaluate(map) && right.evaluate(map)
}

case class VariableNode(id: Char, not: Boolean = false) extends Node {
  def evaluate(map: Map[Char, Boolean]) = if (!not) map.get(id).get else !map.get(id).get
}
