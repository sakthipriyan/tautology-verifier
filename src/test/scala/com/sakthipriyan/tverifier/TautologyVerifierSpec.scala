package com.sakthipriyan.tverifier

import org.scalatest.Inside
import org.scalatest.Inspectors
import org.scalatest.OptionValues
import org.scalatest.Matchers
import org.scalatest.WordSpec

class TautologyVerifierSpec extends WordSpec with Matchers with OptionValues with Inside with Inspectors {
  "TautologyVerifier" when {
    "created" should {
      "succeed" in {
        val tVerifier = new TautologyVerifier("(a | b)")
        tVerifier.statement should equal("(a | b)")
        tVerifier.variables should have size 2
      }
    }
    /*"count" should {
      "return 0" in {
        whenReady(raw.count) {
          s => s should be(0)
        }
      }
    }*/
  }
}

class NodeSpec extends WordSpec with Matchers with OptionValues with Inside with Inspectors {
  "VariableNode" when {
    "created without not" should {
      "succeed" in {
        val variableNode = VariableNode('a')
        variableNode.id should equal('a')
        variableNode.not should be(false)
      }
    }
    "created with not" should {
      "succeed" in {
        val variableNode = VariableNode('a', true)
        variableNode.id should equal('a')
        variableNode.not should be(true)
      }
    }
    "evaluated without not" should {
      "return same value" in {
        val variableNode = VariableNode('a')
        val result1 = variableNode.evaluate(Map('a' -> true))
        result1 should be(true)
        val result2 = variableNode.evaluate(Map('a' -> false))
        result2 should be(false)
      }
    }
    "evaluated with not" should {
      "return not value" in {
        val variableNode = VariableNode('a', true)
        val result1 = variableNode.evaluate(Map('a' -> true))
        result1 should be(false)
        val result2 = variableNode.evaluate(Map('a' -> false))
        result2 should be(true)
      }
    }
  }
  /*
   * case class OrOperatorNode(val left: Node, val right: Node) extends Node {
  def evaluate(map: Map[Char, Boolean]) = left.evaluate(map) || right.evaluate(map)
}

case class AndOperatorNode(val left: Node, val right: Node) extends Node {
  def evaluate(map: Map[Char, Boolean]) = left.evaluate(map) && right.evaluate(map)
}
   * 
   */
  val leftNode = VariableNode('a')
  val rightNode = VariableNode('a',true)
  "OrOperatorNode" when {
    "created" should {
      "succeed" in {
        val orNode = OrOperatorNode(leftNode, rightNode)
        orNode.left should be(leftNode)
        orNode.right should be(rightNode)
      }
    }
    "created with complementary nodes" should {
      "return true always" in {
        val orNode = OrOperatorNode(leftNode, rightNode)
        val result1 = orNode.evaluate(Map('a'->true))
        result1 should be(true)
        val result2 = orNode.evaluate(Map('a'->false))
        result2 should be(true)        
      }
    }
  }
}