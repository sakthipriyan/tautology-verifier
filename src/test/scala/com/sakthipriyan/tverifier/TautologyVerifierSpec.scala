package com.sakthipriyan.tverifier

import org.scalatest.{ Matchers, WordSpec }

class TautologyVerifierSpec extends WordSpec with Matchers {
  "TautologyVerifier" when {

    "created" should {
      "succeed" in {
        val tVerifier = new TautologyVerifier("(a | b)")
        tVerifier.statement should equal("(a | b)")
        tVerifier.variables should have size 2
      }
    }

    "buildTree" should {
      "succeed with one node" in {
        val tVerifier = new TautologyVerifier("a")
        val inputs = tVerifier.buildTree
        inputs should be(VariableNode('a'))
      }
      "succeed with single operation" in {
        val tVerifier = new TautologyVerifier("(a | b)")
        val tree = tVerifier.buildTree
        tree should be(OrOperatorNode(VariableNode('a'), VariableNode('b')))
      }
      "succeed with double operation" in {
        val tVerifier = new TautologyVerifier("((a | b) & c)")
        val tree = tVerifier.buildTree
        tree should be(AndOperatorNode(OrOperatorNode(VariableNode('a'), VariableNode('b')), VariableNode('c')))
      }
      "succeed with triple operation" in {
        val tVerifier = new TautologyVerifier("(( a | b ) & ( c | d ))")
        val tree = tVerifier.buildTree
        tree should be(AndOperatorNode(
          OrOperatorNode(VariableNode('a'), VariableNode('b')),
          OrOperatorNode(VariableNode('c'), VariableNode('d'))))
      }
    }

    "isTautology" should {
      "fail with single node" in {
        val tVerifier = new TautologyVerifier("a")
        tVerifier.isTautology should be(false)
      }
      "fail with simple and operation on same nodes" in {
        val tVerifier = new TautologyVerifier("(a & a)")
        tVerifier.isTautology should be(false)
      }
      "fail with simple or operation on same nodes" in {
        val tVerifier = new TautologyVerifier("(a | a)")
        tVerifier.isTautology should be(false)
      }
      "succeed with complementary nodes" in {
        val tVerifier = new TautologyVerifier("(a | !a)")
        tVerifier.isTautology should be(true)
      }
      "succeed with simple operation" in {
        val tVerifier = new TautologyVerifier("(!a | (a & a))")
        tVerifier.isTautology should be(true)
      }
      "succeed with complex operation" in {
        val tVerifier = new TautologyVerifier("((a & (!b | b)) | (!a & (!b | b)))")
        tVerifier.isTautology should be(true)
      }
    }

    "buildInputs" should {
      "succeed with one variable" in {
        val tVerifier = new TautologyVerifier("a")
        val inputs = tVerifier.buildInputs
        inputs should have size 2
        inputs(0).get('a').get should be(false)
        inputs(1).get('a').get should be(true)
      }
      "succeed with two variables" in {
        val tVerifier = new TautologyVerifier("(a | b)")
        val inputs = tVerifier.buildInputs
        inputs should have size 4
        inputs(0).get('a').get should be(false)
        inputs(0).get('b').get should be(false)
        inputs(1).get('a').get should be(true)
        inputs(1).get('b').get should be(false)
        inputs(2).get('a').get should be(false)
        inputs(2).get('b').get should be(true)
        inputs(3).get('a').get should be(true)
        inputs(3).get('b').get should be(true)
      }
    }

    "getPostFix" should {
      val tVerifier = new TautologyVerifier("a")
      "succeed single operation" in {
        val infix = "(a|b)".toList
        val postfix = tVerifier.getPostFix(infix)
        postfix.mkString should be("ab|")
      }
      "succeed double operation" in {
        val infix = "((a|b)|c)".toList
        val postfix = tVerifier.getPostFix(infix)
        postfix.mkString should be("ab|c|")
      }
      "succeed triple operation" in {
        val infix = "(((a|b)|c)&d)".toList
        val postfix = tVerifier.getPostFix(infix)
        postfix.mkString should be("ab|c|d&")
      }
      "succeed triple operation with !" in {
        val infix = "(((a|!b)|c)&!d)".toList
        val postfix = tVerifier.getPostFix(infix)
        postfix.mkString should be("a!b|c|!d&")
      }
    }
  }
}

class NodeSpec extends WordSpec with Matchers {
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

  val leftNode = VariableNode('a')
  val rightNode = VariableNode('a', true)

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
        val result1 = orNode.evaluate(Map('a' -> true))
        result1 should be(true)
        val result2 = orNode.evaluate(Map('a' -> false))
        result2 should be(true)
      }
    }
    "created with same node" should {
      "return same value" in {
        val orNode = OrOperatorNode(leftNode, leftNode)
        val result1 = orNode.evaluate(Map('a' -> true))
        result1 should be(true)
        val result2 = orNode.evaluate(Map('a' -> false))
        result2 should be(false)
      }
    }
  }
  "AndOperatorNode" when {
    "created" should {
      "succeed" in {
        val andNode = AndOperatorNode(leftNode, rightNode)
        andNode.left should be(leftNode)
        andNode.right should be(rightNode)
      }
    }
    "created with complementary nodes" should {
      "return false always" in {
        val andNode = AndOperatorNode(leftNode, rightNode)
        val result1 = andNode.evaluate(Map('a' -> true))
        result1 should be(false)
        val result2 = andNode.evaluate(Map('a' -> false))
        result2 should be(false)
      }
    }
    "created with same node" should {
      "return same input value" in {
        val andNode = AndOperatorNode(leftNode, leftNode)
        val result1 = andNode.evaluate(Map('a' -> true))
        result1 should be(true)
        val result2 = andNode.evaluate(Map('a' -> false))
        result2 should be(false)
      }
    }
  }
}