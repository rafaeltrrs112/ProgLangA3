package org.openjdk.jmh.samples

abstract class S
case class E(left: T, right: Option[E2]) extends S
case class E2(left: E3) extends S
case class E3(left: T, right: Option[E2]) extends S
case class T(left: F, right: Option[T2]) extends S
case class T2(left: F, right: Option[T2]) extends S
case class F(left: A, right: Option[F2]) extends S
case class F2(left: Option[F2]) extends S
case class A(opt1: Option[C], opt2: Option[A2]) extends S
case class A2(left: E) extends S
case class C(content: Char) extends S

object Regex extends App {
  var input : String = readLine("Enter a regex")
  var inputIndex : Int = 0
  val result = parseS()
  val root = result
  val candidate = readLine("Enter a string")
  val candidateLength = candidate.length
  var candidateIndex = 0

  def parseS() : E = {parseE()}

  def parseE() : E = {E(parseT(), parseE2())}

  def parseE2() : Option[E2] = {
    if(inputIndex < input.length && input.charAt(inputIndex).equals('|')){
      inputIndex = inputIndex + 1
      Some(E2(parseE3()))
    }
    else if(inputIndex < input.length && input.charAt(inputIndex).equals(')')) {
      inputIndex += 1
      None
    }
    else None
  }

  def parseE3() : E3 = {E3(parseT(), parseE2())}

  def parseT() : T = {
    T(parseF(), parseT2())
  }

  def parseT2() : Option[T2] = {
    if(inputIndex < input.length && !input.charAt(inputIndex).equals('|') && !input.charAt(inputIndex).equals(')')){
      Some(T2(parseF(), parseT2()))
    }
    else None
  }

  def parseF() : F = {
    F(parseA, parseF2())
  }

  def parseF2() : Option[F2] = {
    if(inputIndex < input.length && input.charAt(inputIndex).equals('?')){
      inputIndex = inputIndex + 1
      Some(F2(parseF2()))
    }
    else None
  }


  def parseA() : A = {
    if(inputIndex < input.length && input.charAt(inputIndex).equals('(')){
      inputIndex = inputIndex + 1
      A(None, parseA2())
    }
    else {
      A(parseC(), None)
    }
  }

  def parseA2() : Option[A2] = {
    if(inputIndex < input.length && input.charAt(inputIndex).equals(')')){
      None
    }
    else {
      if(inputIndex < input.length){
        Some(A2(parseE()))
      }
      else{
        None
      }
    }
  }

  def parseC() : Option[C] = {
    if(input.charAt(inputIndex).equals(')')){
      None
    }
    else{
      inputIndex += 1
      Some(C(input.charAt(inputIndex-1)))
    }
  }

  def matchRegExp() : Boolean = {
    matchE(root) && candidateIndex == candidate.length
  }

  def matchE(concat : E) : Boolean = {
    val currentIndex = candidateIndex
    if(!matchLeftBranch(concat.left)){
      candidateIndex = currentIndex
      matchE2(concat.right)
    }else true
  }

  //Go to next character
  def matchE2(leftOr : Option[E2]) : Boolean = leftOr match {
    case Some(lr) => {
      matchRightOr(lr.left)
    }
    case None => false
  }

  def matchRightOr(right : E3) : Boolean = {
    matchLeftBranch(right.left) || matchE2(right.right)
  }

  def matchLeftBranch(t : T) : Boolean = {
    matchF(t.left) && matchT2(t.right)
  }

  def matchT2(rightBranch : Option[T2]) : Boolean = rightBranch match {
    case Some(s) => matchF(s.left) && matchT2(s.right)
    case None => true
  }

  def matchF(leftBranchTwo : F) : Boolean = {
    val currentIndex = candidateIndex
    if(!matchA(leftBranchTwo.left)){
      candidateIndex = currentIndex
      matchF2(leftBranchTwo.right)
    } else true
  }

  //optional
  def matchF2(f2 : Option[F2]) : Boolean = f2 match {
    case Some(s) => !matchF2(s.left)
    case None => false

  }

  //Left-parens
  def matchA(a : A) : Boolean = {
    matchC(a.opt1) && matchA2(a.opt2)
  }

  //right-parens
  def matchA2(rightParen: Option[A2]) : Boolean = rightParen match {
    case Some(rp) => {
      matchE(rp.left)
    }
    case None => true
  }

  def matchC(characterLiteral: Option[C]) = characterLiteral match {
    case Some(c) => {
      candidateIndex += 1
      if (candidateIndex > candidateLength) {
        false
      } else {
        candidate(candidateIndex - 1).equals(c.content)
      }
    }
    case None => true
  }

  val test = matchRegExp()
  println(test)
}
