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
/**
 */
object Regex extends App {

  val specialChars = collection.immutable.List('(',')', '?', '|')

  var input : String = readLine("Enter a string")
  var inputIndex : Int = 0

  def parseS() : E = {parseE()}

  def parseE() : E = {E(parseT(), parseE2())}

  def parseE2() : Option[E2] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == '|'){
      inputIndex = inputIndex + 1
      Some(E2(parseE3()))
    }
    else None
  }

  def parseE3() : E3 = {E3(parseT(), parseE2())}

  def parseT() : T = {
    T(parseF(), parseT2())
  }

  def parseT2() : Option[T2] = {
    if(inputIndex < input.length && !input.charAt(inputIndex).equals('|')){
      Some(T2(parseF(), parseT2()))
    }
    else None
  }

  def parseF() : F = {
    F(parseA, parseF2())
  }

  def parseF2() : Option[F2] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == '?'){
      inputIndex = inputIndex + 1
      Some(F2(parseF2()))
    }
    else None
  }

  def parseA() : A = {
    if(inputIndex < input.length && input.charAt(inputIndex) != '('){
        A(parseC(), None)
    }
    else {
      inputIndex = inputIndex + 1
      A(None, parseA2())
    }
  }

  def parseA2() : Option[A2] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == ')'){
      inputIndex = inputIndex + 1
      None
    }
    else {
      Some(A2(parseE()))
    }
  }

  def parseC() : Option[C] = {
    if(!specialChars.contains(input.charAt(inputIndex))){
      inputIndex = inputIndex + 1
      Some(C(input.charAt(inputIndex - 1)))
      }
    else {
      inputIndex = inputIndex + 1
      None
    }
  }

  def hasEven(i : Int) : Boolean = {
    i % 2 == 0
  }

  def hasOdd(i : Int) : Boolean = {
    i % 2 != 0
  }

  def satisfies(l : scala.collection.immutable.List[Int], predicate : Int => Boolean): Boolean ={
    var result : Boolean = false
    l.foreach((elem) => if(predicate(elem)){
      result = true
    })
    result
  }

  val state : E = parseS()
  val root : E = state
  val candidate : String = input
  var candidateIndex = 0
  val trace : E = E(T(F(A(Some(C('a')),None),None),Some(T2(F(A(Some(C('b')),None),None),None))),None)

  def matchS() : Boolean = {
    matchE(root) && candidateIndex = candidate.length
  }

  def matchE(e : Option[E]) : Boolean = e match {
    case Some(s) => matchT(s.left) && matchE2(s.right)
    case None => true
  }

  def matchE2(e2 : Option[E2]) : Boolean = e2 match {
    case Some(s) => match
  }

  def matchT(t : Option[T])

  def matchT2(t : Option[T2]) : Boolean = t match {
    case Some(s) => matchF(s.left) && matchT2(s.right)
    case None => true
  }



  state match {
    case E(T(F(A(Some(C('a')),None),None),Some(T2(F(A(Some(C('b')),None),None),None))),None) => println(true)
  }

}
