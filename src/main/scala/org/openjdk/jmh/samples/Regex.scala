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

  /**
   *
   * @return
   */
  def parseT2() : Option[T2] = {
    if(inputIndex < input.length && !input.charAt(inputIndex).equals('|')){
      Some(T2(parseF(), parseT2()))
    }
    else None
  }

  def parseF() : F = {
    F(parseA, parseF2())
  }

  /**
   *
   * @return
   */
  def parseF2() : Option[F2] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == '?'){
      inputIndex = inputIndex + 1
      Some(F2(parseF2()))
    }
    else None
  }

  /**
   *
   * @return
   */
  def parseA() : A = {
    if(inputIndex < input.length && input.charAt(inputIndex) != '('){
        A(parseC(), None)
    }
    else {
      inputIndex = inputIndex + 1
      A(None, parseA2())
    }
  }

  /**
   *
   * @return
   */
  def parseA2() : Option[A2] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == ')'){
      inputIndex = inputIndex + 1
      None
    }
    else {
      Some(A2(parseE()))
    }
  }

  /**
   *
   * @return
   */
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

  val state : E = parseS()
  val root : E = state
  val candidate : String = input
  var candidateIndex = 0
  //Try to just traverse through the case tree until you reach a Char or none...and hope...
  val trace : E = E(T(F(A(Some(C('a')),None),None),Some(T2(F(A(Some(C('b')),None),None),None))),None)

  def matchS() : Boolean = {
    matchE(root) && candidateIndex == candidate.length
  }

  def matchE(e : E) : Boolean = e match {
    case s : E =>  matchT(s.left) && matchE2(s.right)
  }

  def matchE2(e2 : Option[E2]) : Boolean = e2 match {
    case Some(s) => true
  }

  def matchT(t : T) : Boolean = {
    case t : T => matchF(t.left) && matchT2(t.right)
  }

  def matchT2(t : Option[T2]) : Boolean = t match {
    case Some(s) => matchF(t.get.left) && matchT2(s.right)
    case None => true
  }

  def matchF(f : F) : Boolean = f match {
    case f : F => matchA(f.left) && matchF2(f.right)
  }

  def matchF2(f2 : Option[F2]) : Boolean = f2 match {
    case Some(s) => //Do something similiar to the parse method but with the inputted string this time and the index of that string.
      true
  }

  def matchA(a : A) : Boolean = a match {
    case Some(s) => true
    case None => true
  }



  state match {
    case E(T(F(A(Some(C('a')),None),None),Some(T2(F(A(Some(C('b')),None),None),None))),None) => println(true)
  }

}
