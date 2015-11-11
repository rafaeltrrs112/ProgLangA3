package org.openjdk.jmh.samples

abstract class RegexExp
case class Concat(left: LeftBranch, right: Option[LeftOr]) extends RegexExp
case class LeftOr(left: RightOr) extends RegexExp
case class RightOr(left: LeftBranch, right: Option[LeftOr]) extends RegexExp
case class LeftBranch(left: LeftBranchTwo, right: Option[RightBranch]) extends RegexExp
case class RightBranch(left: LeftBranchTwo, right: Option[RightBranch]) extends RegexExp
case class LeftBranchTwo(left: LeftParen, right: Option[Optional]) extends RegexExp
case class Optional(left: Option[Optional]) extends RegexExp
case class LeftParen(opt1: Option[CharacterLiteral], opt2: Option[RightParen]) extends RegexExp
case class RightParen(left: Concat) extends RegexExp
case class CharacterLiteral(content: Char) extends RegexExp
case class State(position : Int)
/**
  */
object Regex extends App {

  val specialChars = collection.immutable.List('(',')', '?', '|')
  var input : String = readLine("Enter a regex")
  var inputIndex : Int = 0
  val result = parseS()
  val root = result
  val candidate = readLine("Enter a string")
  val candidateLength = candidate.length
  var candidateIndex = 0
  println(result)

  def parseS() : Concat = {parseConcat()}

  def parseConcat() : Concat = {Concat(parseLeftBranch(), parseOr())}

  def parseOr() : Option[LeftOr] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == '|'){
      inputIndex = inputIndex + 1
      Some(LeftOr(parseRightOr()))
    }
    else None
  }

  def parseRightOr() : RightOr = {RightOr(parseLeftBranch(), parseOr())}

  def parseLeftBranch() : LeftBranch = {
    LeftBranch(parseLeftBranchTwo(), parseRightBranch())
  }

  /**
    *
    * @return
    */
  def parseRightBranch() : Option[RightBranch] = {
    if(inputIndex < input.length && !input.charAt(inputIndex).equals('|')){
      Some(RightBranch(parseLeftBranchTwo(), parseRightBranch()))
    }
    else None
  }

  def parseLeftBranchTwo() : LeftBranchTwo = {
    LeftBranchTwo(parseOpenGroup, parseOptional())
  }

  /**
    *
    * @return
    */
  def parseOptional() : Option[Optional] = {
    if(inputIndex < input.length && input.charAt(inputIndex) == '?'){
      inputIndex = inputIndex + 1
      Some(Optional(parseOptional()))
    }
    else None
  }

  /**
    *
    * @return
    */
  def parseOpenGroup() : LeftParen = {
    if(inputIndex < input.length && input.charAt(inputIndex) != '('){
      LeftParen(parseCharacterLiteral(), None)
    }
    else {
      inputIndex = inputIndex + 1
      LeftParen(None, parseCloseGroup())
    }
  }

  /**
    *
    * @return
    */
  def parseCloseGroup() : Option[RightParen] = {
    if(inputIndex < input.length && input.charAt(inputIndex) != ')'){
      Some(RightParen(parseConcat()))
    }
    else {
      inputIndex = inputIndex + 1
      None
    }
  }

  /**
    *
    * @return
    */
  def parseCharacterLiteral() : Option[CharacterLiteral] = {
    if(!specialChars.contains(input.charAt(inputIndex))){
      inputIndex = inputIndex + 1
      Some(CharacterLiteral(input.charAt(inputIndex - 1)))
    }
    else {
      inputIndex = inputIndex + 1
      None
    }
  }


  var currentState = State(0)
  def matchRegExp() : Boolean = {
    matchConcat(root) && candidateIndex == candidate.length
  }

  def matchConcat(concat : Concat) : Boolean = concat match {
    case Concat(_, Some(lr)) => {
      println("holding")
      val currentIndex = candidateIndex
      matchLeftBranch(concat.left) || {
        println("up-top failed")
        candidateIndex = currentIndex
        matchLeftOr(concat.right)
      }
    }
    case Concat(lb, None) => matchLeftBranch(lb)
  }

  //Go to next character
  def matchLeftOr(leftOr : Option[LeftOr]) : Boolean = leftOr match {
    case Some(lr) => {
      matchRightOr(lr.left)
    }
    case _ => false
  }

  def matchRightOr(right : RightOr) : Boolean = right match {
    case RightOr(_, Some(lr)) => {
      println("holding")
      val currentIndex = candidateIndex
      matchLeftBranch(right.left) || {
        println("up-top failed")
        candidateIndex = currentIndex
        matchLeftOr(right.right)
      }
    }
    case RightOr(lb, None) => matchLeftBranch(lb)
  }

  def matchLeftBranch(t : LeftBranch) : Boolean = {
    matchLeftBranchTwo(t.left) || matchRightBranch(t.right)
  }

  def matchRightBranch(rightBranch : Option[RightBranch]) : Boolean = rightBranch match {
    case Some(s) => matchLeftBranchTwo(s.left) || matchRightBranch(s.right)
    case None => false
  }

  def matchLeftBranchTwo(leftBranchTwo : LeftBranchTwo) : Boolean = {
    matchLeftParen(leftBranchTwo.left) || matchOptional(leftBranchTwo.right)
  }

  //optional
  def matchOptional(f2 : Option[Optional]) : Boolean = false

  //Left-parens
  def matchLeftParen(leftParen : LeftParen) : Boolean = {
    leftParen match {
      case LeftParen(Some(charLit), _) => {
        if(charLit.content == candidate.charAt(candidateIndex)) {
          println("got match comparing" + charLit.content + " " + candidate.charAt(candidateIndex))
          candidateIndex += 1
          true
        }
        else {
          println("in else comparing" + charLit.content + " " + candidate.charAt(candidateIndex))
          candidateIndex += 1
          false
        }
      }
      case LeftParen(None, rightParen) => {
        matchCharacterLiteral(leftParen.opt1) || matchRightParen(rightParen)
      }
    }
  }

  //right-parens
  def matchRightParen(rightParen: Option[RightParen]) : Boolean = rightParen match {
    case Some(rp) => {
      matchConcat(rp.left)
    }
    case _ => false
  }

  def matchCharacterLiteral(characterLiteral: Option[CharacterLiteral]) = characterLiteral match {
    case Some(c) => if(candidate.charAt(candidateIndex) == c.content){
      true
    }
    else{
      false
    }
    case None => false
  }

  Concat(LeftBranch(LeftBranchTwo(LeftParen(Some(CharacterLiteral('a')),None),None),Some(RightBranch(LeftBranchTwo(LeftParen(Some(CharacterLiteral('b')),None),None),None))),Some(LeftOr(RightOr(LeftBranch(LeftBranchTwo(LeftParen(Some(CharacterLiteral('c')),None),None),Some(RightBranch(LeftBranchTwo(LeftParen(Some(CharacterLiteral('d')),None),None),None))),None))))

  val test = matchRegExp()
  println(test)
}
