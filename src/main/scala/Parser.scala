package main

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object Parser extends StandardTokenParsers {
  lexical.reserved ++= Set("inputs","output","OR","AND","NAND","NOR","NOT","input","output")
  lexical.delimiters ++= Set("=","/",",")

  def circuit:Parser[Circuit] = inputs ~ rep1(gate) ~ output ^^ {
    case inputNames ~ gates ~ outputName => Circuit(inputNames,gates,outputName)
  }

  def inputs:Parser[List[String]] = "inputs" ~> "="~> rep1sep(ident,",") 

  def output:Parser[String] = "output" ~> "=" ~> ident

  def gate:Parser[Gate] = binaryGate | unaryGate

  //e.g. AND,OR,NAND,NOR
  def binaryGate:Parser[BinaryGate] = ident ~ "=" ~ ident ~ binaryOperation ~ ident ~ delaySpecs ^^ {
    case name ~ "=" ~ input1Name ~ operationName ~ input2Name ~ delays => BinaryGate(name,input1Name,input2Name,delays,operationName)
  }
  
  //i.e. NOT
  def unaryGate:Parser[UnaryGate] = ident ~ "=" ~ unaryOperation ~ ident ~ singleDelaySpec ^^ {
    case name ~ "=" ~ operationName ~ inputName ~ delaySpec => UnaryGate(name,inputName,UnaryDelaySpec(delaySpec),operationName)
  }

  //delay specs for 
  def delaySpecs:Parser[BinaryDelaySpec] = singleDelaySpec ~ singleDelaySpec ^^ {
    case input1DelaySpec ~ input2DelaySpec => BinaryDelaySpec(input1DelaySpec,input2DelaySpec)
  }

  //(Rising,Falling) delays
  def singleDelaySpec:Parser[(Int,Int)] = numericLit ~ "/" ~ numericLit ^^ { 
    case risingDelay ~ "/" ~ fallingDelay => (risingDelay.toInt,fallingDelay.toInt)
  }

  def binaryOperation = "OR"|"AND"|"NAND"|"NOR"

  def unaryOperation = "NOT"

  def parse(s:String):Option[Circuit] = {
    val tokens = new lexical.Scanner(s)
    circuit(tokens) match {
      case Success(circuit,_) => Some(circuit)
      case _ => {
	None
      }
    }
  }
}

case class Circuit(val inputNames:List[String],val gates:List[Gate], val outputName:String) extends Sensitizer{
  val outputGate:Gate = gates.find(_.name == outputName) match{
    case Some(gate) => gate
    case _ => throw InputError("Output gate \""+outputName+"\" wasn't specified")
  }
}

trait Gate{
  val name:String
  val delaySpec:DelaySpec
}

trait Operation{
  val operationName:String
}

case class BinaryGate(val name: String, val input1Name:String, val input2Name: String, val delaySpec:BinaryDelaySpec, val operationName:String) extends Gate with Operation

case class UnaryGate(val name: String, val inputName:String, val delaySpec:UnaryDelaySpec, val operationName:String) extends Gate with Operation

trait DelaySpec

case class BinaryDelaySpec(val input1Delays:(Int,Int), val input2Delays:(Int,Int)) extends DelaySpec

case class UnaryDelaySpec(val inputDelays:(Int,Int)) extends DelaySpec
