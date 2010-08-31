package main

trait Gate {
  val isBinary:Boolean
  val name:String
  val delaySpec:DelaySpec
  val propagationValue:Option[Boolean]
  val operationName:String
}

trait Operation{
  val operationName:String

  val propagationValue:Option[Boolean] = operationName match{
    case "OR" => Some(false)
    case "AND" => Some(true)
    case "NAND" => Some(true)    
    case "NOR" => Some(false)
    case "NOT" => None
  }
}

case class BinaryGate(val name: String, val input1Name:String, val input2Name: String, val delaySpec:BinaryDelaySpec, val operationName:String) extends Gate with Operation{
  val isBinary = true
}

case class UnaryGate(val name: String, val inputName:String, val delaySpec:UnaryDelaySpec, val operationName:String) extends Gate with Operation{
  val isBinary = false
}

trait DelaySpec

case class BinaryDelaySpec(val input1Delays:(Int,Int), val input2Delays:(Int,Int)) extends DelaySpec

case class UnaryDelaySpec(val inputDelays:(Int,Int)) extends DelaySpec
