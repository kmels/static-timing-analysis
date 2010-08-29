package main

trait Justifier extends Sensitizer {
  implicit def stateToSet(state:State):Set[State] = Set(state)

  def justify(route:List[String],state:State):Set[State] = 
    state.find(checkedWire => checkedWire._2 == false) match { //find the first which hasn't been checked yet 
      case None => {	
	Set() //empty set
      }
      case Some(nonCheckedWire) => {	
	val wireName = nonCheckedWire._1._1
	val wireValue:Option[Boolean] = nonCheckedWire._1._2

	if (inputNames.contains(wireName)){
	  state.updated((wireName,wireValue),true) //mark the wire as checked
	}else{
	  val gate:Gate = gates.find(_.name == nonCheckedWire._1._1) match {
	    case Some(gate) => gate
	    case _ => {
	      println("Could not found gate for "+nonCheckedWire._1._1)
	      throw new Exception("Could not found gate for "+nonCheckedWire._1._1)
	    }
	  }
	  val gateValue:Option[Boolean] = nonCheckedWire._1._2
	  
	  gate match {
	    case binaryGate:BinaryGate => {
	      wireValue match{
		case Some(gateOutputValue) => {
		  val input1ShouldBe: (Boolean) => CheckedSensitizedWire = value => {
		    ((binaryGate.input1Name,Some(value)),false)
		  }

		  val input2ShouldBe: (Boolean) => CheckedSensitizedWire = value => ((binaryGate.input2Name,Some(value)),false)
		  val sensitizedGate:SensitizedWire = nonCheckedWire._1

		  binaryGate.operationName match{
		    case "AND" => {			  
		      gateOutputValue match{
			case true => //gate AND, with output 1. 
			  state.updated(sensitizedGate,true) + (
			    input1ShouldBe(true),input2ShouldBe(true)
			  )
			case _ => { //gate AND, with output 0,
			  atLeastOneInputMustBeFalse(binaryGate,route,sensitizedGate,state)
			}
		      }
		    }
		    case "OR" => 
		      gateOutputValue match{
			case true => {
			  atLeastOneInputMustBeTrue(binaryGate,route,sensitizedGate,state)
			}
			case _ => //OR output is 0, both inputs must be 0 as well
			  state.updated(sensitizedGate,true) + (
			    input1ShouldBe(false),input2ShouldBe(false)
			  )			
		      }//end OR
		    case "NOR" => 
		      gateOutputValue match{
			case false =>{
			  //NOR output is 0, at least one input must be true
			  atLeastOneInputMustBeTrue(binaryGate,route,sensitizedGate,state)
			}
			case _ => //NOR output is 1, both inputs must be 0
			  state.updated(sensitizedGate,true) + (
			    input1ShouldBe(false),input2ShouldBe(false)
			  )
		      } //END NOR
		    case "NAND" => 
		      gateOutputValue match{
			case true => {
			  atLeastOneInputMustBeFalse(binaryGate,route,sensitizedGate,state)
			}
			case _ => { //NAND output is 0, both inputs must be 1
			  state.updated(sensitizedGate,true) + (
			    input1ShouldBe(true),input2ShouldBe(true)
			  )
			}
		      }//END NAND
		  }
		} //end if wire value is Some(_)
		case _ => {
		  state.updated(nonCheckedWire._1,true) //go to the next state, it's ok
		}
	      }	      
	    }
	    case unaryGate:UnaryGate => unaryGate.operationName match {
	      case "NOT" => {
		val inputsValue:Option[Boolean] = gateValue match{
		  case None => None
		  case Some(value) => Some(!value)
		}
		//it's input has to be its complement
		val inputsWireValue:(SensitizedWire,Boolean) = ((unaryGate.inputName,inputsValue),false)
		  state.updated(nonCheckedWire._1,true) + inputsWireValue // mark as checked and add its input wire value conditional
	      } //end gate's operation name match
	    } //end unary gate	  	  	  
	  } //end gate match
	} //end else if it's not an input
	
	
      } // end Some(noncheckedwire)
    } //end next states

  private def atLeastOneInputMustBeFalse(binaryGate:BinaryGate,route:List[String],sensitizedGate:SensitizedWire,state:State):Set[State] ={
    val input1ShouldBe: (Boolean) => State = value => Map((binaryGate.input1Name,Some(value))->false)
    val input2ShouldBe: (Boolean) => State = value => Map((binaryGate.input2Name,Some(value))->false)

    //return the three possible states
    if (route.contains(binaryGate.input1Name)){
      state.updated(sensitizedGate,true) ++ input2ShouldBe(false)
    }else 
      if (route.contains(binaryGate.input2Name)){
	state.updated(sensitizedGate,true) ++ input1ShouldBe(false)
      }
      else{
	val state1:State = state.updated(sensitizedGate,true) ++ input1ShouldBe(false) ++ input2ShouldBe(true)
	val state2:State = state.updated(sensitizedGate,true) ++ input1ShouldBe(true) ++ input2ShouldBe(false)
	val state3:State = state.updated(sensitizedGate,true) ++ input1ShouldBe(false) ++ input2ShouldBe(false)
	Set(state1,state2,state3)
      }
  }

  private def atLeastOneInputMustBeTrue(binaryGate:BinaryGate,route:List[String],sensitizedGate:SensitizedWire,state:State):Set[State] ={
    val input1ShouldBe: (Boolean) => State = value => Map((binaryGate.input1Name,Some(value))->false)
    val input2ShouldBe: (Boolean) => State = value => Map((binaryGate.input2Name,Some(value))->false)

    //return the three possible states
    if (sensitizedWireSetsValueInRoute(route,binaryGate.input1Name)){
      state.updated(sensitizedGate,true) ++ input2ShouldBe(true)
    }else 
      if (sensitizedWireSetsValueInRoute(route,binaryGate.input2Name)){
	state.updated(sensitizedGate,true) ++ input1ShouldBe(true)
      }
      else{
	println("Ni miesh")
	val state1:State = state.updated(sensitizedGate,true) ++ input1ShouldBe(false) ++ input2ShouldBe(true)
	val state2:State = state.updated(sensitizedGate,true) ++ input1ShouldBe(true) ++ input2ShouldBe(false)
	val state3:State = state.updated(sensitizedGate,true) ++ input1ShouldBe(true) ++ input2ShouldBe(true)
	Set(state1,state2,state3)
      }
  }

  private def sensitizedWireSetsValueInRoute(route:List[String],inputName:String):Boolean = 
    if (route.contains(inputName))
      true
    else{
      println(route)

      println("input name: "+inputName)

      println(gates.find(_.name == inputName))

      gates.find(_.name == inputName) match{
	case Some(gate) => gate match{
	  case unaryGate:UnaryGate => {
	    println (unaryGate)
	    sensitizedWireSetsValueInRoute(route,unaryGate.inputName)
	  }
	  case _ => {
	    println("false")
	      false
	  }	  
	}
	case _ => false
      }
    }// end else
}
