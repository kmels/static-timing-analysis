package main

case class Circuit(val inputNames:List[String],val gates:List[Gate], val outputName:String) extends Justifier {  
  val outputGate:Gate = gates.find(_.name == outputName) match{
    case Some(gate) => gate
    case _ => throw InputError("Output gate \""+outputName+"\" wasn't specified")
  }
  
  //pendiente
  //val criticalPath

  /*
   * @param number the id of the route
   * @param the route's path
   * @param isValid wether is truthful or not.
   * abstraction for a route
   */
  case class Route(val number:Int,val path:List[String], val isValid:Boolean, finalState:Option[State]){    
    //wirename => sensitized wire value
    private val sensitizedPathWires:Map[String,Option[Boolean]] = sensitize(path).keys.toMap

    //wire name -> function application, used for computing both rising and falling delays
    val gateFunctions: Map[String,Boolean => Boolean] = path.map(
      wireName =>
	if (inputNames.contains(wireName)){
	  val booleanFunction: Boolean => Boolean = bValue => bValue
	  (wireName,booleanFunction)
	}	  
	else
	  gates.find(_.name == wireName) match{
	    case Some(gate) => {
	      //None if it's unary (it shouldn't have a sensitized value)
	      val sensitizedWireName:Option[String] = 
		gate match{
		  case binaryGate:BinaryGate => 
		    if (path.contains(binaryGate.input1Name))
		      Some(binaryGate.input2Name)
		    else
		      Some(binaryGate.input1Name)
		  case _ => //unary gate 
		    None
		}	     
	      //only binary gates have a sensitized value for one of their inputs
	      val booleanFunction:Boolean => Boolean = sensitizedWireName match{
		case Some(sensitizedWireName) => {
		  //its a binary gate or an input
		  sensitizedPathWires.get(sensitizedWireName).get match{
		    case Some(sensitizedValue) => { 
		      val booleanFunctionForSensitizedWire:Boolean => Boolean = 
			if (!inputNames.contains(sensitizedWireName))//its a gate
			  gate.operationName match {
			    case "AND" => bValue => sensitizedValue && bValue
			    case "OR" => bValue => sensitizedValue | bValue
			    case "NAND" => bValue => !(sensitizedValue && bValue)
			    case "NOR" => bValue => !(sensitizedValue | bValue)
			  }
			else
			  //its an input
			  bValue => bValue
		      
		      booleanFunctionForSensitizedWire
		    } //end Some(sensitizedValue)
		    case _ => {
		      val b:Boolean => Boolean = b => b
		      b
		    }
		  }		  
		} //end Some(sensitized value)
		case _ => {//its a "NOT" gate
		  val booleanFunction:Boolean => Boolean = bValue => !bValue
		  booleanFunction
		} 
	      } //end sensitizedWireName
	      (wireName,booleanFunction)
	    } //end Some(gate)
	    case _ => throw new Exception("Error in route#gateFunctions: could not found gate")	    
	  } //end gates.find
    ).toMap

    //pendiente
    private val delay: Boolean => Int = inputValue => {
      var sum = 0
      var result = inputValue

      path.tail.foreach(
	wireName => {
	  result = gateFunctions(wireName)(result)	  
	  val gate = gates.find(_.name == wireName).get	  
	  val (risingDelayToSum,fallingDelayToSum) = gate match{
	    case binaryGate:BinaryGate => {
	      if (path.contains(binaryGate.input1Name))
		binaryGate.delaySpec.input1Delays
	      else
		binaryGate.delaySpec.input2Delays
	    }
	    case unaryGate:UnaryGate => unaryGate.delaySpec.inputDelays
	  }
	  
	  val toSum:Int = result match{
	    case true => risingDelayToSum
	    case _ => fallingDelayToSum
	  }
	  sum += toSum
	}//end wireName
      )
      sum
    }
       
    val fallingPropagationVector:Map[String,Option[Boolean]] = finalState match{
      case Some(state) => {
	println("state: "+state)	
	val stateInputValues = state.keys.toMap
	println("uno: "+stateInputValues.filter(inputNames.contains(_)))
	stateInputValues.filter(inputNames.contains(_)) ++ inputNames.filterNot(stateInputValues.contains(_)).map(inputName => (inputName,None)).toMap
      }
      case _ => Map()
    }
    val risingPropagationVector:Map[String,Option[Boolean]] = finalState match{
      case Some(state) => {
	val stateInputValues = state.keys.toMap
	stateInputValues.filter(inputNames.contains(_)) ++ inputNames.filterNot(stateInputValues.contains(_)).map(inputName => (inputName,None)).toMap
      }
      case _ => Map()
    }
    val fallingDelay:Int = delay(false)
    val risingDelay:Int = delay(true)

    override def toString = "Route: id="+number+", path="+path.mkString(",")+", valid="+isValid+", falling,rising delays="+fallingDelay+","+risingDelay+", (rising propagation vector, falling propagation vector="+(risingPropagationVector,fallingPropagationVector)+";"
  }
  /*
   * Computes the routes for this circuit
   *
   */
  val routes:List[Route] = {
    def getRoutesFrom(id:String):List[List[String]] ={      
      if (inputNames.contains(id))
	List(Nil:+id)
      else {
	gates.find(_.name == id) match{
	  case Some(UnaryGate(name,inputName,_,_)) => getRoutesFrom(inputName).map(_ :+ name)
	  case Some(BinaryGate(name,input1Name,input2Name,_,_)) => getRoutesFrom(input1Name).map(_ :+ name) ++ getRoutesFrom(input2Name).map(_ :+ name)
	  case None => throw InputError("gate \""+id+"\" not found")
	}
      }
    }
    
    val paths = getRoutesFrom(outputGate.name)
    paths.zipWithIndex.map(
      routeWithIndex => {
	val truthfulResult = isTruthful(routeWithIndex._1)
	Route(
	  routeWithIndex._2,
	  routeWithIndex._1,
	  truthfulResult._1,
	  truthfulResult._2
	)
      }
    )
  }
  
  /**
   * Checks if a route is valid
   * 
   * @param route the list of gates in order
   * @returns true if the route is valid within this circuit and the optional final state
   */
  private def isTruthful(route:List[String]):(Boolean,Option[State]) = {
    import collection.mutable.ListBuffer
    val uncheckedStates = ListBuffer(sensitize(route)) //initial states is the sensitized values for this route
    val justifiedStates = ListBuffer[State]()
    
    /**
     * returns true if all the wires have been justified
     */ 
    def isFinalState(state:State):Boolean = state.valuesIterator.forall(_ == true)
    
    /**
     * @returns true if there exists some incoherent value or there exists a wire in the route whose value is set
     */ 
    def isInvalidState(path:List[String],state:State):Boolean = {
      val allWires:List[String] = state.keysIterator.toList.map(_._1)
      //there has to be only one value for a wire
      val hasIncoherences = allWires.diff(allWires.toSet.toSeq) != Nil //if there's at least one repeated
      //if it sets a value for a wire in the route (path), it's invalid too, i.e. at least one sensitized wire sets value for a wire in the path
      val valueSetInPath = allWires.filter(route.contains(_)).size >0
      
      hasIncoherences | valueSetInPath
    }
    
    while (!uncheckedStates.isEmpty){ //while there is no state left to check
      val newStates = justify(route,uncheckedStates.remove(0)) //get next states
      
      val (invalidStates,validStates) = newStates.partition( s => isInvalidState(route,s) )
      //we don't care about invalid states anymore, but there could be one final state (or more) within the valid states

      val (finalStates,justValidStates) = validStates.partition(isFinalState(_))
      justifiedStates ++= finalStates
      uncheckedStates ++= justValidStates
    }
    
      if (justifiedStates.size > 0)
	(justifiedStates.size > 0,Some(justifiedStates.head))
      else
	(justifiedStates.size > 0,None)
  }
}
