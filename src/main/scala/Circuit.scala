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
  case class Route(val number:Int,val path:List[String], val isValid:Boolean){
    private val sensitizedPathWires:Map[String,Option[Boolean]] = sensitize(path).keys.toMap

    private val gateFunctions: Map[String,Boolean => Boolean] = path.map(
      wireName =>
	if (inputNames.contains(wireName)){
	  val function: Boolean => Boolean = bValue => bValue
	  (wireName,function)
	}	  
	else
	  gates.find(_.name == wireName) match{
	    case Some(gate) => {
	      val sensitizedWireName:Option[String] = 
		gate match{
		  case binaryGate:BinaryGate => 
		    if (binaryGate.input1Name == wireName)
		      Some(binaryGate.input2Name)
		    else
		      Some(binaryGate.input1Name)
		  case _ => //unary gate 
		    None
		}
	      
	      //only binary gates have a sensitized value for one of their inputs
	      val booleanFunction:Boolean => Boolean = sensitizedWireName match{
		case Some(sensitizedWireName) => {
		  //its a binary gate
		  val sensitizedValue:Boolean = sensitizedPathWires(sensitizedWireName).get
		  val booleanFunction: Boolean => Boolean = gate.operationName match {
		    case "AND" => bValue => sensitizedValue && bValue
		    case "OR" => bValue => sensitizedValue | bValue
		  }
		  booleanFunction
		}
		case _ => {//its a NOT gateb
		  val booleanFunction:Boolean => Boolean = bValue => !bValue
		  booleanFunction
		} 
	      } //end sensitizedWireName
	      (wireName,booleanFunction)
	    } //end Some(gate)
	    case _ => throw new Exception("Error in route#gateFunctions")	    
	  } //end gates.find
    ).toMap

    //pendiente
    private val delay: Boolean => Int = inputValue => path.foldLeft[(Boolean,Int)](inputValue,0)(
      (inputValue,delaySum) => {
	(true,0)
      }
    )._2
    
    //pendiente
    val propagationVector:Map[String,Boolean] = Map()
    //pendiente: (falling delay,rising delay)
    val delays:(Int,Int) = (delay(false),delay(true))
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
      routeWithIndex => 
	Route(
	  routeWithIndex._2,
	  routeWithIndex._1,
	  isTruthful(routeWithIndex._1)
	)
    )
  }
  
  /**
   * Checks if a route is valid
   * 
   * @param route the list of gates in order
   * @returns true if the route is valid within this circuit
   */
  private def isTruthful(route:List[String]):Boolean = {
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
    
    justifiedStates.size > 0
  }
}
