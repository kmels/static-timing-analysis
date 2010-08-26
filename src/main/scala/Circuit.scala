package main

case class Circuit(val inputNames:List[String],val gates:List[Gate], val outputName:String) extends Justifier {  
  val outputGate:Gate = gates.find(_.name == outputName) match{
    case Some(gate) => gate
    case _ => throw InputError("Output gate \""+outputName+"\" wasn't specified")
  }


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
    
    val routes = getRoutesFrom(outputGate.name)

    val ruta = routes(1)
    //    println("Ruta 1: "+ruta)
    //   println("Ruta es valida: "+isValid(ruta))
    //println("RUTA 0: "+isValid(routes(0)))
    

    routes.zipWithIndex.foreach(
      r => {
	val index = r._2
	val route = r._1
	println("RUTA "+index+" valida: "+isValid(route)+" \n\tRUTA: "+route+"\n\n")
      }
    )
    /*    val merasMeras:List[Route] = routes.zipWithIndex.map(
     routeWithIndex => 
     Route(
     routeWithIndex._2,
     routeWithIndex._1,
     isValid(routeWithIndex._1)
     )
     )*/

    //println("Meras meras: "+merasMeras.mkString("\n,"))
    //merasMeras
    List()
  }



  /**
   * Checks if a route is valid
   * 
   * @param route the list of gates in order
   * @returns true if the route is valid within this circuit
   */
  private def isValid(route:List[String]):Boolean = {
    import collection.mutable.ListBuffer
    val uncheckedStates = ListBuffer(sensitize(route)) //initial states is the sensitized values for this route
    val justifiedStates = ListBuffer[State]()
    
    /**
     * returns true if all the wires have been justified
     */ 
    def isFinalState(state:State):Boolean = state.valuesIterator.forall(_ == true)
    
    /**
     * @returns true if there exists some incoherent value
     */ 
    def isInvalidState(state:State):Boolean = {
      val allWires:List[String] = state.keysIterator.toList.map(_._1)
      //there has to be only one value for a wire
      allWires.diff(allWires.toSet.toSeq) != Nil //if there's at least one repeated
    }
    
    while (!uncheckedStates.isEmpty){ //while there is no state left to check
      val newStates = nextStates(uncheckedStates.remove(0)) //get next states
      
      val (invalidStates,validStates) = newStates.partition( s => isInvalidState(s) )
      //we don't care about invalid states anymore, but there could be one final state (or more) within the valid states
      val (finalStates,justValidStates) = validStates.partition(isFinalState(_))

      println("encontro "+ finalStates.size +" estados finales")
      justifiedStates ++= finalStates
      uncheckedStates ++= justValidStates
    }
    
    println("en total: "+justifiedStates.size+" estados finales")
    justifiedStates.size > 0
  }
}
