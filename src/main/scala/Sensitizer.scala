package main

trait Sensitizer {
  val inputNames:List[String]
  val gates:List[Gate]
  val outputGate:Gate
  
  lazy val routes:List[Route] = {
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

    getRoutesFrom(outputGate.name).zipWithIndex.map(routeWithIndex => Route(routeWithIndex._2,routeWithIndex._1))
  }

  case class Route(val number:Int,val route:List[String])
}
