package main

trait Sensitizer {
  type SensitizedWire = (String,Option[Boolean]) // wirename,non-controllinv value
  type CheckedSensitizedWire = (SensitizedWire,Boolean) // sensitized wire, is checked
  type State = Map[SensitizedWire,Boolean]  // wire -> is justified

  val inputNames:List[String]
  val gates:List[Gate]
  val outputGate:Gate

  def sensitize(route:List[String]):State = {
    println("Sensetizando: "+route)
    val binaryGatesInRoute:List[BinaryGate] = route.map(gateName => gates.find(_.name == gateName)).flatten.flatMap({
      case binaryGate:BinaryGate => Some(binaryGate)
      case _ => None
    })      
    //get gates' inputs that are not in the route, i.e. to sensitize later
    val inputsToSentitize:List[String] = binaryGatesInRoute.map(gate => {
      if (route.contains(gate.input1Name))
	gate.input2Name
      else{
	gate.input1Name
      }
    })	
    
    val r = binaryGatesInRoute.zip(inputsToSentitize).map(zippedGateWithInputs => {
      val sensitizedWire:SensitizedWire = (zippedGateWithInputs._2,zippedGateWithInputs._1.propagationValue)
	val justifiedWire:(SensitizedWire,Boolean) = (sensitizedWire,false)
	  justifiedWire
    }).toMap

    println("valores: "+r.keys.toMap)

    r
  } //end sensitize
}
