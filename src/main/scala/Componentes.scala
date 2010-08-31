package ed.gui

import main._;
import scala.swing._
import scala.swing.event._

trait Componente {
	var posX : Int
	var posY : Int
	var name : String
}

class GraphicalCircuit(var inputNames:List[GraphicalInput],var gates:List[Gate], var outputName:String)

class GraphicalBinaryGate(override var name: String, var gInput1Name:String, val gInput2Name: String, val gDelaySpec:BinaryDelaySpec, val gOperationName:String, var posX : Int, var posY : Int) extends BinaryGate (name, gInput1Name, gInput2Name, gDelaySpec, gOperationName) with Componente {
	
}

class GraphicalUnaryGate(override var name: String, val gInputName:String, val gDelaySpec:UnaryDelaySpec, val gOperationName:String, var posX : Int, var posY : Int) extends UnaryGate (name, gInputName, gDelaySpec, gOperationName) with Componente {
	
}

class GraphicalInput(var name: String, var posX : Int, var posY : Int) extends Componente {
	
}

class ComponenteGrafico(val c : Componente) extends Component {
	var posX : Int = c.posX
	var posY : Int = c.posY
	var name : String = c.name
}
