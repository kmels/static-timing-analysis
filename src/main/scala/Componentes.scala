package ed.gui

import main._;
import scala.swing._
import scala.swing.event._

class GraphicalBinaryGate(var gName: String, var gInput1Name:String, val gInput2Name: String, val gDelaySpec:BinaryDelaySpec, val gOperationName:String, var posX : Int, var posY : Int) extends BinaryGate (gName, gInput1Name, gInput2Name, gDelaySpec, gOperationName) {
	
}

class GraphicalUnaryGate(val gName: String, val gInputName:String, val gDelaySpec:UnaryDelaySpec, val gOperationName:String, var posX : Int, var posY : Int) extends UnaryGate (gName, gInputName, gDelaySpec, gOperationName) {
	
}

class GraphicalInput(var name: String, var posX : Int, var posY : Int) {
	
}