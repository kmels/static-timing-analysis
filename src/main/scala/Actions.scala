package ed.gui

import main._
import scala.swing._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import java.awt.event.{ KeyEvent, InputEvent }
import javax.swing.{ KeyStroke, ImageIcon }

object Creador extends StandardTokenParsers {
  lexical.reserved ++= Set("inputs","output","OR","AND","NAND","NOR","NOT","input","output")
  lexical.delimiters ++= Set("=","/",",", "-")

  def circuit:Parser[GraphicalCircuit] = inputs ~ rep1(gate) ~ output ^^ {
    case inputNames ~ gates ~ outputName => new GraphicalCircuit(inputNames,gates,outputName)
  }

  def inputs:Parser[List[GraphicalInput]] = "inputs" ~> "="~> rep1sep(input,",") 
  
  def input:Parser[GraphicalInput] = ident ~ "-" ~ numericLit ~ "-" ~ numericLit ^^ {
	case name ~ "-" ~ posX ~ "-" ~ posY => new GraphicalInput(name, posX.toInt, posY.toInt)
  }

  def output:Parser[String] = "output" ~> "=" ~> ident

  def gate:Parser[Gate] = binaryGate | unaryGate

  //e.g. AND,OR,NAND,NOR
  def binaryGate:Parser[GraphicalBinaryGate] = ident ~ "=" ~ ident ~ binaryOperation ~ ident ~ delaySpecs ~ numericLit ~ "-" ~ numericLit ^^ {
    case name ~ "=" ~ input1Name ~ operationName ~ input2Name ~ delays ~ posX ~ "-" ~ posY => new GraphicalBinaryGate(name,input1Name,input2Name,delays,operationName, posX.toInt, posY.toInt)
  }
  
  //i.e. NOT
  def unaryGate:Parser[GraphicalUnaryGate] = ident ~ "=" ~ unaryOperation ~ ident ~ singleDelaySpec ~ numericLit ~ "-" ~ numericLit ^^ {
    case name ~ "=" ~ operationName ~ inputName ~ delaySpec ~ posX ~ "-" ~ posY => new GraphicalUnaryGate(name,inputName,UnaryDelaySpec(delaySpec),operationName, posX.toInt, posY.toInt)
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

  def parse(s:String):Option[GraphicalCircuit] = {
    val tokens = new lexical.Scanner(s)
    circuit(tokens) match {
      case Success(circuit,_) => Some(circuit)
      case f => {println(f); None}
    }
  }
}

object accionAbrir extends Action("Abrir") {
	mnemonic = KeyEvent.VK_O
	accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_DOWN_MASK))
	icon = new ImageIcon("./data/images/app/document-open.png")
	toolTip = "Abre un documento existente."
	def apply() {
		
		// Abrir el archivo
		val archivo = panelCentral.fc.showOpenDialog(panelCentral)
		// Revisar decision
		if(archivo == FileChooser.Result.Approve) {
			// Crear arbol del circuito
			val c:Option[GraphicalCircuit] = Creador.parse(io.Source.fromFile(panelCentral.fc.selectedFile).mkString)
			
			if(!c.isDefined) {
				Dialog.showMessage(null, "El archivo especificado no contiene un circuito valido.", "Error", Dialog.Message.Error)
				
			} else {
				println(c.get)
				
			}
		}
	}
}
object accionGuardar extends Action("Guardar") {
	mnemonic = KeyEvent.VK_S
	accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, InputEvent.CTRL_DOWN_MASK))
	icon = new ImageIcon("./data/images/app/document-save.png")
	toolTip = "Guarda el documento abierto."
	def apply() {
		println("guardar")
	}
}
object accionGuardarComo extends Action("Guardar como...") {
	mnemonic = KeyEvent.VK_S
	accelerator = Some(KeyStroke.getKeyStroke (KeyEvent.VK_S, InputEvent.CTRL_DOWN_MASK + InputEvent.SHIFT_DOWN_MASK))
	icon = new ImageIcon("./data/images/app/document-save-as.png")
	toolTip = "Guarda una copia del documento abierto."
	def apply() {
		println("guardar como")
	}
}
