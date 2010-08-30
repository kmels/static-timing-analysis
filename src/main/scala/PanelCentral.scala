import swing._
import scala.swing.Color
import scala.swing.event._
import java.awt.Color

object panelCentral extends TabbedPane() {
	var lineasHorizontales : Boolean = false
	var lineasVerticales : Boolean = false
	
	def addPage = 
		pages += new TabbedPane.Page("Sin titulo", new PanelPrincipal())	
}

//trait

class PanelPrincipal extends Panel {
	background = Color.white
	border = Swing.EmptyBorder(0,3, 0, 0)
}

