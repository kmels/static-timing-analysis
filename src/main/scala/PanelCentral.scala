package ed.gui

import swing._
import scala.swing.Color
import scala.swing.event._
import java.awt.Color
import java.awt.BasicStroke

object panelCentral extends TabbedPane() {
	var lineasHorizontales : Boolean = false
	var lineasVerticales : Boolean = false
	var counter = 0
	
	def addPage = {
		val p = new TabbedPane.Page("Sin titulo ("+counter+")", new ScrollPane(new PanelPrincipal()))	
		counter += 1
		pages += p
		selection.page = p
	}
}

trait LineasVerticales extends Panel {
	override protected def paintComponent(g : Graphics2D) {
		super.paintComponent(g)
		if(panelCentral.lineasVerticales) {
			g setColor Color.gray
			g setStroke new BasicStroke( 1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 2, Array[Float](4f), 2 )
			for(x <- 0 until size.width by 15) g.drawLine(x, 0, x, size.height)
		}
	}
}

trait LineasHorizontales extends Panel {
	
	override protected def paintComponent(g : Graphics2D) {
		super.paintComponent(g)
		if(panelCentral.lineasHorizontales) {
			g setColor Color.gray
			g setStroke new BasicStroke( 1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 2, Array[Float](4f), 2 )
			for(y <- 0 until size.height by 15) g.drawLine(0, y, size.width, y)
		}
	}
}

class PanelPrincipal extends Panel
		with LineasHorizontales with LineasVerticales {
	background = Color.white
	border = Swing.EmptyBorder(1, 1, 1, 1)
}

