package ed.gui

import main.Parser
import scala.swing.Action
import java.awt.event.{ KeyEvent, InputEvent }
import javax.swing.{ KeyStroke, ImageIcon }

object Creador extends main.Parser {
	
}

object accionAbrir extends Action("Abrir") {
	mnemonic = KeyEvent.VK_O
	accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_DOWN_MASK))
	icon = new ImageIcon("./data/images/app/document-open.png")
	toolTip = "Abre un documento existente."
	def apply() {
		//val fc = FileChooser
		println("abrir")
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
