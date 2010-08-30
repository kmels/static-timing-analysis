package ed.gui

import main._
import scala.swing._
import scala.swing.event._
import java.awt.Color

object Interfaz  extends SimpleSwingApplication {
	
	def top = new MainFrame {
		// Configurar ventana
		title = "Justificator (v0.1)";
		
		// Agregar menu
		menuBar = barraMenu;
			
		// Crear y configurar elementos graficos
		
		
		// Agregar elementos a la ventana 
		contents = new BoxPanel(Orientation.Horizontal) {
			//contents += barraMenu.archivo
			
			// Agregar barra con botones
			contents += new BoxPanel(Orientation.Vertical) {
				contents += new Button("Seleccionar")
				contents += new Separator()
				contents += new Button("Agregar entrada")
				contents += new Separator()
				contents += new Button("Not")
				contents += new Button("Or")
				contents += new Button("And")
				contents += new Button("Nor")
				contents += new Button("NAnd")
				contents += new Separator()
				contents += new Button("Conexion")
			}
			
			// Agregar panel principal
			contents += panelCentral
			
			// Configurar ventana
			border = Swing.EmptyBorder(1, 15, 5, 30)
		}
		
		
		// Handlers para los elementos
		//listenTo()
		reactions += {
			case ButtonClicked(b) =>
				None
		}
		
		// Configurar elementos
		size = new Dimension(800,600)
		
	}
}

object barraMenu extends MenuBar {
	// Crear utilitarios
	import java.awt.event.{ KeyEvent, InputEvent }
	import javax.swing.{ KeyStroke, ImageIcon }

	val archivo = new Menu("Archivo") {
		val nuevo = new MenuItem( new Action("Nuevo") {
		mnemonic = KeyEvent.VK_N
		accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.CTRL_DOWN_MASK))
		icon = new ImageIcon("./data/images/app/document-new.png")
		toolTip = "Crea un documento nuevo."
		def apply() {
			//Interfaz.top.pnlCentral.pnlPrincipal. pages += new TabbedPane.Page("Sin titulo", new PanelCentral())
			panelCentral.addPage
		}
		})
		val abrir = new MenuItem(accionAbrir) 
		val guardar = new MenuItem(accionGuardar)
		val guardarComo = new MenuItem(accionGuardarComo)
		val imprimir = new MenuItem("Imprimir")
		val exportar = new Menu("Exportar") {
			val imagen = new MenuItem("Imagen") 
            contents += imagen
		}
		val propiedades = new MenuItem("Propiedades")
		val cerrar = new MenuItem("Cerrar")
		contents += nuevo
        contents += abrir
        contents += guardar
        contents += guardarComo
        contents += new Separator()
        contents += imprimir
        contents += exportar
        contents += new Separator()
        contents += propiedades
        contents += cerrar
	}
	val editar = new Menu("Editar") {
		contents += new MenuItem("Deshacer")
		contents += new MenuItem("Rehacer")
		contents += new Separator()
		contents += new MenuItem("Copiar")
		contents += new MenuItem("Cortar")
		contents += new MenuItem("Pegar")
	} 
	val vista = new Menu("Vista") {
		val linHor : CheckMenuItem = new CheckMenuItem ("Lineas horizontales") {
			action = new Action("Lineas horizontales") {
				def apply = {
					panelCentral.lineasHorizontales = linHor.peer.isSelected
					panelCentral.repaint()
				}
			}
		}
		val linVer : CheckMenuItem = new CheckMenuItem ("Lineas verticales") {
			action = new Action("Lineas verticales") {
				def apply = {
					panelCentral.lineasVerticales = linVer.peer.isSelected
					panelCentral.repaint()
				}
			}
		}
		
        contents += linHor
        contents += linVer
	}
	val componente = new Menu("Componente") {
		contents += new MenuItem("Insertar entrada")
		contents += new Menu("Insertar componente") {
			contents += new MenuItem("Not")
			contents += new MenuItem("Or")
			contents += new MenuItem("And")
			contents += new MenuItem("Xor")
			contents += new MenuItem("Xand")
		}
		contents += new Separator()
		contents += new MenuItem("Seleccionar componente")
		contents += new MenuItem("Seleccionar todos")
		contents += new Separator()
		contents += new MenuItem("Eliminar componente")
		contents += new MenuItem("Eliminar componente seleccionado")
	}
	val herramientas = new Menu("Herramientas") {
		contents += new MenuItem("Mostrar rutas")
		contents += new MenuItem("Sensibilizar y justificar") {
			val pathToFile = "/home/kmels/tmp/ie2005_3"
			val c:Option[Circuit] = Parser.parse(io.Source.fromFile(pathToFile).mkString)
			println(c)
			println(c.get.routes)
		}
	}
	val ayuda = new Menu("Ayuda") {
		contents += new MenuItem("Acercad de Justificator")
	}
	
	// Agregar menus a la barra de menus
	contents += archivo
	contents += editar
	contents += vista
	contents += componente
	contents += herramientas
	contents += ayuda
}
