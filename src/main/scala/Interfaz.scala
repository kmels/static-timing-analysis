
import scala.swing._
import scala.swing.event._
import java.awt.Color

object Interfaz  extends SimpleGUIApplication {
	
	def top = new MainFrame {
		// Configurar ventana
		title = "Justificator (v0.1)"
		
		// Agregar menu
		menuBar = barraMenu
			
		// Crear y configurar elementos graficos
		
		
		// Agregar elementos a la ventana 
		contents = new BoxPanel(Orientation.Horizontal) {
			// Agregar barra con botones
			contents += new BoxPanel(Orientation.Vertical) {
				contents += new Button("Not")
				contents += new Button("Or")
				contents += new Button("And")
				contents += new Button("Xor")
				contents += new Button("XAnd")
				//contents += new Separator()
				contents += new Button("Conexion")
			}
			// Agregar panel principal
			contents += new Panel() {
				background = Color.white
				border = Swing.EmptyBorder(0,3, 0, 0)
			}
			border = Swing.EmptyBorder(1, 15, 5, 30)
		}
		
		// Handlers para los elementos
		//listenTo()
		reactions += {
			case ButtonClicked(b) =>
				None
		}
	}
}

object barraMenu extends MenuBar {
	val archivo = new Menu("Archivo") {
		contents += new Menu("Nuevo") 
		contents += new Menu("Abrir") 
		contents += new Menu("Guardar")
		contents += new Menu("Guardar como...")
		contents += new Separator()
		contents += new Menu("Imprimir")
		contents += new Menu("Exportar") {
			contents += new Menu("Imagen")
		}
		contents += new Separator()
		contents += new Menu("Propiedades")
		contents += new Menu("Cerrar") 
	}
	val editar = new Menu("Editar") {
		contents += new Menu("Deshacer")
		contents += new Menu("Rehacer")
		contents += new Separator()
		contents += new Menu("Copiar")
		contents += new Menu("Cortar")
		contents += new Menu("Pegar")
	}
	val componente = new Menu("Componente") {
		contents += new Menu("Insertar componente") {
			contents += new Menu("Not")
			contents += new Menu("Or")
			contents += new Menu("And")
			contents += new Menu("Xor")
			contents += new Menu("Xand")
		}
		contents += new Separator()
		contents += new Menu("Seleccionar componente")
		contents += new Menu("Seleccionar todos")
		contents += new Separator()
		contents += new Menu("Eliminar componente")
		contents += new Menu("Eliminar componente seleccionado")
	}
	val herramientas = new Menu("Herramientas") {
		contents += new Menu("Mostrar rutas")
		contents += new Menu("Sensibilizar y justificar")
	}
	val ayuda = new Menu("Ayuda") {
		contents += new Menu("Acercad de Justificator")
	}
	
	// Agregar menus a la barra de menus
	contents ++= List(archivo, editar, componente, herramientas, ayuda)
}
