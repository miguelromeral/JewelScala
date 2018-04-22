package source

import scala.swing._
import scala.swing.event._
import scala.swing.ComboBox.intEditor
import javax.swing.JFrame
import javax.swing.UIManager
import javax.swing.LookAndFeel
import javax.swing.UIManager.LookAndFeelInfo
import source.Main._
import java.awt.Dimension
import java.awt.Toolkit

//Pide los datos de un nuevo juego y genera el tablero.
class Iniciar extends Frame {
  val f = new Frame {
    //Obtenemos el ancho y el alto de la pantalla del ordenador para poder centrar el frame
    val wf = 400
    val hf = 180
    val screenSize: Dimension = Toolkit.getDefaultToolkit().getScreenSize();
    val width = screenSize.getWidth().toInt / 2 - (wf / 2)
    val height = screenSize.getHeight().toInt / 2 - (hf / 2)
    peer.setLocation(width, height)
    //Título de la ventana.
    title = "Scala Jewel's Legends"
    //Dimensinoes de la ventana.
    preferredSize = new Dimension(wf, hf)
    //La aplicación se cierra cuando se cierre la ventana.
    peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    //Valores de la dificultad.
    val dificultad = List(
      "1 - Tablero 7x9, con 4 tipos de diamantes",
      "2 - Tablero 11x17, con 5 tipos de diamantes",
      "3 - Tablero 15x27, con 7 tipos de diamantes")
    //Caja de opciones con la dificultad.
    val comboBoxDificultad = new ComboBox(dificultad)
    //Valores del modo de juego.
    val modo = List(textoCajaManual, "Automático")
    //Caja de texto del modo
    val comboBoxModo = new ComboBox(modo)
    //Etiqueta para la dificultad.
    val labelDificultad = new Label {
      text = "Seleccione la dificultad:"
    }
    //Etiqueta para el modo de juego.
    val labelModoJuego = new Label {
      text = "Seleccione el modo de juego:"
    }
    //Botón para inciar el juego.
    val buttonEmpezar = new Button {
      text = "Empezar el juego"
    }
    //Contenidos del panel: las etiquetas, las cajas y el botón, además de un borde por los lados.
    contents = new BoxPanel(Orientation.Vertical) {
      contents += labelDificultad
      contents += comboBoxDificultad
      contents += labelModoJuego
      contents += comboBoxModo
      contents += buttonEmpezar
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
    //Escucharemos las acciones del botón que empieza el juego.
    listenTo(buttonEmpezar)
    reactions += {
      //En caso de que se pulse el botón
      case ButtonClicked(b) => {
        //Se obtiene la dificultad y el modo de juego.
        val dificultadDeCaja: String = comboBoxDificultad.item
        val dif: Int = dificultadDeCaja.toCharArray()(0).asDigit
        val modo: String = comboBoxModo.item
        //Se obtienen las dimensiones del tablero en una lista. Col :: Fil :: Nil
        val dimensiones = ScalaLegends.poner_dimensiones_teclado(dif)
        //Se pasan esas dimensiones a variables.
        val col = dimensiones(0)
        val fil = dimensiones(1)
        //Se recogen los diferentes tipos de diamantes según la dificultad.
        val diamantes = ScalaLegends.coger_diamantes(dif)
        //Se genera un tablero al azar.
        val tablero = ScalaLegends.generar(col, fil, diamantes)
        //Indicamos que se eliminen los conjuntos automáticamente antes de realizar un movimiento.
        val eliminarConjuntosEnCasoDeQueLosHubiese = true
        //Crearemos el frame únicamente cuando no haya conjuntos en la situación actual, de eso se encarga crearFrame.
        ScalaLegends.crearFrame(0, 0, col, fil, dif, modo, tablero, diamantes, eliminarConjuntosEnCasoDeQueLosHubiese)
        //Cerramos la ventana.
        this.dispose()
      }
    }
    //Hacemos visible esta ventana.
    visible = true
  }
}