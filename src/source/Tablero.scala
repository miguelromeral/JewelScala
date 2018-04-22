package source

import java.awt.Color
import java.awt.Image
import java.awt.Toolkit

import scala.swing.BorderPanel
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Dimension
import scala.swing.Frame
import scala.swing.GridPanel
import scala.swing.Label
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.ScrollPane
import scala.swing.Swing
import scala.swing.event.ButtonClicked

import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JOptionPane
import source.ScalaLegends.backtracking
import source.ScalaLegends.ejecutar_movimiento
import source.ScalaLegends.get_posicion
import source.ScalaLegends.get_puntos
import source.ScalaLegends.imprimir
import source.ScalaLegends.jugada_posible
import source.ScalaLegends.limpiar_conjuntos_del_tablero
import source.ScalaLegends.mover_hacia_abajo
import source.ScalaLegends.movimiento_valido
import source.ScalaLegends.movimientos_situacion_actual
import source.ScalaLegends.obtener_la_otra_casilla
import source.ScalaLegends.posicion_mejor_jugada
import source.Main._
import java.awt.PageAttributes.ColorType

//Imprime el tablero y permite movimientos al usuario.
class Tablero(pts: Int, nmovs: Int, col: Int, fil: Int, dificultad: Int, modo: String, tableroInicial: List[Int], diamantes: List[Int], eliminarConjuntosEnCasoDeQueLosHubiese: Boolean) extends Frame {
  val f = new Frame {
    //Obtenemos el ancho y el alto de la pantalla del ordenador para poder centrar el frame
    val siz = 700
    val screenSize: Dimension = Toolkit.getDefaultToolkit().getScreenSize();
    val width = screenSize.getWidth().toInt / 2 - (siz / 2)
    val height = screenSize.getHeight().toInt / 2 - (siz / 2)
    peer.setLocation(width, height)
    //Tamaño de la ventana
    preferredSize = new Dimension(siz, siz)
    //Si no hay más combinaciones posibles al iniciar el juego, se advierte del fin del juego y se finaliza la ejecución
    if (!ScalaLegends.backtracking(col, fil, tableroInicial)) {
      JOptionPane.showMessageDialog(null, "¡No le quedan más movimientos! :(\nEnhorabuena, su puntuación es de " + pts + " tras haber realizado " + nmovs + " movimientos.\n"
        + "Tenga en cuenta que el tablero que aparece por pantalla pertenece al último movimiento, por lo que se aprecia el último movimiento válido realizado.", "Fin del juego", JOptionPane.ERROR_MESSAGE)
      System.exit(0)
    }
    //Se deterimna que la acción por defecto al cerrar la ventana sea el cierre de la app.
    peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    //Título del swing.
    title = "Scala Jewel's Legends"
    //Etiqueta que indica la acción a realizar por el usuario.
    val accion = new Label {
      if (modo.equals(Main.textoCajaManual)) {
        text = "Seleccione una casilla y su vecina para permutarlas si hay una jugada posible."
      } else {
        text = "Modo automático: el ordenador realizará jugadas sucesivamente."
      }
    }
    //Etiqueta que indica los puntos.
    val lPuntos = new Label {
      text = "Puntos: " + pts
    }
    //Etiqueta que indica los movimientos.
    val lMovimientos = new Label {
      text = "Movimientos: " + nmovs
    }
    //Obtenemos una lista con las tuplas de botones y posición de dicho botón para saber qué posición del tablero
    //para realizar los movimientos.
    val tableroSwing: List[Tuple2[Int, Button]] = crear_tuplas_de_botones(Nil, col * fil, tableroInicial, this, tamBoton)
    //Panel con las celdas del tablero.
    val myPanel = new Panel() {
      //Añadimos como contenido...
      private val myContents = (
        //... un grid con fil x col (al revés que siempre) para hacer la cuadrícula
        new Content += new GridPanel(fil, col) {
          //Para cada tuplas de botón,posición, se añade los botones al grid.
          //Como los botones serán compartidos por los hilos que los actualizan, no hará falta crear nuevas ventanas de manera recursiva.
          for (tupla <- tableroSwing) {
            contents += tupla._2
          }
        })
      //Una vez puestos todos los botones se sobreescribe el contenido del panel.
      override def contents = myContents

    }
    //Indica al usuario si desea guardar el juego
    val buttonGuardar = new Button {
      text = "GUARDAR"
    }
    //Contenidos del panel: las etiquetas, el botón y el panel de botones.
    contents = new BoxPanel(Orientation.Vertical) {
      contents += accion
      contents += lPuntos
      contents += lMovimientos
      contents += buttonGuardar
      contents += myPanel
      //Al panel de los botones se le añade además un scroll pane para poder ver todos los diamantes.
      contents += new BorderPanel {
        import scala.swing.BorderPanel.Position._
        add(new ScrollPane(myPanel), Center)
      }
      //Añade un borde.
      border = Swing.EmptyBorder(30, 30, 30, 30)
    }
    //Escuchamos al boton de guardar.
    listenTo(buttonGuardar)
    reactions += {
      //Si se pulsa el botón, se guarda el estado del juego
      case ButtonClicked(this.buttonGuardar) => {
        //Obtenemos los puntos del label, de nuevo, no podemos almacenarlos en ninguna variable.
        val pts = Integer.parseInt(lPuntos.text.drop(8))
        //De igual forma con los movimientos.
        val nmovs = Integer.parseInt(lMovimientos.text.drop(13))
        println("pts: " + pts + ", mos: " + nmovs)
        ScalaLegends.guardar(col, fil, dificultad, diamantes, get_tablero(Nil, col * fil, tableroSwing), pts, nmovs, modo)
        println("Juego guardado con éxito.")
        JOptionPane.showMessageDialog(null, "Juego guardado con éxito.", "Juego guardado", JOptionPane.INFORMATION_MESSAGE)
      }
    }
    //Para cada tupla de la lista de tuplas que contienen los botones se le añade un evento.
    for (elem <- tableroSwing) {
      //Escuchamos al botón en concreto.
      listenTo(elem._2)
      reactions += {
        //Si se pulsa el botón...
        case ButtonClicked(elem._2) => {
          //Se obtiene el tablero de la situación actual viendo TODOS los diamantes de todos los botones, puesto que no podemos sobre-
          //escribirlos en ningún lado.
          val tablero = get_tablero(Nil, col * fil, tableroSwing)
          //Obtenemos los puntos del label, de nuevo, no podemos almacenarlos en ninguna variable.
          val pts = Integer.parseInt(lPuntos.text.drop(8))
          //De igual forma con los movimientos.
          val nmovs = Integer.parseInt(lMovimientos.text.drop(13))
          //Si existen jugadas potenciales...
          if (backtracking(col, fil, tablero)) {
            //Creamos un hilo que se encarga de actualizar el panel y lo iniciamos.
            val hilo = new Actualizar(elem._2.asInstanceOf[Button], col, fil, tablero, pts, nmovs, dificultad, diamantes, modo, true, tableroSwing, lPuntos, lMovimientos)
            hilo.start()
          } else {
            //Indicamos al usuario que ya se ha acabado el juego.
            JOptionPane.showMessageDialog(null, "¡No le quedan más movimientos! :(\nEnhorabuena, su puntuación es de " + pts + " tras haber realizado " + nmovs + " movimientos.\n", "Fin del juego", JOptionPane.ERROR_MESSAGE)
            System.exit(0)
          }
        }
      }
    }
    //Si estamos en el modo automático crearemos un hilo para que ejecute el movimiento con un timelapse
    //específico, para que el usuario pueda ver la evolución del tablero.
    if (modo.equals(Main.textoCajaAutomatico)) {
      val thread = new JugadorAutomatico(col, fil, tableroInicial, pts, nmovs, dificultad, diamantes, modo, eliminarConjuntosEnCasoDeQueLosHubiese, tableroSwing, lPuntos, lMovimientos)
      thread.start()
    } else {
      //Sino, imprimimos la primera situación del tablero por pantalla, una especie de log.
      imprimir(col, fil, tableroInicial)
    }
    //Hacemos visible la ventana.
    visible = true
  }
  //Obtenemos el tablero en función de los botones. Cada botón tiene como texto el número de diamante que es, de ahí obtenemos cada valor.
  def get_tablero(lista: List[Int], tam: Int, tableroSwing: List[Tuple2[Int, Button]]): List[Int] = {
    //Si llegamos a 0, retornamos la lista.
    if (tam == 0) {
      lista
    } else {
      //Obtenemos el color del texto del botón, esto hace que estén algo descentrados a la izquierda las imagenes), nada importante.
      val color: String = tableroSwing(tam - 1)._2.text
      //Lo pasamos a int.
      val i: Int = color.toCharArray()(0).asDigit
      //Vamos generando el tablero.
      val nueva = i :: lista
      //de manera recursiva
      get_tablero(nueva, tam - 1, tableroSwing)
    }
  }

  //Devuelve tupla de numero (posicion en la lista), boton. El botón de momento no tiene evento.
  def crear_tuplas_de_botones(tuplas: List[Tuple2[Int, Button]], inicio: Int, tablero: List[Int], ventana: Frame, tam: Int): List[Tuple2[Int, Button]] = {
    //Si llegamos hasta el final, hasta 0, devolvemos la lista de tuplas que hemos ido construyendo.
    if (inicio == 0) {
      tuplas
    } else {
      //Creamos una tupla nueva que se incluye el número actual y el botón ocn la acción.
      val tuplaNueva: Tuple2[Int, Button] = (inicio, new Button {
        //Se lo indicamos al botón.
        preferredSize = new Dimension(tam, tam)
        //Si se ha seleccionado un diamante en el anterior turno, se colorea de blanco
        //para indicar al usuario cual había cogido antes, sino, estarán de negro.
        background = Color.BLACK
        //Se obtiene la imagen del diamante de la carpeta del programa, en función del color.
        val imagen = new ImageIcon("imagenes/" + ScalaLegends.get_posicion(inicio, tablero) + ".png")
        //Escalamos la imagen, puesto que el tamaño de la imagen y del botón puede ser diferente.
        icon = new ImageIcon(imagen.getImage().getScaledInstance(tam, tam, Image.SCALE_DEFAULT))
        //Ponemos como texto el valor del diamante, así podremos leerlo en todo momento en get_tablero()
        text = "" + get_posicion(inicio, tablero)
      })
      //Añadimos a una lista la tupla recién creada con el resto de tuplas que teníamos.
      val listaNueva = tuplaNueva :: tuplas
      //Llamamos recursivamente hasta crear todas las tuplas.
      crear_tuplas_de_botones(listaNueva, inicio - 1, tablero, ventana, tam)
    }
  }
}
//Hilo que actualiza el panel de botones según el estado del tablero.
class Actualizar(pulsado: Button, col: Int, fil: Int, tablero: List[Int], pts: Int, nmovs: Int, dificultad: Int, diamantes: List[Int],
                 modo: String, ec: Boolean, tableroSwing: List[Tuple2[Int, Button]], labelPuntos: Label, labelMovimientos: Label) extends Thread {
  //Comprueba cual es el primer botón que está pintado de blanco, lo que significa que lo ha seleccionado el usuario para moverlo
  //Si no tiene ninguno seleccionado, se devuelve 0.
  def comprueba_blanco(celda: Int, tuplas: List[Tuple2[Int, Button]]): Int = {
    if (tuplas.isEmpty) {
      0
    } else {
      //Cogemos el primer valor que veamos, no hará falta mirar más porque únicamente dejará que un botón esté en blanco.
      if (tuplas.head._2.background.equals(Color.WHITE)) {
        celda
      } else {
        //Sigue buscando el botón si no coincide
        comprueba_blanco(celda + 1, tuplas.tail)
      }
    }
  }
  //Obtiene los puntos de todo el tablero. Divide entre el numero de diamantes seguidos para on contar más de la cuenta.
  def get_puntos_recursivo(celda: Int, col: Int, fil: Int, tablero: List[Int]): Int = {
    if (celda > col * fil) {
      0
    } else {
      //Obtenemos los puntos de la celda.
      val ptsAhora = get_puntos(col, fil, celda, tablero)
      //Si existen puntos, hay que dividirlos para no contar varias veces los mismos.
      if (ptsAhora > 0) {
        //Obtenemos el conjunto de diamante sy divide a los puntos.
        val conjunto = ScalaLegends.comprobar_conjunto(col, fil, celda, tablero) + 1
        (ptsAhora / conjunto).intValue() + get_puntos_recursivo(celda + 1, col, fil, tablero)
      } else {
        get_puntos_recursivo(celda + 1, col, fil, tablero)
      }
    }
  }
  //Elimina conjuntos que se encuentren en la situación actual y recoge los puntos automáticamente. Método adaptado de ScalaLegends.scala
  def eliminarConjuntos(col: Int, fil: Int, tablero: List[Int], pts: Int): Tuple2[List[Int], Int] = {
    if (movimientos_situacion_actual(col, fil, tablero)) {
      val ptsNuevos = get_puntos_recursivo(1, col, fil, tablero)
      //Cambiamos las imagenes, puesto que ahora estan los valores eliminados como imagen y el tablero tiene nuevos valores.
      for (tupla <- tableroSwing) {
        //Recogemos el valor del diamante en función del número de botón y el tablero nuevo.
        val n = get_posicion(tupla._1, tablero)
        //Ponemos el fondo a negro para que pueda realizar otro movimiento:
        val imagen = new ImageIcon("imagenes/" + n + ".png")
        //Escalamos la imagen, puesto que el tamaño de la imagen y del botón puede ser diferente.
        val icono = new ImageIcon(imagen.getImage().getScaledInstance(tamBoton, tamBoton, Image.SCALE_DEFAULT))
        tupla._2.icon = icono
      }
      //Obtenemos el conjutno con ceros
      val conjuntoConCeros = limpiar_conjuntos_del_tablero(col, fil, tablero)
      //Con ese conjunto parcialmente vacío, miramos los 0s para pintarlo de rojo, mostrando al usuario qué diamantes se están eliminando.
      for (tupla <- tableroSwing) {
        //Recogemos el valor del diamante en función del número de botón y el tablero nuevo.
        val n = get_posicion(tupla._1, conjuntoConCeros)
        //Si el valor es cero, debemos pintarlo de rojo.
        if (n == 0) {
          //Se lo ponemos como texto.
          tupla._2.text = "" + n
          tupla._2.background_=(Color.RED)
        }
      }
      //Esperamos medio segundo para que el usuario lo pueda ver.
      Thread sleep 500
      //Ahora, a cada tupla lo pintamos de nuevo de negro.
      for (tupla <- tableroSwing) {
        tupla._2.background_=(Color.BLACK)
      }
      //Corremos los diamantes abajo y rellenamos aleatoriamente por arriba.
      val tableroAhoraSiNuevo = mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
      //Volvemos a comprobar si existiesen conjuntos con este nuevo tablero, retornando los puntos obtenidos en esta eliminación parcial
      eliminarConjuntos(col, fil, tableroAhoraSiNuevo, pts + ptsNuevos)
    } else {
      //Si no hay conjuntos se devuelve ese mismo tablero.
      (tablero, pts)
    }
  }

  //Obtiene un nuevo tablero en función de la celda seleccionada, el movimiento y el tablero inicial
  //Se devuelve una tupla en la que se incluyen los puntos obtenidos en la jugada.
  def nuevo_tablero(selec: Int, mov: Int, tableroInicial: List[Int], tableroSwing: List[Tuple2[Int, Button]]): Tuple2[List[Int], Int] = {
    //Comprobamos que el movimiento sea valido, es decir, no se salga del grid.
    if (movimiento_valido(col, fil, selec, mov, tableroInicial)) {
      //Obtenemos un tablero con el movimiento seleccionado.
      val tableroNuevo = ejecutar_movimiento(col, fil, selec, mov, tableroInicial)
      //Cogemos la otra casilla que sufre el movimiento, además de la seleccionada.
      val otra = obtener_la_otra_casilla(col, fil, selec, mov, tableroNuevo)
      //Si la jugada es posible, es decíar, hay más de 2 diamantes seguidos en  alguno de ellos
      if (jugada_posible(col, fil, selec, otra, tableroNuevo)) {
        //Cogemos los puntos de las dos casillas que sufren el movimiento.
        val ptsNuevos = get_puntos(col, fil, selec, tableroNuevo) + get_puntos(col, fil, otra, tableroNuevo)
        //Cogemos las dos celdas que se han cambiado por el usuario y les cambiamos las imagenes para que se vea el movimiento.
        for (tupla <- tableroSwing) {
          if (tupla._1 == selec || tupla._1 == otra) {
            //Recogemos el valor del diamante en función del número de botón y el tablero nuevo.
            val n = get_posicion(tupla._1, tableroNuevo)
            //Ponemos el fondo a negro para que pueda realizar otro movimiento:
            val imagen = new ImageIcon("imagenes/" + n + ".png")
            //Escalamos la imagen, puesto que el tamaño de la imagen y del botón puede ser diferente.
            val icono = new ImageIcon(imagen.getImage().getScaledInstance(tamBoton, tamBoton, Image.SCALE_DEFAULT))
            tupla._2.icon = icono
          }
        }
        //Obtenemos el tablero con ceros.
        val conjuntoConCeros = limpiar_conjuntos_del_tablero(col, fil, tableroNuevo)
        //A las celdas eliminadas se les pinta de rojo para que el usuario vea el progreso.
        for (tupla <- tableroSwing) {
          //Recogemos el valor del diamante en función del número de botón y el tablero nuevo.
          val n = get_posicion(tupla._1, conjuntoConCeros)
          //Si está vacío se muestra en rojo
          if (n == 0) {
            tupla._2.text = "" + n
            tupla._2.background_=(Color.RED)
          }
        }
        //Esperamos medio segundo para que el usuario pueda percibirlo
        Thread sleep 500
        //Pintamos de nuevo los botones de negro.
        for (tupla <- tableroSwing) {
          tupla._2.background_=(Color.BLACK)
        }
        //Limpiamoslos ceros rellenandolos con valores aleatorios y bajandolos.
        val tableroAhoraSiNuevo = mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
        //Si la jugada no es posible, se crea un nuevo frame con el primer turno y sin modificar nada.
        eliminarConjuntos(col, fil, tableroAhoraSiNuevo, ptsNuevos)
      } else {
        //Si no hay jugada, se devuelve el mismo tablero.
        (tableroInicial, 0)
      }
    } else {
      //Si no es valido el movimiento, se devuelve el mismo tablero.
      (tableroInicial, 0)
    }
  }
  //Obtiene el movimiento en formato (1:arriba, 2:derecha, 3:abajo, 4:izquierda) según el botón que se pulsó anteriormente y el nuevo.
  def get_movimiento_segun_boton(col: Int, fil: Int, anterior: Int, selec: Int): Int = {
    //Obtenemos las cuatro posibles celdas que se podría mover.
    val m1 = selec - col
    val m2 = selec + 1
    val m3 = selec + col
    val m4 = selec - 1
    //Comparamos cada una de las posibles combinaciones con la celda que hemos seleccionado supuestamente vecina.
    if (anterior == m1)
      1
    else if (anterior == m2)
      2
    else if (anterior == m3)
      3
    else if (anterior == m4)
      4
    //Si no coincide con ninguno el movimiento no será valido.
    else
      0
  }
  //Obtenemos el número que pertenece al botón, que es la posición de la celda en el tablero.
  def get_numero_de_boton(bot: Button, tuplas: List[Tuple2[Int, Button]]): Int = {
    //Si está vacío, devuelve 0
    if (tuplas.isEmpty)
      0
    //Si coincide el botón pulsado con el botón que estamos comprobanod de la tupla, se devuelve su identificador.
    else if (bot.equals(tuplas.head._2))
      tuplas.head._1
    else
      //Sino, seguimos mirando las demás tuplas.
      get_numero_de_boton(bot, tuplas.tail)
  }
  //Run del hilo, actualiza las imagenes de los botones y el tablero del juego.
  override def run {
    //Si estamos en modo manual:
    if (modo.equals(Main.textoCajaManual)) {
      //Cogemos la celta que está en blanco (anteriormente seleccionada por el usuario)
      val bl = comprueba_blanco(1, tableroSwing)
      //Si no ha pulsado antes ninguna, se pone esta en blanco, indicando al hilo la próxima vez que recuerde dicho valor.
      if (bl == 0) {
        pulsado.background_=(Color.WHITE)
      } else {
        //Si ya se ha seleccionado una anteriormente, cogemos el número de botón que el usuario acaba de pulsar.
        val selec = get_numero_de_boton(pulsado, tableroSwing)
        //Y el movimiento en formato (1,2,3,4) según la posición en la que esté.
        val mov = get_movimiento_segun_boton(col, fil, selec, bl)
        //Si el movimiento no es valido, es decir, no es un diamante vecino del blanco, se aborta el movimiento, poniendo todas las celdas
        //en negro.
        if (mov == 0) {
          for (tupla <- tableroSwing) {
            tupla._2.background_=(Color.BLACK)
          }
          //Si se ha pulsado una celda vecina:
        } else {
          tableroSwing(bl - 1)._2.background_=(Color.BLACK)
          //Obtenemos el nuevo tablero y los puntos que se consiguen al haber realizado ese movimiento.
          val nuevoYPuntos = nuevo_tablero(bl, mov, tablero, tableroSwing)
          //Recogemos por separado el tablero nuevo
          val nuevo = nuevoYPuntos._1
          //Y los puntos nuevos.
          val ptsNuevos = nuevoYPuntos._2
          //Si el tablero nuevo no coincide con el inicial, queire decir que ha habido una jugada por parte del usuario:
          if (!nuevo.equals(tablero)) {
            //Se imprime por pantalla en el log qué acción ha realizado:
            val x = ((bl - 1) % col) + 1
            val y = ((bl - 1) / col) + 1
            print("Respecto al anterior tablero,  muevo la columna " + x + ", fila " + y)
            mov match {
              case 1 => print(" arriba ")
              case 2 => print(" a la derecha ")
              case 3 => print(" abajo ")
              case 4 => print(" a la izquierda ")
            }
            println("obteniendo " + ptsNuevos + " puntos más, en total: " + (ptsNuevos + pts))
            //Y el tablero nuevo.
            imprimir(col, fil, nuevo)
            //Para cada botón, se actualiza su diamante y se pone en negro.
            for (tupla <- tableroSwing) {
              //Recogemos el valor del diamante en función del número de botón y el tablero nuevo.
              val n = get_posicion(tupla._1, nuevo)
              //Se lo ponemos como texto.
              tupla._2.text = "" + n
              //Ponemos el fondo a negro para que pueda realizar otro movimiento:
              tupla._2.background_=(Color.BLACK)
              //Cargamos la imagen y se la ponemos
              val imagen = new ImageIcon("imagenes/" + n + ".png")
              //Escalamos la imagen, puesto que el tamaño de la imagen y del botón puede ser diferente.
              val icono = new ImageIcon(imagen.getImage().getScaledInstance(tamBoton, tamBoton, Image.SCALE_DEFAULT))
              //Si el nuevo icono es diferente del que ya tenía se le actualiza.
              if (!icono.equals(tupla._2.icon)) {
                tupla._2.icon = icono
              }

            }
            //Se almacenan los puntos en el texto de la etiqueta de puntos
            labelPuntos.text = "Puntos: " + (pts + ptsNuevos)
            //Y lo mismo con los movimientos
            labelMovimientos.text = "Movimientos: " + (nmovs + 1)
          } else {
            for (tupla <- tableroSwing) {
              tupla._2.background_=(Color.BLACK)
            }
          }
        }
      }
    } else {
      val opcionOptima = true
      //Obtenemos la mejor posición en una lista.
      val accion = posicion_mejor_jugada(col, fil, tablero, opcionOptima)
      //La cabeza de la lista es la posición de la celda.
      val pos = get_posicion(1, accion)
      //El segundo elemento de la lista es el movimiento (1,2,3,4)
      val mov = get_posicion(2, accion)
      val otra = mov match {
        case 1 => pos - col
        case 2 => pos + 1
        case 3 => pos + col
        case 4 => pos - 1
      }
      tableroSwing(pos - 1)._2.background_=(Color.WHITE)
      tableroSwing(otra - 1)._2.background_=(Color.WHITE)
      Thread sleep 1000
      tableroSwing(pos - 1)._2.background_=(Color.BLACK)
      tableroSwing(otra - 1)._2.background_=(Color.BLACK)
      //Obtenemos las coordenadas de la posicion del tablero.
      val x = ((pos - 1) % col) + 1
      val y = ((pos - 1) / col) + 1
      print("Respecto al anterior tablero,  muevo la columna " + x + ", fila " + y)
      mov match {
        case 1 => print(" arriba ")
        case 2 => print(" a la derecha ")
        case 3 => print(" abajo ")
        case 4 => print(" a la izquierda ")
      }
      //Obtenemos el tablero nuevo y los puntos.
      val nuevoYPuntos = nuevo_tablero(pos, mov, tablero, tableroSwing)
      val nuevo = nuevoYPuntos._1
      val ptsNuevos = nuevoYPuntos._2
      println("obteniendo " + ptsNuevos + " puntos más, en total: " + (ptsNuevos + pts))
      //Para cada celda, actualizamos el tablero con las nuevas imagenes.
      for (tupla <- tableroSwing) {
        //Recogemos el valor del tablero.
        val n = get_posicion(tupla._1, nuevo)
        tupla._2.text = "" + n
        //Y lo ponemos en negro.
        tupla._2.background_=(Color.BLACK)
        val imagen = new ImageIcon("imagenes/" + n + ".png")
        //Escalamos la imagen, puesto que el tamaño de la imagen y del botón puede ser diferente.
        tupla._2.icon = new ImageIcon(imagen.getImage().getScaledInstance(tamBoton, tamBoton, Image.SCALE_DEFAULT))

      }
      //Guardamos los puntos y movimientos en los labels
      labelPuntos.text = "Puntos: " + (pts + ptsNuevos)
      labelMovimientos.text = "Movimientos: " + (nmovs + 1)
      //Como es automático, creamos un nuevo jugador que esperará, una vez sabido el nuevo tablero, no el inicial
      //y ejecuta un movimiento.
      val thread = new JugadorAutomatico(col, fil, nuevo, pts + ptsNuevos, nmovs + 1, dificultad, diamantes, modo, ec, tableroSwing, labelPuntos, labelMovimientos)
      thread.start()
    }
  }
}

//Hilo que se encarga de realizar el movimiento automático tras un tiempo parado para que el usuario aprecie los cambios.
class JugadorAutomatico(col: Int, fil: Int, tableroInicial: List[Int], pts: Int, nmovs: Int, dificultad: Int, diamantes: List[Int], modo: String, ec: Boolean, tableroSwing: List[Tuple2[Int, Button]], lp: Label, lm: Label) extends Thread {
  override def run {
    //Imprime la situación actual
    imprimir(col, fil, tableroInicial)
    if (backtracking(col, fil, tableroInicial)) {
      //Indicamos que el movimiento a realizar debe ser óptimo.
      val t = new Actualizar(null, col, fil, tableroInicial, pts, nmovs, dificultad, diamantes, modo, true, tableroSwing, lp, lm)
      t.start()
    } else {
      //Indicamos que ha finalizado el juego.
      JOptionPane.showMessageDialog(null, "¡No le quedan más movimientos! :(\nEnhorabuena, su puntuación es de " + pts + " tras haber realizado " + nmovs + " movimientos.\n", "Fin del juego", JOptionPane.ERROR_MESSAGE)
      System.exit(0)
    }
  }
}