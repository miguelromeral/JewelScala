package source

import java.io._
import java.nio.file.{ Paths, Files }
import java.lang._
import javax.swing.JOptionPane
import source._

object Main {
  //Valores estáticos que damos para que se pueda comparar sin problemas los modos.
  val textoCajaManual = "Manual"
  val textoCajaAutomatico = "Automático"
    //Tamaño del botón de cada celda en el tablero
    val tamBoton = 40
  //MAIN del programa.
  def main(args: Array[String]) {
    //Comprueba si existen datos guardados y se solicita su carga o no.
    if (Files.exists(Paths.get("datos.dat"))) {
      val options = Array[Object]("Sí, cargar juego", "No, nuevo juego")
      //Obtenemos la respuesta.
      val selec = JOptionPane.showOptionDialog(null,
        "Tiene un juego guardado anteriormente. ¿Desea reanudar dicho juego?", "Juego guardado", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options(1))
      //Se inicia un juego con los datos anteriores
      if (selec == 0) {
        //Obtenemos el estado del juego guardado.
        val juego = ScalaLegends.cargar()
        //Iniciamos un Frame con el estado de dicho juego.
        ScalaLegends.crearFrame(juego.getPts(), juego.getNmovs(), juego.getCol(),
          juego.getFil(), juego.getDif(), juego.getModo(), juego.getTab(), juego.getDiam(), true)
      } else {
        //Como no se quiere cargar, se inicia un nuevo juego con un nuevo estado.
        val n = new Iniciar()
      }
    } else {
      //Al no haber juego cargado, se inicia uno con una nueva configuración.
      val n = new Iniciar()
    }
  }

}