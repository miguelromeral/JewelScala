package source

import java.io._
import java.io.{ FileNotFoundException, IOException }
import java.nio.file.{ Paths, Files }
import java.lang._
import scala.swing.Frame

object ScalaLegends {
  //MAIN del programa.
  def pseudo_main(args: Array[String]) {
    //Si existen datos actualmente se pide si se quiere reanudar el juego.
    if (Files.exists(Paths.get("datos.dat"))) {
      println("Actualmente tiene una partida guardada anteriormente. ¿Desea reanudar dicho juego? (1 : Sí - 2 : No)")
      val isr = new InputStreamReader(System.in)
      val br = new BufferedReader(isr)
      val carga = cogerInt(br, 1, 2)
      //Se inicia un juego con los datos anteriores
      if (carga == 1) {
        iniciar_juego(cargar())
      } else {
        //Sino, se inicia un juego nuevo.
        iniciar_juego(null)
      }
    } else {
      iniciar_juego(null)
    }
  }

  //Inserta en la lista xs el elemento x. Si lo estuviese, añade x + n, hasta 9,
  //momento en que pasa a intentar añadir 1, así hasta que tenga éxito.
  //La función sirve para añadir diamantes de manera aleatoria.
  def insertar_en_lista_diamantes(x: Int, xs: List[Int]): List[Int] =
    if (x == 9)
      insertar_en_lista_diamantes(1, xs)
    else {
      if (!xs.contains(x)) {
        if (xs.isEmpty || x <= xs.head) x :: xs
        else xs.head :: insertar_en_lista_diamantes(x, xs.tail)
      } else {
        insertar_en_lista_diamantes(x + 1, xs)
      }
    }

  //Se encarga de imprimir las celdas con tabulador o fin linea en cada caso.
  //Llamada por imprimir
  def imprimir_aux(col: Int, fil: Int, l: List[Int], pos: Int, contenido: String): String =
    if (l.isEmpty) {
      contenido + "\n"
    } else {
      if ((pos + 1) % col == 0) {
        val nueva = contenido + "| " + int_a_diamante(l.head) + " |\n"
        imprimir_aux(col, fil, l.tail, pos + 1, nueva)
      } else {
        val nueva = contenido + "| " + int_a_diamante(l.head) + " "
        imprimir_aux(col, fil, l.tail, pos + 1, nueva)
      }
    } //> imprimir_aux: (l: List[Int])String

  //Imprime el teclado por pantalla en función de las columnas y filas.
  def imprimir(col: Int, fil: Int, l: List[Int]) = {
    print(imprimir_aux(col, fil, l, 0, "")) //> imprimir: (l: List[Int])Unit
  }

  //Cambia el valor del diamante por su letra correspondiente.
  def int_a_diamante(diamante: Int): Char = {
    diamante match {
      case 1 => 'A'
      case 2 => 'R'
      case 3 => 'N'
      case 4 => 'V'
      case 5 => 'P'
      case 6 => 'M'
      case 7 => 'G'
      case 8 => 'B'
    }
  }

  //Pone el elemento col en la posición pos, sustituyendo al que había en su lugar.
  //Suele utilizarse para poner diamantes al azar cuando están eliminados, por ejemplo.
  def poner(col: Int, pos: Int, l: List[Int]): List[Int] = {
    if (l.isEmpty) Nil
    else if (pos == 1) col :: l.tail
    else l.head :: poner(col, (pos - 1), l.tail)
  } //> poner: (col: Int, pos: Int, l: List[Int])List[Int] 	

  //Genera un tablero poniendo diamantes aleatorias de la lista de diamantes. El tablero
  //es de dimensiones [col]x[fil]. Pueden generarse situaciónes de 3 o más elementos juntos.
  def generar(col: Int, fil: Int, diamantes: List[Int]): List[Int] =
    generar_aux(col * fil, diamantes)

  //Genera un tablero completamente recursivamente con diamantes aleatorios de la lista de diamantes.
  //Llamada por generar
  def generar_aux(rest: Int, diamantes: List[Int]): List[Int] =
    if (rest == 0) {
      Nil
    } else {
      random_de_lista(diamantes) :: generar_aux(rest - 1, diamantes)
    }

  //Obtiene el elemento de la posición pos en la lista. Si el elemento que se pretende
  //buscar es mayor que el tamaño de la lista devuelve -1
  def get_posicion(pos: Int, lista: List[Int]): Int =
    if (lista.isEmpty) {
      -1
    } else {
      if (pos == 1) {
        lista.head
      } else {
        get_posicion(pos - 1, lista.tail)
      }
    }

  //Permuta el elemento que está en x el que está en y de la lista.
  def cambiar(x: Int, y: Int, lista: List[Int]): List[Int] =
    cambiar_aux(1, x, y, get_posicion(x, lista), get_posicion(y, lista), lista)

  //Cambia el elmento que está en x y el que está en y, se le pasan los valores de la lista en esas posiciones.
  //Llamada por cambiar.
  def cambiar_aux(pos: Int, x: Int, y: Int, px: Int, py: Int, lista: List[Int]): List[Int] = {
    if (lista.isEmpty)
      Nil
    else if (pos == x) py :: cambiar_aux(pos + 1, x, y, px, py, lista.tail)
    else if (pos == y) px :: cambiar_aux(pos + 1, x, y, px, py, lista.tail)
    else lista.head :: cambiar_aux(pos + 1, x, y, px, py, lista.tail)
  }

  //Elimina un diamante (lo pone a 0) en la posición pos de la lista.
  def eliminar_diamante(pos: Int, lista: List[Int]): List[Int] =
    pos match {
      case 1 => 0 :: lista.tail
      case n => lista.head :: eliminar_diamante(pos - 1, lista.tail)
    }

  //Determina la posición de la celda que se quiere permutar y se verifica que el movimiento es valido.
  //Esto es, si está en la primera fila, no se podrá hacer un movimiento hacia arriba.
  //Movimientos: 1 arriba, 2 derecha, 3 abajo, 4 izquierda
  def movimiento_valido(col: Int, fil: Int, pos: Int, mov: Int, lista: List[Int]): Boolean =
    if ((pos <= (col * fil)) && (pos > 0))
      mov match {
        case 1 => if (pos > col) true else false
        case 2 => if ((pos % col) != 0) true else false
        case 3 => if (pos <= (col * fil) - col) true else false
        case 4 => if ((pos % col) != 1) true else false
        case n => false
      }
    else
      false

  //Realiza la permutación comprobando si el movimiento es válido y realiza el cambio si lo es.
  //NO DETECTA si el movimiento genera una jugada de 3 o más diamantes juntos.
  def ejecutar_movimiento(col: Int, fil: Int, pos: Int, mov: Int, lista: List[Int]): List[Int] =
    if (movimiento_valido(col, fil, pos, mov, lista))
      mov match {
        case 1 => cambiar(pos - col, pos, lista)
        case 2 => cambiar(pos, pos + 1, lista)
        case 3 => cambiar(pos, pos + col, lista)
        case 4 => cambiar(pos - 1, pos, lista)
      }
    else
      lista

  //Obtiene el máximo de una lista. Si la lista está vacía devuelve -1
  //Función auxiliar
  def maximo_lista(lista: List[Int]): Int =
    if (lista.isEmpty) {
      -1
    } else {
      val n = maximo_lista(lista.tail)
      if (lista.head > n)
        lista.head
      else
        n
    }

  //Comprueba que una celda tiene un conjunto de 3 o más (empezando a contar por ella).
  //Suma los elementos que tiene en la derecha seguidos y los de la izquierda por si la celda que se está comprobando
  //está entre medias de un conjunto.
  def comprobar_conjunto(col: Int, fil: Int, pos: Int, lista: List[Int]): Int = {
    val color: Int = get_posicion(pos, lista)
    //Empieza a contar seguidos como -1 para que al contar con él mismo, no se añada uno más (no cuenta)
    //Un conjunto es válido cuando tiene 2 seguidos sea por donde sea.
    //El motivo por el que empieza a contar desde el mismo es porque diamantes_seguidos comprueba los laterales y para si es necesario.
    val listaaa = (diamantes_seguidos(col, fil, pos, lista, 1, -1, color) +
      diamantes_seguidos(col, fil, pos, lista, 3, -1, color)) :: (
        diamantes_seguidos(col, fil, pos, lista, 2, -1, color) +
        diamantes_seguidos(col, fil, pos, lista, 4, -1, color)) :: Nil
    maximo_lista(listaaa)
  }

  //Determina cuántos diamantes seguidos tiene una celda en las cuatro direcciones.
  //Si una celda ya no tiene más diamantes (porque se sale del tablero o porque no coincide su color) para de contar.
  def diamantes_seguidos(col: Int, fil: Int, pos: Int, lista: List[Int], mov: Int, seguidos: Int, color: Int): Int =
    //Si coincide el color inicial con esta posición...
    if (get_posicion(pos, lista) == color) {
      //... comprobamos que haya más colores en la dirección que se nos especifica.
      mov match {
        //Si podemos seguir mirando arriba, sumamos uno y llamamos recursivamente, sino, paramos antes de salirnos del tablero, habiendo añadido uno más.
        case 1 => if (pos - col > 0) diamantes_seguidos(col, fil, pos - col, lista, 1, seguidos + 1, color) else seguidos + 1
        case 2 => if ((pos + 1) % col != 1) diamantes_seguidos(col, fil, pos + 1, lista, 2, seguidos + 1, color) else seguidos + 1
        case 3 => if (pos + col <= col * fil) diamantes_seguidos(col, fil, pos + col, lista, 3, seguidos + 1, color) else seguidos + 1
        case 4 => if ((pos - 1) % col != 0) diamantes_seguidos(col, fil, pos - 1, lista, 4, seguidos + 1, color) else seguidos + 1
      }
    } else {
      //Sino, devolvemos todos los seguidos que hemos contado hasta aquí.
      seguidos
    }

  //Indica si hay un conjunto posible en las celdas x e y. Se comprueban dos celdas porque se permutan dos diamantes.
  def jugada_posible(col: Int, fil: Int, x: Int, y: Int, lista: List[Int]): Boolean =
    //Como ya se dijo anteriormente, cuando hay 3 diamantes seguidos estas dos funciones devuelve 2, por eso debe ser > 1
    comprobar_conjunto(col, fil, x, lista) > 1 || comprobar_conjunto(col, fil, y, lista) > 1

  //Recorre todo el tablero realizando todos los movimientos con backtracking (no termina de ser backtracking puro)
  //y devuelve si es posible realizar una jugada moviendo cualquiera de los diamantes en cualquier dirección.
  //Pila de llamadas: backtracking --> recorrer_backtracking_cada_celda --> backtracking_una_celda --> backtracking_una_celda_aux
  def backtracking(col: Int, fil: Int, lista: List[Int]): Boolean = {
    if (maximo_lista(recorrer_backtracking_cada_celda(col, fil, 1, lista)) > 1) true else false
  }

  //Para cada celda, obtiene el valor de backtracking (jugada posible más alta).
  //Si se la llama empezando por 1, se obtiene una lista de el máximo de los valores de cada celda en orden. Esto sirve para
  //que el modo automático obtenga qué posición del tablero mover mejor.
  def recorrer_backtracking_cada_celda(col: Int, fil: Int, pos: Int, lista: List[Int]): List[Int] = {
    if (pos > col * fil)
      Nil
    else
      backtracking_una_celda(col, fil, pos, lista) :: recorrer_backtracking_cada_celda(col, fil, pos + 1, lista)
  }

  //Obtiene el máximo valor de conjuntos de todos los movimientos posibles en esa celda.
  //Mueve en las cuatro direcciones y obtiene el más alto de los cuatro.
  def backtracking_una_celda(col: Int, fil: Int, celda: Int, lista: List[Int]): Int = {
    val todos = backtracking_una_celda_aux(col, fil, celda, 1, lista)
    val maximo = maximo_lista(todos)
    maximo
  }

  //Ejecuta el movimiento en el tablero (dirección es mov) y obtiene el valor de los puntos obtenidos de ese movimiento.
  //Si se le llama empezando por 1 se obtiene una lista de todos los valores si se permuta la posición de la celda que se le pasa
  //por parámetro en las cuatro direcciones. El modo automático recoge esa lista para obtener la dirección de la celda a cambiar.
  def backtracking_una_celda_aux(col: Int, fil: Int, celda: Int, mov: Int, lista: List[Int]): List[Int] = {
    mov match {
      //Ponemos fin a la lista.
      case 5 => Nil
      case n => {
        //Obtenemos la nueva situación del tablero.
        val nuevaLista = ejecutar_movimiento(col, fil, celda, mov, lista)
        //Si el movimiento es valido tendremos en cuenta los puntos de la celda que sufre el movimiento
        //además de la celda que seleccionamos.
        if (movimiento_valido(col, fil, celda, mov, lista)) {
          //En función del movimiento, se obtiene una celda diferente y se recogen esos puntos.
          val ptsOtra = mov match {
            case 1 => get_puntos(col, fil, celda - col, nuevaLista)
            case 2 => get_puntos(col, fil, celda + 1, nuevaLista)
            case 3 => get_puntos(col, fil, celda + col, nuevaLista)
            case 4 => get_puntos(col, fil, celda - 1, nuevaLista)
          }
          //Los puntos totales serán los puntos de la celda seleccionada mas los puntos de la celda que sufre el movimiento
          //anteriormente calculado.
          val ptsTotales = get_puntos(col, fil, celda, nuevaLista) + ptsOtra
          //Se genera la lista con los puntos totales de este movimiento y se recorre recursivamente.
          ptsTotales :: backtracking_una_celda_aux(col, fil, celda, mov + 1, lista)
        } else {
          //El valor que tiene como referencia es la cantidad de puntos que ocasiona dicho movimiento.
          val ptsUnicaCelda = get_puntos(col, fil, celda, nuevaLista)
          ptsUnicaCelda :: backtracking_una_celda_aux(col, fil, celda, mov + 1, lista)
        }
      }
    }
  }

  //Suma pares o impares, sirve para sumar los dos movimientos (1 y 3: arriba y abajo; 2 y 4: derecha e izquierda) para
  //Obtener el valor del conjunto en las columnas y en las filas.
  //0 pares, 1 impares 
  def sumaParesOImpares(par: Int, lista: List[Int]): Int =
    sumaParesOImpares_aux(par, 1, lista)

  //Recorre la lista comprobando si es par o no y si debe sumar el elemento lo hace.
  //Llamada por sumaParesOImpares
  def sumaParesOImpares_aux(par: Int, pos: Int, lista: List[Int]): Int = {
    if (lista.isEmpty) {
      0
    } else {
      if (par == pos % 2) {
        lista.head + sumaParesOImpares_aux(par, pos + 1, lista.tail)
      } else {
        sumaParesOImpares_aux(par, pos + 1, lista.tail)
      }
    }
  }

  //Comprueba si existen movimientos en la situación actual. Comprobará si en alguna de las celdas existe un conjunto de 3 o más diamantes
  //sin necesidad de realizar un movimiento.
  def movimientos_situacion_actual(col: Int, fil: Int, tablero: List[Int]): Boolean = {
    if (maximo_lista(movimientos_situacion_actual_aux(col, fil, 1, tablero)) > 1) true else false
  }

  //Comprueba cuantos diamantes seguidos hay en una determinada celda.
  def movimientos_situacion_actual_aux(col: Int, fil: Int, pos: Int, tablero: List[Int]): List[Int] = {
    if (pos > col * fil)
      Nil
    else
      comprobar_conjunto(col, fil, pos, tablero) :: movimientos_situacion_actual_aux(col, fil, pos + 1, tablero)
  }

  //Obtiene una lista de diamantes cuyo tamaño varía en función de la dificultad. Los valores son aleatorios.
  def coger_diamantes(dif: Int): List[Int] = {
    dif match {
      case 1 => coger_diamantes_aux(4, Nil)
      case 2 => coger_diamantes_aux(5, Nil)
      case 3 => coger_diamantes_aux(7, Nil)
    }
  }

  //Obtiene una lista de numeros aleatorios entre 1 y 8 incluídos.
  def coger_diamantes_aux(vuelta: Int, lista: List[Int]): List[Int] = {
    if (vuelta == 0)
      lista
    else
      coger_diamantes_aux(vuelta - 1, insertar_en_lista_diamantes(scala.util.Random.nextInt(7) + 1, lista))
  }

  //Devuelve una lista con las dimensiones del teclado (col)::(fil)::Nil
  //Al iniciar el juego recoge estos datos y obtiene un tablero.
  def poner_dimensiones_teclado(dif: Int): List[Int] = {
    dif match {
      case 1 => 7 :: 9 :: Nil
      case 2 => 11 :: 17 :: Nil
      case 3 => 15 :: 27 :: Nil
    }
  }

  //Inicia la ejecución del juego en función del juego que se le pase (si es que se ha cargado uno) o nulo, si se realizará uno nuevo.
  def iniciar_juego(juego: Juego) = {
    println("Scala Jewel's Legends -> Interfaz:\nPrimero se introducen las columnas, luego las filas y por último el movimiento.")
    println("Los movimientos válidos son: 1:arriba, 2:derecha, 3:abajo, 4:izquierda.")
    println("Si se introduce un -1 se finaliza el juego manualmente.\n")
    val isr = new InputStreamReader(System.in)
    val br = new BufferedReader(isr)
    try {
      //Elimina los conjuntos, antes de que el usuario realice un movimiento, de manera automática.
      val eliminarConjuntosEnCasoDeQueLosHubiese = true
      //Si no se ha cargado un juego nuevo se piden los datos para iniciar uno nuevo.
      if (juego == null) {
        print("Escriba la dificultad (1 a 3): ")
        val dif = cogerInt(br, 1, 3)
        if (dif == -1)
          throw new Exception("Ha terminado manualmente el juego.")

        print("Modo de juego (1:Manual, 2:Automático) - ")
        val modo = cogerInt(br, 1, 2)
        val dimensionesTablero = poner_dimensiones_teclado(dif)
        val col = dimensionesTablero.head
        val fil = get_posicion(2, dimensionesTablero)
        val listaDiamantes = coger_diamantes(dif)
        val tablero = generar(col, fil, listaDiamantes)
        println("")
        if (modo == 1) {
          //Modo manual.
          val pts = bucle_principal(0, 1, col, fil, dif, listaDiamantes, tablero, br, eliminarConjuntosEnCasoDeQueLosHubiese)
          println("Enhorabuena, tu puntuacion ha sido de: " + pts)
        } else {
          //Indicamos la opción óptima activada a la hora de buscar movimientos posibles.
          val opcionOptima = true
          //Modo automático.
          println("La puntuación del ordenador ha sido de: " + bucle_ia(0, col, fil, dif, listaDiamantes, tablero, eliminarConjuntosEnCasoDeQueLosHubiese, 0, opcionOptima))
        }
        //Sino, se inicia un juego nuevo con la configuración guardada.
      } else {
        //iniciar el juego anterior.
        if (juego.getCol() != 0)
          //Si hay un juego guardado únicamente será en modo manual.
          println("Enhorabuena, tu puntuacion ha sido de: " + bucle_principal(juego.getPts(), juego.getNmovs(), juego.getCol(), juego.getFil(), juego.getDif(), juego.getDiam(), juego.getTab(), br, eliminarConjuntosEnCasoDeQueLosHubiese))
        else {

          //Si no se ha podido cargar el juego se habrá devuelto un juego con valores 0, entonces sabemos que no es posible
          println("*******************************************************")
          println("Por desgracia, el archivo guardado no es compatible con la versión actual del juego,\npor lo que no puede " +
            "cargar el juego guardado anteriormente.\nInicie un juego nuevo y podrá guardar ese progreso para otro momento en esta versión.")
          println("*******************************************************")
        }

      }
    } catch {
      case nfe: NumberFormatException => { println("Deben introducirse números naturales") }
      case ex: Exception              => { println(ex.getMessage) }
    } finally
      println("Final del programa")
  }

  //Devuelve la mejor posicion y el mejor movimiento en una lista.
  //MejorPosicion :: MejorMovimiento :: Nil
  //Realiza backtracking de todo el tablero y obtiene la combinación mejor y su movimiento.
  def posicion_mejor_jugada(col: Int, fil: Int, tablero: List[Int], opcionOptima: Boolean): List[Int] = {
    if (opcionOptima) {
      //Lista con la mejor jugada de cada celda al haber realizado backtracking
      val mejores_jugadas_cada_celda = recorrer_backtracking_cada_celda(col, fil, 1, tablero)
      //Valor del mejor conjunto de toda la lista anteriormente. Solo el valor.
      val mejorValor = maximo_lista(mejores_jugadas_cada_celda)
      //Obtenemos una lista de posiciones en las que se da la mejor jugada.
      val listaPosMejor = posicion_de_x_en_lista(mejorValor, 1, mejores_jugadas_cada_celda, Nil)
      //Obtenemos la posición que moveremos al azar, para que no esté siempre por encima.
      val posMejor = random_de_lista(listaPosMejor)
      //Combinación de jugadas en las cuatro direcciones sobre la mejor posición encontrada.
      val jugadasEnMejorPosicion = backtracking_una_celda_aux(col, fil, posMejor, 1, tablero)
      //Encuentra la dirección del mejor movimiento (por tanto, mejor movimiento) de la celda seleccionada anteriomente.
      val mejorMovimiento = posicion_de_x_en_lista(mejorValor, jugadasEnMejorPosicion)
      //Devolvemos la lista completa.
      posMejor :: mejorMovimiento :: Nil
    } else {
      //Obtenemos la posición que moveremos, será el primer movimiento que encontremos posible empezando por arriba
      val posicion = mejor_jugada_ineficiente(col, fil, 1, tablero)
      //Combinación de jugadas en las cuatro direcciones sobre la posición encontrada.
      val jugadasEnMejorPosicion = backtracking_una_celda_aux(col, fil, posicion, 1, tablero)
      //Valor más alto en esa posición (respecto a las cuatro direcciones)
      val mejorValor = backtracking_una_celda(col, fil, posicion, tablero)
      //Encuentra la dirección del mejor movimiento (por tanto, mejor movimiento) de la celda seleccionada anteriomente.
      val mejorMovimiento = posicion_de_x_en_lista(mejorValor, jugadasEnMejorPosicion)
      //Devolvemos la lista completa.
      posicion :: mejorMovimiento :: Nil
    }
  }

  def mejor_jugada_ineficiente(col: Int, fil: Int, pos: Int, tablero: List[Int]): Int = {
    if (pos > col * fil) {
      -1
    } else {
      if (backtracking_una_celda(col, fil, pos, tablero) > 1)
        pos
      else
        mejor_jugada_ineficiente(col, fil, pos + 1, tablero)
    }
  }

  def posicion_de_x_en_lista(x: Int, vuelta: Int, lista: List[Int], retorno: List[Int]): List[Int] = {
    if (lista.isEmpty)
      retorno
    else if (x == lista.head)
      posicion_de_x_en_lista(x, vuelta + 1, lista.tail, vuelta :: retorno)
    else
      posicion_de_x_en_lista(x, vuelta + 1, lista.tail, retorno)
  }

  //Obtiene la posición de x en la lista. Si no está, devuelve -1.
  def posicion_de_x_en_lista(x: Int, lista: List[Int]): Int = {
    val pos = posicion_de_x_en_lista_aux(x, 0, lista)
    if (pos > lista.length)
      -1
    else
      pos
  }

  //Suma +1 si la cabeza de la lista no es la posición que buscamos. Si la encuentra para, sino suma uno más que su longitud.
  def posicion_de_x_en_lista_aux(x: Int, pos: Int, lista: List[Int]): Int = {
    if (lista.isEmpty) {
      1
    } else {
      if (x == lista.head)
        1
      else
        1 + posicion_de_x_en_lista_aux(x, pos + 1, lista.tail)
    }
  }

  //Bucle del modo automático
  def bucle_ia(movs: Int, col: Int, fil: Int, dif: Int, diamantes: List[Int], tablero: List[Int], eliminarEnCasoDeConjuntos: Boolean, pts: Int, opcionOptima: Boolean): Int = {
    //Si se ha seleccionado la opción de eliminar conjuntos automáticamente y
    //existen conjuntos en la situación actual:
    if (eliminarEnCasoDeConjuntos && movimientos_situacion_actual(col, fil, tablero)) {
      //Limpiamos el conjunto, los eliminamos.
      val conjuntoConCeros = limpiar_conjuntos_del_tablero(col, fil, tablero)
      //      println("Elimino porque hay movimientos posibles")
      //      imprimir(col, fil, conjuntoConCeros)
      //Baja los diamantes y añade diamantes aleatorios en la parte superior.
      val tableroAhoraSiNuevo = mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
      //De nuevo realizamos el bucle automático.
      //En esta ocasión no se suma ningún punto
      bucle_ia(movs, col, fil, dif, diamantes, tableroAhoraSiNuevo, eliminarEnCasoDeConjuntos, pts, opcionOptima)
    } else {
      //Si no hay movimientos en la situación actual se imprime para ver el recorrido:
      println("** Pts.: " + pts + " - Movs.: " + movs ++ " **")
      imprimir(col, fil, tablero)
      //Si se pueden realizar movimientos mediante cualquier jugada, se busca la mejor.
      if (backtracking(col, fil, tablero)) {
        //Obtenemos la mejor posición en una lista.
        val accion = posicion_mejor_jugada(col, fil, tablero, opcionOptima)
        //La cabeza de la lista es la posición de la celda.
        val pos = get_posicion(1, accion)
        //El segundo elemento de la lista es el movimiento (1,2,3,4)
        val mov = get_posicion(2, accion)
        //Obtenemos las coordenadas de la posicion del tablero.
        val x = ((pos - 1) % col) + 1
        val y = ((pos - 1) / col) + 1
        print("Respecto al anterior tablero,  muevo la columna " + x + ", fila " + y)
        mov match {
          case 1 => println(" arriba.")
          case 2 => println(" a la derecha.")
          case 3 => println(" abajo.")
          case 4 => println(" a la izquierda.")
        }
        //Ejecutamos el movimiento.
        val tableroNuevo = ejecutar_movimiento(col, fil, pos, mov, tablero)
        //Obtenemos la casilla que es afectada. Por ejemplo, si muevo la celda 1 a la derecha (a 2), la celda 2 es la afectada (otra) y 1 es la principal (pos).
        val otra = obtener_la_otra_casilla(col, fil, pos, mov, tableroNuevo)
        //Recogemos cuántos hay en las dos posiciones tras haber ejecutado el movimiento.
        val ptsNuevos = get_puntos(col, fil, pos, tableroNuevo) + get_puntos(col, fil, otra, tableroNuevo)
        //Limpiamos los conjuntos
        val conjuntoConCeros = limpiar_conjuntos_del_tablero(col, fil, tableroNuevo)
        //Corremos las celdas hacia abajo y rellenamos por arriba.
        val tableroAhoraSiNuevo = mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
        //Devolvemos la suma de los puntos totales.
        bucle_ia(movs + 1, col, fil, dif, diamantes, tableroAhoraSiNuevo, eliminarEnCasoDeConjuntos, pts + ptsNuevos, opcionOptima)
      } else {
        //Si no hay más combinaciones posibles se suma cero a todos los puntos anteriores.
        pts
      }
    }
  }

  //Bucle principal del juego en la que se realizan movimientos en modo manual.
  def bucle_principal(pts: Int, nmovs: Int, col: Int, fil: Int, dif: Int, diamantes: List[Int], tablero: List[Int], br: BufferedReader, eliminarConjuntosEnCasoDeQueLosHubiese: Boolean): Int = {
    //Si está seleccionada la opción de eliminar conjuntos de manera automática
    //y existen conjuntos en la situación actual.
    if (eliminarConjuntosEnCasoDeQueLosHubiese && movimientos_situacion_actual(col, fil, tablero)) {
      //Limpiamos los conjuntos.
      val conjuntoConCeros = limpiar_conjuntos_del_tablero(col, fil, tablero)
      //Corremos los diamantes abajo y rellenamos aleatoriamente por arriba.
      val tableroAhoraSiNuevo = mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
      //Llamamos al bucle principal sin que el usuario pueda realizar ningún movimiento. Los puntos de los eliminados aquí no se cuentan.
      bucle_principal(pts, nmovs, col, fil, dif, diamantes, tableroAhoraSiNuevo, br, eliminarConjuntosEnCasoDeQueLosHubiese)
    } else {
      //Si no hay movimientos en la situación actual se muestra por pantalla al usuario el tablero y se le pide la acción.
      println("** Pts.: " + pts + " - Movs.: " + nmovs ++ " **")
      imprimir(col, fil, tablero)
      //Recogemos la acción que ha seleccionado.
      val accion = get_movimiento(br, col, fil)
      //Si está vacía es porque ha deseado finalizarlo, devolvemos entonces los puntos totales.
      if (accion.isEmpty) {
        println("¿Desea guardar el juego para reanudarlo en otra ocasión? (1 : Sí - 2 : No)")
        val guarda = cogerInt(br, 1, 2)
        /*if (guarda == 1) {
          guardar(col, fil, dif, diamantes, tablero, pts, nmovs)
        }*/
        pts
      } else {
        //Si sí ha realizado un movimiento comprobamos si hay combinaciones mediante backtracking.
        if (backtracking(col, fil, tablero)) {
          //Columnas y filas seleccionadas respectivamente.
          val x = get_posicion(1, accion)
          val y = get_posicion(2, accion)
          //Obtenemos la posición en la lista que compone el tablero.
          val pos = x + (y - 1) * col
          //Dirección del movimiento.
          val mov = get_posicion(3, accion)
          //Si el movimiento es válido, se puede permutar con esa celda... (no se sale del tablero)
          if (movimiento_valido(col, fil, pos, mov, tablero)) {
            //Obtenemos un tablero nuevo con el movimiento ejecutado.
            val tableroNuevo = ejecutar_movimiento(col, fil, pos, mov, tablero)
            //Cogemos la celda que sufre el movimiento.
            val otra = obtener_la_otra_casilla(col, fil, pos, mov, tableroNuevo)
            //Si la jugada es posible, es decir, se ha generado un conjunto de 3 o más diamantes...
            if (jugada_posible(col, fil, pos, otra, tableroNuevo)) {
              //Obtenemos los puntos de las dos posiciones que ha seleccionado.
              val ptsNuevos = get_puntos(col, fil, pos, tableroNuevo) + get_puntos(col, fil, otra, tableroNuevo)
              //Limpiamos el tablero con los conjuntos.
              val conjuntoConCeros = limpiar_conjuntos_del_tablero(col, fil, tableroNuevo)
              //Movemos hacia abajo las celdas y rellenamos por arriba con diamantes aleatorios.
              val tableroAhoraSiNuevo = mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
              //Llamamos de nuevo al bucle principal con los puntos añadidos.
              bucle_principal(ptsNuevos + pts, nmovs + 1, col, fil, dif, diamantes, tableroAhoraSiNuevo, br, eliminarConjuntosEnCasoDeQueLosHubiese)
            } else {
              //Si se puede ejecutar el movimiento pero no hay combinaciones, no se deshace el movimiento, simplemente
              //Pasamos a la pila de recursividad el tablero inicial.
              println("No existen conjuntos de diamantes mediante esa jugada.")
              bucle_principal(pts, nmovs, col, fil, dif, diamantes, tablero, br, eliminarConjuntosEnCasoDeQueLosHubiese)
            }
          } else {
            //No se puede permutar un diamante con una casilla que esté fuera del tablero.
            println("No puede permutar un diamante con una casilla que esté fuera del tablero.")
            bucle_principal(pts, nmovs, col, fil, dif, diamantes, tablero, br, eliminarConjuntosEnCasoDeQueLosHubiese)
          }
        } else {
          //Al final no existen movimientos de ninguna manera. Fin de la partida.
          println("¡No existen movimientos posibles! :(")
          pts
        }
      }
    }
  }

  //Mueve las celdas eliminadas hacia arriba y las que no están eliminadas las baja. Arriba, si ya no se pueden bajar, se rellena con un diamante aleatorio.
  def mover_hacia_abajo(col: Int, fil: Int, tablero: List[Int], diamantes: List[Int]): List[Int] = {
    mover_hacia_abajo_cada_celda(col, fil, col * fil, tablero, diamantes)
  }

  //Mueve cada celda hacia abajo empezando por la más baja (de más a menos). Si está eliminada comprueba hacia arriba para cambiarla con la más
  //cercana, si no existe ninguna, pone un valor aleatorio en esa celda. Devuelve el tablero completo cuando ha llegado a la primera fila.
  def mover_hacia_abajo_cada_celda(col: Int, fil: Int, pos: Int, tablero: List[Int], diamantes: List[Int]): List[Int] = {
    if (pos < 1)
      tablero
    else {
      //Si la posición es cero miramos hacia arriba para cambiarla con una celda que no sea vacía.
      if (get_posicion(pos, tablero) == 0) {
        //Obtenemos la posición con la que queremos cambiarla.
        val posACambiar = mirar_hacia_arriba(col, fil, pos - col, tablero, diamantes)
        //Si la posición a cambiar es -1 indica que no hay más celdas eliminadas arriba, por tanto, debe poner un valor aleatorio en esa celda.
        if (posACambiar == -1) {
          val lista = poner(random_de_lista(diamantes), pos, tablero)
          mover_hacia_abajo_cada_celda(col, fil, pos - 1, lista, diamantes)
        } else {
          //Cambiamos las dos celdas (ponemos la celda eliminada arriba)
          val lista = cambiar(pos, posACambiar, tablero)
          mover_hacia_abajo_cada_celda(col, fil, pos - 1, lista, diamantes)
        }
      } else {
        //Si no es cero, seguimos mirando para arriba.
        mover_hacia_abajo_cada_celda(col, fil, pos - 1, tablero, diamantes)
      }
    }
  }

  //Devuelve la posición en la que existe (por encima del tablero) un diamante no vacío para que se cambie
  //por el vacío y los vacíos se queden arriba.
  def mirar_hacia_arriba(col: Int, fil: Int, posFin: Int, tablero: List[Int], diamantes: List[Int]): Int = {
    if (posFin < 1) {
      -1
    } else {
      val elemento = get_posicion(posFin, tablero)
      if (elemento == 0) {
        mirar_hacia_arriba(col, fil, posFin - col, tablero, diamantes)
      } else {
        posFin
      }
    }
  }

  //Obtiene un elemento aleatorio de la lista.
  def random_de_lista(lista: List[Int]): Int = {
    get_posicion(scala.util.Random.nextInt(lista.length) + 1, lista)
  }

  //Pone a cero los diamantes que componen un conjunto (3 ó más diamantes)
  def limpiar_conjuntos_del_tablero(col: Int, fil: Int, tablero: List[Int]): List[Int] = {
    limpiar_conjuntos_aux(col, fil, 1, tablero)
  }

  //Comprueba una a una las celdas del tablero y elimina los diamantes que componen un conjunto.
  def limpiar_conjuntos_aux(col: Int, fil: Int, pos: Int, tablero: List[Int]): List[Int] = {
    if (pos > col * fil)
      Nil
    else {
      //Si existe combinación se pone a 0.
      if (comprobar_conjunto(col, fil, pos, tablero) > 1)
        0 :: limpiar_conjuntos_aux(col, fil, pos + 1, tablero)
      else
        //Sino, se mantiene la posición actual.
        get_posicion(pos, tablero) :: limpiar_conjuntos_aux(col, fil, pos + 1, tablero)

    }
  }

  //Obtiene los puntos de una determinada celda. Los puntos cuentan (x25) cada celda únicamente si componen un conjunto
  def get_puntos(col: Int, fil: Int, pos: Int, tablero: List[Int]): Int = {
    val color: Int = get_posicion(pos, tablero)
    //Comprobamos los dos ejes Porque puede que hagamos un movimiento en forma de T, en cuyo caso
    //sumamos los diamantes del eje vertical y el horizontal. Además, la celda que está en medio vale EL DOBLE.
    val conjuntoHor = diamantes_seguidos(col, fil, pos, tablero, 1, -1, color) +
      diamantes_seguidos(col, fil, pos, tablero, 3, -1, color)
    val conjuntoVer = diamantes_seguidos(col, fil, pos, tablero, 2, -1, color) +
      diamantes_seguidos(col, fil, pos, tablero, 4, -1, color)
    //Un conjunto es posible si tiene 2 (ya que no se cuenta a la celda misma, por lo que serían 3)
    //Luego se suma 1 y se suman los puntos oportnuos.
    if (conjuntoHor > 1) {
      if (conjuntoVer > 1) {
        //Los movimientos en forma de T valen EL DOBLE.
        (conjuntoHor + 1) * 25 + (conjuntoVer + 1) * 25
      } else {
        (conjuntoHor + 1) * 25
      }
    } else {
      if (conjuntoVer > 1) {
        (conjuntoVer + 1) * 25
      } else {
        0
      }
    }

  }

  //Obtiene la casilla que sufre el movimiento según el movimiento seleccionado.
  def obtener_la_otra_casilla(col: Int, fil: Int, pos: Int, mov: Int, lista: List[Int]): Int = {
    mov match {
      case 1 => pos - col
      case 2 => pos + 1
      case 3 => pos + col
      case 4 => pos - 1
    }
  }

  //Pide al usuario un entero entre x e y hasta que esté entre ese valor. También admite -1 para acabar.
  def cogerInt(br: BufferedReader, x: Int, y: Int): Int = {
    val dev = cogerIntEntreRango(br, x, y)
    //Si nos devuelve 0, indica que no es válido el número seleccionado. Se repite la llamada.
    if (dev == 0)
      cogerInt(br, x, y)
    else
      dev
  }

  //Obtiene un Int entre x e y o -1.
  //Llamada por cogerInt
  def cogerIntEntreRango(br: BufferedReader, x: Int, y: Int): Int = {
    try {
      //Obtiene el número.
      val num = Integer.parseInt(br.readLine())
      //Si el numero es -1 se admite, significa que acaba manualmente.
      if (num == -1) {
        -1
      } else {
        //Sino, si no está entre el rango se lanza una excepción y se devuelve 0.
        if (num < x || num > y) {
          throw new Exception("Deben introducirse números entre " + x + " y " + y + ": ")
        } else
          num
      }
    } catch {
      case nfe: NumberFormatException => { println("Deben introducirse números naturales"); 0 }
      case ex: Exception              => { print(ex.getMessage); 0 }
    }
  }

  //Devuelve una lista con columna::fila::movimiento
  def get_movimiento(br: BufferedReader, nc: Int, nf: Int): List[Int] = {
    try {
      print("Columna: ")
      val col = cogerInt(br, 1, nc)
      if (col == -1)
        throw new Exception("Ha terminado manualmente el juego.")

      print("Fila: ")
      val fil = cogerInt(br, 1, nf)
      if (fil == -1)
        throw new Exception("Ha terminado manualmente el juego.")

      print("Movimiento: ")
      val mov = cogerInt(br, 1, 4)
      if (mov == -1)
        throw new Exception("Ha terminado manualmente el juego.")

      col :: fil :: mov :: Nil
    } catch {
      case nfe: NumberFormatException => { println("Deben introducirse números naturales"); Nil }
      case ex: Exception              => { println(ex.getMessage); Nil }
    }
  }

  //Guarda el juego serializando los datos que se le pasan por parámetro.
  def guardar(col: Int, fil: Int, dif: Int, diamantes: List[Int], tablero: List[Int], pts: Int, nmovs: Int, modo: String) {
    try {
      val fos = new FileOutputStream("datos.dat")
      val oos = new ObjectOutputStream(fos)
      oos.writeObject(new Juego(col, fil, dif, diamantes, pts, tablero, nmovs, modo))
      fos.close()
    } catch {
      case ex: FileNotFoundException => {
        println("Problema al guardar el archivo \"datos.dat\".")
      }
    }
  }

  //Carga en el juego el estado anterior deserailizando lo guardado anteriomente.
  //Comprueba además si existe y si el usuario quiere cargarlo.
  def cargar(): Juego = {
    try {
      val fis = new FileInputStream("datos.dat")
      val ois = new ObjectInputStream(fis)
      val j = ois.readObject.asInstanceOf[Juego]
      fis.close()
      j
    } catch {
      //No tiene ningun archivo guardado si no lo encuentra.
      case e: FileNotFoundException  => new Juego(0, 0, 0, Nil, 0, Nil, 0, null)
      case e: ClassNotFoundException => new Juego(0, 0, 0, Nil, 0, Nil, 0, null)
      case e: RuntimeException       => new Juego(0, 0, 0, Nil, 0, Nil, 0, null)
      case e: IOException            => new Juego(0, 0, 0, Nil, 0, Nil, 0, null)
    }
  }

  class Juego(col: Int, fil: Int, dif: Int, diam: List[Int], pts: Int, tab: List[Int], nmovs: Int, modo: String) extends Serializable {
    def getCol(): Int = col
    def getFil(): Int = fil
    def getDif(): Int = dif
    def getDiam(): List[Int] = diam
    def getPts(): Int = pts
    def getTab(): List[Int] = tab
    def getNmovs(): Int = nmovs
    def getModo(): String = modo
  }

  //Creamos un frame nuevo con el tablero:
  def crearFrame(pts: Int, nmovs: Int, col: Int, fil: Int, dif: Int, modo: String, tablero: List[Int], diamantes: List[Int], ec: Boolean): Frame = {
    //Si existen movimientos en la situación actual y se ha decidido eliminarlo, aún no se genera el tablero.
    if (ec && movimientos_situacion_actual(col, fil, tablero)) {
      //Limpiamos los conjuntos y llamamos nuevamente a la función
      val conjuntoConCeros = ScalaLegends.limpiar_conjuntos_del_tablero(col, fil, tablero)
      val tableroAhoraSiNuevo = ScalaLegends.mover_hacia_abajo(col, fil, conjuntoConCeros, diamantes)
      crearFrame(pts, nmovs, col, fil, dif, modo, tableroAhoraSiNuevo, diamantes, ec)
    } else {
      //Se crea el frame con el tablero
      new Tablero(pts, nmovs, col, fil, dif, modo, tablero, diamantes, ec)
    }
  }

}