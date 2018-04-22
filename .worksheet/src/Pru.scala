import java.io._

object Pru {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(89); 
  //Dificultad que le damos
  val dificultadJuego: Int = 2;System.out.println("""dificultadJuego  : Int = """ + $show(dificultadJuego ));$skip(56); 
	//Semilla
  val escogeColoresJuego = scala.util.Random;System.out.println("""escogeColoresJuego  : util.Random.type = """ + $show(escogeColoresJuego ));$skip(62); 
  //Lista vacia de cualquier tipo
  val coloresElegidos = Nil;System.out.println("""coloresElegidos  : scala.collection.immutable.Nil.type = """ + $show(coloresElegidos ));$skip(251); 
	//Insertar x en la lista si no lo contiene.
  def insert(x: Int, xs: List[Int]): List[Int] =
    if (!xs.contains(x)) {
      if (xs.isEmpty || x <= xs.head) x :: xs
      else xs.head :: insert(x, xs.tail)
    } else {
      insert(x + 1, xs)
    };System.out.println("""insert: (x: Int, xs: List[Int])List[Int]""");$skip(807); val res$0 = 
	//Según la dificultad mete los diamantes de un valor.
  if (dificultadJuego == 1)
    insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), coloresElegidos))))
  else if (dificultadJuego == 2)
    insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), coloresElegidos)))))
  else
    insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), insert(escogeColoresJuego.nextInt(7), coloresElegidos)))))));System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(124); 
  
  def imprimir_aux(l: List[Int]):String =
  if(l.isEmpty){
    "\n"
  }else{
  	l.head + " " + imprimir_aux(l.tail)
  };System.out.println("""imprimir_aux: (l: List[Int])String""");$skip(33); 
  
  val lista = List(1,2,3,4,5);System.out.println("""lista  : List[Int] = """ + $show(lista ));$skip(25); val res$1 = 
  
  lista.mkString(",");System.out.println("""res1: String = """ + $show(res$1));$skip(23); 
  lista.foreach(print);$skip(63); 
  
  
  def imprimir(l: List[Int])=
 	println(imprimir_aux(l));System.out.println("""imprimir: (l: List[Int])Unit""");$skip(21); 
 	
 	imprimir(lista);$skip(328); 
  
  
  def posicion_de_x_en_lista(x: Int, vuelta:Int, lista: List[Int], retorno: List[Int]): List[Int] = {
    if (lista.isEmpty)
      retorno
    else if (x == lista.head)
      posicion_de_x_en_lista(x, vuelta + 1, lista.tail, vuelta :: retorno)
    else
      posicion_de_x_en_lista(x, vuelta + 1, lista.tail, retorno)
  };System.out.println("""posicion_de_x_en_lista: (x: Int, vuelta: Int, lista: List[Int], retorno: List[Int])List[Int]""");$skip(53); 
  
  
  
  
   val pruebas = List(1,2,1,3,1,4,1,5,1);System.out.println("""pruebas  : List[Int] = """ + $show(pruebas ));$skip(46); val res$2 = 
   posicion_de_x_en_lista(1, 1, pruebas, Nil);System.out.println("""res2: List[Int] = """ + $show(res$2));$skip(589); 
  
    
  //Guarda el juego serializando los datos que se le pasan por parámetro.
  def guardar(col: Int, fil: Int, dif: Int, diamantes: List[Int], tablero: List[Int], pts: Int) {
    try {
      val fos = new FileOutputStream("datos.dat")
      val oos = new ObjectOutputStream(fos)
      oos.write(col)
      oos.write(fil)
      oos.write(dif)
      oos.writeObject(diamantes)
      oos.writeObject(tablero)
      oos.write(pts)
      fos.close()
    } catch {
      case ex: FileNotFoundException => {
        println("Problema al guardar el archivo \"datos.dat\".")
      }
    }
  };System.out.println("""guardar: (col: Int, fil: Int, dif: Int, diamantes: List[Int], tablero: List[Int], pts: Int)Unit""");$skip(1412); 

  //Carga en el juego el estado anterior deserailizando lo guardado anteriomente.
  //Comprueba además si existe y si el usuario quiere cargarlo.
  def cargar(): Juego = {
    try {
        try {
          val fis = new FileInputStream("datos.dat")
          val ois = new ObjectInputStream(fis)
          //Sin necesidad de cast???
          val col = ois.readObject.asInstanceOf[Int]
          val fil = ois.readObject.asInstanceOf[Int]
          val dif = ois.readObject.asInstanceOf[Int]
          val diamantes = ois.readObject.asInstanceOf[List[Int]]
          val pts = ois.readObject.asInstanceOf[Int]
          val tablero = ois.readObject.asInstanceOf[List[Int]]
          fis.close()
          new Juego(col, fil, dif, diamantes, pts, tablero)
        } catch {
          //No tiene ningun archivo guardado si no lo encuentra.
          case e: FileNotFoundException  => new Juego(0, 0, 0, Nil, 0, Nil)
          case e: ClassNotFoundException => new Juego(0, 0, 0, Nil, 0, Nil)
          case e: RuntimeException       => new Juego(0, 0, 0, Nil, 0, Nil)
          case e: IOException            => new Juego(0, 0, 0, Nil, 0, Nil)
        }
    } catch {
      //No tiene ningun archivo guardado si no lo encuentra.
      case e: FileNotFoundException => new Juego(0, 0, 0, Nil, 0, Nil)
      case e: IOException           => println("Got an IOException!"); new Juego(0, 0, 0, Nil, 0, Nil)
    }
  };System.out.println("""cargar: ()Pru.Juego""");$skip(69); 
  
  guardar(3,4,1,List(1,2,3,4),List(1,2,3,4,5,6,7,8,9,10,11,12),0);$skip(11); val res$3 = 
  cargar();System.out.println("""res3: Pru.Juego = """ + $show(res$3))}
  
  
  
  class Juego(col:Int, fil:Int, dif:Int, diam:List[Int], pts:Int, tab: List[Int]) extends Serializable {
  	val columnas = col
  	val filas = fil
  	val dificultad = dif
  	val diamantes = diam
  	val puntos = pts
  	val tablero = tab
  	override def toString = "C:"+columnas+", F:"+filas+", D:"+dificultad+", Di:"+diamantes+", P:"+puntos+", T:"+tablero
  }
  
  
}
