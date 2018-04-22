# Jewels en Scala

## Introducción

Juego de diamantes en el que debes unir 3 o más para poder destruirlos y conseguir el mayor número de puntos posible.

## Motivación

Esta aplicación fue creada como práctica por la asignatura "Ampliación de Programación Avanzada" en la Universidad de Alcalá.

La Interfaz gráfica era algo opcional, ¿pero no queda mejor viendo los diamantes?

## Instalación

Para poder utilizar el juego deberás tener instalado un IDE como eclipse que permita la ejecución de Scala (en este enlace se muestra un tutorial: https://doc.akka.io/docs/akka/1.3.1/intro/getting-started-first-scala-eclipse.html), bajar el proyecto, incluir el .jar de la carpeta jar_a_incluir/ (https://github.com/miguelromeral/JewelScala/blob/master/jar_a_incluir/org.scala-lang.modules.scala-swing_1.0.1.jar) en las bibliotecas del proyecto y configurar la ejecución de aplicaciones en Scala.

## Funcionalidades y Capturas

### Diferentes modos de juego

* Tableros de 7x9, 11x17 y hasta 15x27 con 4, 5 y 7 tipos de diamantes respectivamente.

* Modo manual o automático (el ordenador juega solo)

![alt text](https://github.com/miguelromeral/JewelScala/blob/master/Capturas/Captura6.png)

### Guardar partida y recuperarla al inicio del programa

![alt text](https://github.com/miguelromeral/JewelScala/blob/master/Capturas/Captura5.png)

### Juego manual

Busca la mejor coincidencia y consigue la mayor puntuación. Ten en cuenta que los diamantes que se vayan añadiendo por arriba, si generan una nueva combinación, se eliminarán solos.

![alt text](https://github.com/miguelromeral/JewelScala/blob/master/Capturas/Captura3.png)
![alt text](https://github.com/miguelromeral/JewelScala/blob/master/Capturas/Captura4.png)

### Juego automático

Busca la combinación que más diamantes tenga juntos en todo el tablero y ejecuta esa combinación. Es imposible superar al ordenador, os lo aseguro.

Únicamente comprueba la mejor posiblidad en el tablero actual, sin tener en cuenta situaciones que podrían darse en el hipotético caso de romper una combinación.

![alt text](https://github.com/miguelromeral/JewelScala/blob/master/Capturas/Captura1.png)
![alt text](https://github.com/miguelromeral/JewelScala/blob/master/Capturas/Captura2.png)

* Os juro que está implementado el fin del programa (cuando no hay más combinaciones), pero tras 12 horas de ejecución automática no conseguí que finalizara el juego y comprobar que funciona.

## Pruebas

Como este programa se trata de una práctica para la universidad, fue sometido a bastantes pruebas para buscar bugs y tratar de arreglarlos. Esta fue la versión final que se entregó, la cual no tiene fallos bastante a la vista, por lo que es la versión más estable de todas las creadas.

## Agradecimientos

El diseño de los diamantes son creación propia de Pedro José Vacas. 

## Contacto

Correo electrónico: miguelangel.garciar@edu.uah.es, miguel.romeral@gmail.com

LinkedIn: Miguel Romeral (https://www.linkedin.com/in/miguelromeral/)

Twitter: @MiguelRomeral (https://twitter.com/MiguelRomeral)

## Licencia

Licencia Pública General GNU 3.0
