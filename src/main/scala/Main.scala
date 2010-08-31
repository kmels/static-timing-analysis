package main

object Main extends Application{
  val pathToFile = "/home/kmels/tmp/ie2005"

  try {
    val c:Option[Circuit] = Parser.parse(io.Source.fromFile(pathToFile).mkString)
    println(c)
    println(c.get.routes.mkString("\n.."))

    println("validas: "+c.get.routes.filter(_.isValid).mkString("\n.."))
  } catch{
    case e:InputError => println(e.toString)
  }
  
}
