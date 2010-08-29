package main

object Main extends Application{
  val pathToFile = "/home/kmels/tmp/ie2005_3"

  try {
    val c:Option[Circuit] = Parser.parse(io.Source.fromFile(pathToFile).mkString)
    println(c)
    println(c.get.routes)
  } catch{
    case e:InputError => println(e.toString)
  }
  
}
