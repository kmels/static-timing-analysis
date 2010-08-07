package main

object Main extends Application{
  val pathToFile = "/home/kmels/tmp/ie2005"

  try {
    val c:Option[Circuit] = Parser.parse(io.Source.fromFile(pathToFile).mkString)
    println(c)
  } catch{
    case e:InputError => println(e.toString)
  }
  
}