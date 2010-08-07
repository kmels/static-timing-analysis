package main

case class InputError(val msg:String) extends Exception{
  override def toString = "ERROR: "+msg
}
