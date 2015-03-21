
object Main {
 
  def main(args : Array[String]) : Unit = {
      val logic = new Logic()
      val huffVals = List(('a', 45), ('b', 13), ('c', 12), ('d', 16), ('e', 9), ('f', 5))
      val huffCodings = logic.huffman(huffVals)
      println(huffCodings)
  }
}