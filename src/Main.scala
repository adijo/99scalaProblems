
object Main {
 
  def main(args : Array[String]) : Unit = {
      val lists = new Lists()
      
      val one = List(1, 2, 3)
      val two = List(4, 66, 7, 8)
      val three = List(3, 45, 7, 87)
      val four = List(1)
      
      println(lists.lsort(List(one, two, three, four)))
  }
}