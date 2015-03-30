class MultiwayTrees 
{
  
  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
}
  
    def nodeCount[T](mtree : MTree[T]) : Int = {
        1 + (mtree.children.map { x => nodeCount(x) }.sum)
    }
}