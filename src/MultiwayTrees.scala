class MultiwayTrees 
{
  
  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
}
  
    def nodeCount[T](mtree : MTree[T]) : Int = {
        1 + mtree.children.map { x => nodeCount(x) }.sum
    }
    
    def internalPathLength[T](mtree : MTree[T]) : Int = {
        
        def internalDepth[T](mtree : MTree[T], currDepth : Int) : Int = {
            currDepth + mtree.children.map { x => internalDepth(x, currDepth + 1) }.sum
        }
      internalDepth(mtree, 0)
    }
    
    def constructTree[T](chars : List[T]) : MTree[T] = {
        null
      
    }
    
    
    
    def test() = {
      val g = new MTree('g', List())
      val d = new MTree('d', List())
      val e = new MTree('e', List())
      val f = new MTree('f', List(g))
      val c = new MTree('c', List())
      val b = new MTree('b', List(d, e))
      val a = new MTree('a', List(f, c, b))
      
      println(internalPathLength(a))
      
    }
}