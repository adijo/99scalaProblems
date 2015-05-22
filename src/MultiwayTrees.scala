import scala.collection.mutable.ListBuffer
class MultiwayTrees 
{
  
  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
    def isLeaf() : Boolean = children.isEmpty
    def getValue() : T = value
    
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
    
    def lispyTree[T](mtree : MTree[T]) : String = {
        if(mtree.isLeaf()) " " + mtree.getValue().toString()
        else " ( " + mtree.getValue() + " " + mtree.children.map (x => lispyTree(x)).reduceLeft(_ + _) + " )" 
    }
    
    def fromString(mtree : List[Char]) : MTree[Char] = {
        var ptr = 1
        def fromStringHelper(node : MTree[Char]) : MTree[Char] = {
            var flag = true
            var children = ListBuffer[MTree[Char]]()
            while (flag) {
                if(ptr >= mtree.length || mtree(ptr) == '^') flag = false
                else {
                    ptr = ptr + 1
                    children += fromStringHelper(new MTree(mtree(ptr - 1)))
                }
            }
            ptr = ptr + 1
            new MTree(node.getValue(), children.toList)
            
        }
      fromStringHelper(new MTree(mtree(0)))
     
    }
    
    def postorder(mtree : List[Char]) : List[Char] = {
      
        def postorderHelper(mtree : MTree[Char]) : List[Char] = {
              if(mtree.isLeaf()) List(mtree.getValue())
              else {
                  val order = for(child <- mtree.children) yield postorderHelper(child)
                  order.flatten ++ List(mtree.getValue())
              }
        }
        postorderHelper(fromString(mtree))      
    }
    
    def test() = {
      
      val mtree = "afg^^c^bd^e^^^".toList
      println(postorder(mtree))
    }
}