class Trees {
  
  sealed abstract class Tree[+T]
  
  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    def getLeft() : Tree[T] = left
    def getRight() : Tree[T] = right
    def isLeaf() : Boolean = (left == End && right == End)
  }
  case object End extends Tree[Nothing] {
    override def toString = "."
  }

  
  def leafCount[T](tree : Tree[T]) : Int = {
      
      tree match {
        case End => 0
        case node : Node[T] => 
          if(node.isLeaf()) 1
          else leafCount(node.getLeft()) + leafCount(node.getRight())
        
      }
  }
  
  def leafList[T](tree : Tree[T]) : List[Tree[T]] = {
    
     tree match {
       case End => List()
       case node : Node[T] =>
         if(node.isLeaf()) List(node)
         else leafList(node.getLeft()) ++ leafList(node.getRight())
       
     }
    
  }
  
  def internalList[T](tree : Tree[T]) : List[Tree[T]] = {
    
      tree match {
        case End => List()
        case node : Node[T] =>
          if(node.isLeaf()) List()
          else node :: (leafList(node.getLeft()) ++ leafList(node.getRight()))
        
      }
    
  }
  
  
  def symmetric[T](tree : Tree[T]) : Boolean = {

    def symHelper(one : Tree[T], two : Tree[T]) : Boolean = {
          (one, two) match {
            case (End, End) => true
            case (End, _ : Node[T]) => false
            case (_ : Node[T], End) => false
            case (one : Node[T], two : Node[T]) => symHelper(one.getLeft(), two.getRight()) && 
                                                   symHelper(one.getRight(), two.getLeft())
          }
      }
    
      tree match {
        case End => true
        case node : Node[T] => symHelper(node.getLeft(), node.getRight())
      }
  }  
  

}