class Trees {
  
  sealed abstract class Tree[+T]
  
  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
    def getLeft() : Tree[T] = left
    def getRight() : Tree[T] = right
    def isLeaf() : Boolean = (left == End && right == End)
    def getValue() : T = value
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
  /*
  
  def fromString(tree : String) : Tree[Char] = {
    
      if(tree.length() == 1) new Node(tree(0), End, End)
      else {
        
          val start = 2
          val end = tree.length() - 2
          if(tree(start) == ',') new Node(tree(0), End, fromString(tree.substring(start + 1, end + 1)))
          else {
            
              
          }
          
      }
    
  }
 
  */
  
  
  
  def stringRepr[T](tree : Tree[T]) : String = {
     tree match {
       case End => ""
       case node : Node[T] =>
         if(node.isLeaf()) node.getValue().toString()
         else node.getValue().toString() + 
         "(" + stringRepr(node.getLeft()) + "," + 
         stringRepr(node.getRight()) + ")"
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
  
  def atLevel[T](tree : Tree[T], lvl : Int) : List[T] = {
        def atLvlHelper[T](tree : Tree[T], lvl : Int, currLvl : Int) : List[T] = {
          tree match {
               
                case End => throw new Exception("Illegal lvl.")
                case node : Node[T] =>
                  if(currLvl == lvl) List(node.getValue())
                  else atLvlHelper(node.getLeft(), lvl, currLvl + 1) ++ atLvlHelper(node.getRight(), lvl, currLvl + 1)
              }
        }
    atLvlHelper(tree, lvl, 0)  
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
  
  /*
   *  Returns a forest of completely balanced binary trees. 
   */
  
  def cBalanced[T](num : Int, value : T) : List[Tree[T]] = {
         
      if(num == 1) List(new Node(value, End, End))
      else if(num == 0) List(End)
      else {
        if(num % 2 != 0) {
            val nodes = num - 1  
            for(left <- cBalanced[T](nodes / 2, value); right <- cBalanced[T](nodes / 2, value)) 
              yield new Node[T](value, left, right)
        }
        
        else {
          val nodes = num - 1
          val rightOdd = for(left <- cBalanced[T](nodes / 2, value); right <- cBalanced[T]((nodes - nodes / 2), value)) 
              yield new Node[T](value, left, right)
            
          val leftOdd = for(left <- cBalanced[T]((nodes - nodes / 2), value); right <- cBalanced[T](nodes / 2, value)) 
              yield new Node[T](value, left, right)
              rightOdd ++ leftOdd
        }
        
      }
  }
  
  
  def test() = {
    
    val g = new Node("g", End, End)
    val f = new Node("f", g, End)
    val c = new Node("c", End, f)
    val d = new Node("d", End, End)
    val e = new Node("e", End, End)
    val b = new Node("b", d, e)
    val a = new Node("a", b, c)
    println(stringRepr(a))
    
  }
}