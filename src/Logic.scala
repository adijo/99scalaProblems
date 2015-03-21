import scala.collection.mutable.PriorityQueue;
import scala.collection.mutable.HashMap;

class Logic {

    def and(a : Boolean, b : Boolean) : Boolean = a && b
    def or(a : Boolean, b : Boolean) : Boolean = a || b
    def not(a : Boolean) : Boolean = !a
    def nor(a : Boolean, b : Boolean) : Boolean = not(or(a, b))
    def nand(a : Boolean, b : Boolean) : Boolean = not(and(a, b))
    def xor(a : Boolean, b : Boolean) : Boolean = {
        val mid = nand(a, b)
        val c1 = nand(mid, a)
        val c2 = nand(mid, b)
        nand(c1, c2)
    }
    
    def impl(a : Boolean, b : Boolean) : Boolean = {
        or(and(a, b), a)
    }
    
    def table2(f : (Boolean, Boolean) => Boolean)  = {
        println("A" + "       " + "B" + "       " + "result")
        println("true" + "       " + "true" + "       " + (f(true, true)))
        println("true" + "       " + "false" + "       " + (f(true, false)))
        println("false" + "       " + "true" + "       " + (f(false, true)))
        println("false" + "       " + "false" + "       " + (f(false, false)))
      
      
    }
    
    // TODO: Make empty node a singleton object using case classes.
    
    class TreeNode[T](value : T, priority : Int, 
        left : TreeNode[T], right : TreeNode[T]) {
        def getVal() : T = value
        def getPriority() : Int = priority
        def isLeaf() : Boolean = (left == null && right == null)
        def getLeft() : TreeNode[T] = left
        def getRight() : TreeNode[T] = right
    }
    
    def huffman(xs : List[(Char, Int)]) : List[(Char, String)] = {
        def order(a : (Char, Int), b : (Char, Int)) : Int = a._2
        val nodes = xs.map(x => new TreeNode[Char](x._1, x._2, null, null))
        val pq = PriorityQueue.empty[TreeNode[Char]](
            Ordering.by { node : TreeNode[Char] => node.getPriority() }).reverse
        nodes.foreach { x => pq.+=(x) }
        val root = treeBuilder(pq)
        populate(root, "").sortWith((a, b) => a._2.length() < b._2.length())
        
    }
    
    def treeBuilder(pq : PriorityQueue[TreeNode[Char]]) : TreeNode[Char] = {
          def TERMINAL = '?'
          if(pq.length == 1) pq.head
          else {
            val (first, second) = (pq.dequeue(), pq.dequeue())
            val fuse = new TreeNode(TERMINAL, 
                first.getPriority() + second.getPriority(), first, second)
            treeBuilder(pq.+=(fuse))
          }
    }
    
    def populate(node : TreeNode[Char],  path : String) : List[(Char, String)] = {
        if(node.isLeaf()) List((node.getVal(), path))
        else populate(node.getLeft(), path + "0") ++ populate(node.getRight(), path + "1")
    }
    
     
    
}