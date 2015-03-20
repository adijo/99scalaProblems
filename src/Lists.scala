
class Lists 
{
  
    def last(xs : List[Int]) : Int = {
        
        xs match {
          case Nil => throw new Exception("Not a valid list.")
          case x :: Nil => x
          case x :: xs1 => last(xs1)
        }
    }
    
    
    def penultimate(xs : List[Int]) : Int = {
      
        def rev(xs : List[Int], revd : List[Int]) : List[Int] = {
            if(xs == Nil) revd
            else rev(xs.tail, xs.head :: revd)
          
        }
        
        xs match {
          case Nil => throw new Exception("Not a valid list")
          case x :: Nil => throw new Exception("Not a valid list")
          case xs => rev(xs, List()).tail.head
          
          
        }
      
    }
    
    def nth(n : Int, xs : List[Int]) : Int = {
        if(n == 0) xs.head
        else nth(n - 1, xs.tail)
      
    }
    
    def length(xs : List[Int]) : Int = {
        xs match {
          case Nil => 0
          case x :: xs1 => 1 + length(xs1)
          
        }
    }    
   
    def reverse(xs : List[Int]) : List[Int] = {
        def revHelper(xs : List[Int], revd : List[Int]) : List[Int] = {
            if(xs == Nil) revd
            else revHelper(xs.tail, xs.head :: revd)
        }
        revHelper(xs, List())
    }
    
    
    
    
    

}