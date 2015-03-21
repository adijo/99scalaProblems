
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
    
    
    def isPalindrome(xs : List[Int]) : Boolean = {
      
        xs == reverse(xs)
      
    }
    
    def flatten(xs : List[Any]) : List[Any] = {
      
      if(xs.length == 0) xs      
      else {
      xs.head match {
          case _: Int => xs.head :: flatten(xs.tail)
          case a : List[Any] => flatten(a) ++ flatten(xs.tail)
          
        }
      }
      
    }
    
    def compress(xs : List[Character]) : List[Character] = {
        
        def compressHelper(xs : List[Character], last : Character) : List[Character] = {
              
              if(xs.length == 0) List(last)
              else if(xs.head == last) compressHelper(xs.tail, last)
              else last :: compressHelper(xs.tail, xs.head)
          
        }
      compressHelper(xs, 'z').tail
      
    }
    
    def pack(xs : List[Char]) : List[List[Char]] = {
      
        def generate(x : Char, n : Int) : List[Char] = {
            if(n == 0) List()
            else x :: generate(x, n - 1)
        }
        
        def packHelper(xs : List[Char], last : Char, num : Int) : List[List[Char]] = {
          
          if(xs.length == 0) Nil
          else {
              if(xs.head == last) packHelper(xs.tail, last, num + 1)
              else generate(last, num) :: packHelper(xs.tail, xs.head, 1)
          }
        }
        packHelper(xs.tail, xs.head, 1)      
    }
    
    def encode(xs : List[Char]) : List[(Int, Char)] = {
      
        def encodeHelper(xs : List[Char], last : Char, n : Int) : List[(Int, Char)] = {
          
            if(xs.length == 0) Nil
            else {
                if(xs.head == last) encodeHelper(xs.tail, last, n + 1)
                else (n, last) :: encodeHelper(xs.tail, xs.head, 1)
              
            }
          
        }
      encodeHelper(xs.tail, xs.head, 1)
    }
    
    

}