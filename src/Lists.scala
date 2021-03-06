import scala.annotation.tailrec
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
   
    def range(start : Int, end : Int) : List[Int] = {
        if(start > end) List()
        else start :: range(start + 1, end)
      
    } 
   
    def reverse(xs : List[Int]) : List[Int] = {
        def revHelper(xs : List[Int], revd : List[Int]) : List[Int] = {
            if(xs == Nil) revd
            else revHelper(xs.tail, xs.head :: revd)
        }
        revHelper(xs, List())
    }
    
    def insertAt(x : Int, pos : Int, xs : List[Int]) : List[Int] = {
      
        def insertAtHelper(xs : List[Int], currPos : Int) : List[Int] = {
            if(currPos == pos) x :: xs
            else xs.head :: insertAtHelper(xs.tail, currPos + 1)
          
        }
        insertAtHelper(xs, 0)
    } 
    
    def removeAt(k : Int, xs : List[Int]) : List[Int] = {
           def removeAtHelper(pos : Int, xs : List[Int]) : List[Int] = {
               if(pos == k) xs.tail
               else xs.head :: removeAtHelper(pos + 1, xs.tail)
           }
          removeAtHelper(0, xs)      
    }
    
    def removeAtAndRet(k : Int, xs : List[Int]) : (Int, List[Int]) = {
           def removeAtHelper(pos : Int, xs : List[Int]) : (Int, List[Int]) = {
               if(pos == k) (xs.head, xs.tail)
               else {
                  val res = removeAtHelper(pos + 1, xs.tail)
                  (res._1, xs.head :: res._2)
               }
           }
          removeAtHelper(0, xs)      
    }
    
    def randomSelect(k : Int, xs : List[Int]) : List[Int] = {
            
            
            def randHelper(k : Int, xs : List[Int]) : List[Int] = {
              if(k == 0) xs
            else randHelper(k - 1, removeAt(scala.util.Random.nextInt(xs.length), xs))
              
            }
            
            randHelper(xs.length - k, xs)
    }
    
    
    def isPalindrome(xs : List[Int]) : Boolean = {
      
        xs == reverse(xs)
      
    }
    
    def split(xs : List[Int], x : Int) : (List[Int], List[Int]) = {
       
        def splitHelper(xs : List[Int], x : Int, curr : Int, acc : List[Int]) : (List[Int], List[Int]) = {
              if(xs == Nil) throw new Exception("Not valid!")
              else {
                  if(curr == x) (reverse(xs.head :: acc), xs.tail)
                  else splitHelper(xs.tail, x, curr + 1, xs.head :: acc)
              }
        }
        if(x == 0) (List(), xs)
        else splitHelper(xs, x, 1, List())      
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
      compressHelper(xs, '?').tail
      
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
    
    
    def drop(n : Int, xs : List[Int]) : List[Int] = {
        
         def dropHelper(n : Int, currVal : Int, xs : List[Int]) : List[Int] = {
               if(xs.length == 0) List()
               else if(currVal == 0) dropHelper(n, n, xs.tail)
               else xs.head :: dropHelper(n, currVal - 1, xs.tail)
           
         }
      dropHelper(n - 1, n - 1, xs)
    }
    
    def slice(start : Int, end : Int, xs : List[Int]) : List[Int] = {
        
        // Precondition check.
        require(end >= start)
        
        def sliceHelper(start : Int, end : Int, curr : Int, xs : List[Int]) : List[Int] = {
            if(curr == start) xs.head :: sliceHelper(start, end, curr + 1, xs.tail)
            else if(curr == end) List(xs.head)
            else if(curr > end) List()
            else sliceHelper(start, end, curr + 1, xs.tail)
          
        }
        sliceHelper(start, end, 0, xs) 
    }
    
    def rotate(n : Int, xs : List[Int]) : List[Int] = {
      
        def rotateHelper(n : Int, xs : List[Int], taken : List[Int]) : List[Int] = {
              if(n == 0) xs ++ reverse(taken)
              else rotateHelper(n - 1, xs.tail, xs.head :: taken)
        }
        
        if(n >= 0) rotateHelper(n, xs, List())
        else rotateHelper(n + xs.length, xs, List())
    }
    
    def msort[T](xs : List[T], less : (T, T) => Boolean) : List[T] = {
        
        def merge[T](xs : List[T], ys : List[T], less : (T, T) => Boolean) : List[T] = {
                (xs, ys) match {
                  case (Nil, Nil) => Nil
                  case (Nil, ys) => ys
                  case (xs, Nil) => xs
                  case (x :: xs1, y :: ys1) =>
                        if(less(x, y)) x :: merge(xs1, ys, less)
                        else y :: merge(xs, ys1, less)
                }
         
         }
        
         val mid = xs.length / 2
         if(mid == 0) xs
         else
         {
          val (one, two) = xs.splitAt(mid)
          merge(msort(one, less), msort(two, less), less) 
          
        }
    }
    
    def lotto(start : Int, end : Int, n : Int) : List[Int] = {
      
        @tailrec
        def lottoHelper(xs : List[Int], n : Int, acc : List[Int]) : List[Int] = {
            if(n == 0) acc.reverse
            else {
              val k = scala.util.Random.nextInt(xs.length)
              val (removed, remaining) = removeAtAndRet(k, xs)
              lottoHelper(remaining, n - 1, removed :: acc)
            }
        }
        lottoHelper(range(start, end + 1), n, List())
    }
      
    
    def lsort(xs : List[List[Int]]) : List[List[Int]] = {
      
        msort[List[Int]](xs, (x, y) => x.length < y.length)      
    }
    
    /*
     * 
     * Extra random stuff.
     */
    
    def splitter(x : Int, xs : List[Int], acc : List[Int]) : (List[Int], List[Int]) = {
        xs match {
          case Nil => (acc.reverse, Nil)
          case a :: xs1 =>
               if (a > x) (acc.reverse, a :: xs1)
               else splitter(x, xs1, a :: acc)
        }
    
    }                                 
  
    def validBST(xs : List[Int]) : Boolean = {
        xs match {
          case Nil => true
          case x :: xs1 =>
              val (smaller, greater) = splitter(x, xs1, List())
              if(smaller.forall{_  <= x} && greater.forall{_ > x}) validBST(smaller) && validBST(greater)
              else false
        }
    }   
    
    def deepReverse(xs : List[Any]) : List[Any] = {
      
        xs match {
          case Nil => Nil
          case x :: xs1 =>
              x match {
                case _ : Int => deepReverse(xs1) ++ List(x)
                case y : List[Any] => deepReverse(xs1) ++ List(deepReverse(y))
              }
        }
    }

}