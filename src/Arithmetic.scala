class Arithmetic 
{
  def min(a : Int, b : Int) : Int = {
      if(a < b) a
      else b
  }
  
  def max(a : Int, b : Int) : Int = {
      if(a > b) a
      else b
  }
  
  def gcd(a : Int, b : Int) : Int = {
        if(b == 0) a
        else {
            val one = a % b
            gcd(max(one, b), min(one, b))
          
        }
  }
  

}