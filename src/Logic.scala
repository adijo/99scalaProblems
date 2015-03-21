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
    
    
    
}