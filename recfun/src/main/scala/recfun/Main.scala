package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || r == 0) return 1
      else if(c == r) return 1
      return pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      var brCount = 0
      def myBalance(ch: Char, chars: List[Char]) : Boolean = {
        if (ch == '(') {
          brCount += 1
        }
        else if (ch == ')') {
          if(brCount>0) {
            brCount -= 1
          }
          else return false
        }
       if(chars.isEmpty) {
          return (brCount==0)
        }
        return myBalance(chars.head, chars.tail)
      }

      if (chars.isEmpty) return true
      return myBalance(chars.head, chars.tail)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      var numWays = 0
      def isPossibleForChange(amount: Int, change: Int) : Boolean ={
        return (change<=amount && amount%change==0)
      }

      def changer(coins: List[Int]) : Unit = {
        if (coins.isEmpty) return
        doChange(money, coins)
      }

      def doChange(amount:Int, coins: List[Int]) : Unit ={
        if (coins.isEmpty) return
        else if (amount<coins.head) return
        else if (isPossibleForChange(amount, coins.head)) numWays += 1
        for(i <- 1 to (amount/coins.head)) {
          doChange(amount-(coins.head*(i-1)), coins.tail)
        }
      }

      changer(coins.sorted)
      return numWays
    }
  }
