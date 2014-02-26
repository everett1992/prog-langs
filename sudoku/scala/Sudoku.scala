object SudokuSolver extends App {
 
  class Puzzle(puzzle: List) {
    val rows = puzzle.groupBy( n => n % 2)
    val columns = puzzle.groupBy( n => n / 2)
    val boxes = 

    def solution {
      // find the first empty space
      // for each legal value for that space
      //   assign that value and recursivly call solve
      //   if the resultant puzzle is not solved try the next
      //   legal value
    }

    def legal_values(n: Int) {
    }

    // returns true if all spaces are filled,
    // and all spcaes are legal.
    def is_solved {
    }
  }
 
  val f2Str: List[Int] => String = fields => {
    val sepLine = "+---+---+---+"
    val sepPoints = Set(2,5,8)
    val fs: (Int, Int) => String = (i, v) => v.toString.replace("0"," ")+(if (sepPoints.contains(i%9)) "|" else "")
    sepLine+"\n"+(0 to fields.size-1).map(i => (if (i%9==0) "|" else "")+fs(i,fields(i))+(if (i%9==8) if (sepPoints.contains(i/9)) "\n"+sepLine+"\n" else "\n" else "")).foldRight("")(_+_)
  }
 
 
  val puzzle = new Puzzle(List(
    8, 0, 0, 0, 0, 4, 2, 0, 0,
    3, 0, 0, 0, 5, 0, 0, 6, 0,
    5, 0, 0, 0, 3, 2, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 4, 2,
    0, 2, 1, 0, 0, 0, 3, 8, 0,
    4, 7, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 3, 9, 0, 0, 0, 6,
    0, 8, 0, 0, 7, 0, 0, 0, 5,
    0, 0, 6, 5, 0, 0, 0, 0, 9)
  )


  println("puzzle:")
  println(f2Str(puzzle))
  var solution = puzzle.solution()

  println("solution:")
  println(solution match {
    case Nil => "no solution!!!"
    case _ => f2Str(solution)
  })
}
