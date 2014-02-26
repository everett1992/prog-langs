object SudokuSolver extends App {

  class Sudoku(puzzle: List[Int]) {
    if (puzzle.length % Math.sqrt(puzzle.length) != 0 ||
        Math.sqrt(puzzle.length) % Math.sqrt(Math.sqrt(puzzle.length)) != 0) {
      throw new IllegalArgumentException("Puzzle must be square with square sides")
    }

    val size = Math.sqrt(puzzle.length).toInt

    def solution: Sudoku = {
      this
    }


    override def toString: String = {
      val puzzle_string = puzzle.map( e => e.toString )
      val boxed = (arr: List[String], min: String, maj: String) =>
        arr.grouped(Math.sqrt(size).toInt).map ( n => n.mkString(min) ).mkString(maj)

      val main = puzzle_string.grouped(size).map( row =>
        "|"+ boxed(row.map ( n => n ), " ", "|") + "|\n"
      ).toList

      val n = "|" + boxed((0 until size).toList.map(e=>" "), " ", "|") + "|\n"
      val m = "|" + boxed((0 until size).toList.map(e=>"-"), "-", "+") + "|\n"

      "+" + boxed((0 until size).toList.map(e=>"-"), "-", "+") + "\n" +
      boxed(main, n, m) +
      "+" + boxed((0 until size).toList.map(e=>"-"), "-", "+") + "+"
    }
  }

  val sudoku = new Sudoku( List(
    8, 0, 0, 0, 0, 4, 2, 0, 0,
    3, 0, 0, 0, 5, 0, 0, 6, 0,
    5, 0, 0, 0, 3, 2, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 4, 2,
    0, 2, 1, 0, 0, 0, 3, 8, 0,
    4, 7, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 3, 9, 0, 0, 0, 6,
    0, 8, 0, 0, 7, 0, 0, 0, 5,
    0, 0, 6, 5, 0, 0, 0, 0, 9) )

  println(sudoku.toString())
}
