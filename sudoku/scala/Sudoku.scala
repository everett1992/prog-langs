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

    def is_solved: Boolean = {
      puzzle.forall( e => e != 0 ) && is_legal
    }

    def is_legal: Boolean = {
      true
    }

    def row_id(n: Int): Int = { n / size }
    def col_id(n: Int): Int = { n % size }
    def box_id(n: Int): Int = {
      val sqrt_size = Math.sqrt(size).toInt
      (row_id(n) / sqrt_size) + (col_id(n) / sqrt_size) * sqrt_size
    }

    def rows: List[List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => row_id(e._2) ).map( n =>
        n._2.map( e => e._1)
      ).toList
    }

    def cols: List[List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => col_id(e._2) ).map( n =>
        n._2.map( e => e._1)
      ).toList
    }

    def boxes: List[List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => box_id(e._2) ).map( n =>
        n._2.map( e => e._1)
      ).toList
    }

    def legal_values(n:Int): List[Int] = {
      println("row")
      println(rows.apply(row_id(n)))
      println("col")
      println(cols.apply(col_id(n)))
      println("box")
      println(boxes.apply(box_id(n)))
      puzzle.apply(n) match {
        case 0 =>
        (1 to size).toList
          .diff(rows.apply(row_id(n)))
          .diff(cols.apply(col_id(n)))
          .diff(boxes.apply(box_id(n)))
        case _ => List()
      }
    }

    override def toString: String = {
      // Map list of integers to list of strings, replace 0 with empty string
      //val puzzle_string = puzzle.map( e => if (e == 0) " " else e.toString )
      val puzzle_string = puzzle.indices.toList.map( e => box_id(e).toString)

      puzzle_string.grouped(size).map( lst => lst.mkString(" ") ).mkString("\n")

      // A funtion that joins a list with charecters to print
      val boxed = (arr: List[String], min: String, maj: String) =>
        arr.grouped(Math.sqrt(size).toInt).map ( n => n.mkString(min) ).mkString(maj)

      // Formats the main body of the puzzle
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

  println(sudoku)
  println(sudoku.legal_values(2))
  println(sudoku.legal_values(4))
}
