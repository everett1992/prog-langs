object SudokuSolver extends App {

  class Sudoku(puzzle: List[Int]) {
    if (puzzle.length % Math.sqrt(puzzle.length) != 0 ||
        Math.sqrt(puzzle.length) % Math.sqrt(Math.sqrt(puzzle.length)) != 0) {
      throw new IllegalArgumentException("Puzzle must be square with square sides")
    }

    val size = Math.sqrt(puzzle.length).toInt

    def solution: Option[Sudoku] = {
      if (is_solved) { return Option(this) }

      val i = puzzle.zipWithIndex
        .filter(e => e._1 == 0)
        .sortBy(i => legal_values(i._2).length)
        .head._2

      legal_values(i).map(value =>
        fill(i, value).solution
      ).filter(opz => opz.exists(_.is_solved)).map(_.get).headOption
    }

    def fill(i: Int, value: Int): Sudoku = {
      new Sudoku(puzzle.updated(i, value))
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


    def rows: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => row_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }

    def cols: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => col_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }

    def boxes: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => box_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }

    def legal_values(n:Int): List[Int] = {
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
      val puzzle_string = puzzle.map( e => if (e == 0) " " else e.toString )

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
  println(sudoku.solution.get)
}
