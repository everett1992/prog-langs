object SudokuSolver extends App {

  class Sudoku(var puzzle: List[Int]) {
    if (puzzle.size % Math.sqrt(puzzle.size) != 0 ||
        Math.sqrt(puzzle.size) % Math.sqrt(Math.sqrt(puzzle.size)) != 0) {
      throw new IllegalArgumentException("Puzzle must be square with square sides")
    }

    val size = Math.sqrt(puzzle.size).toInt

    def solution: Option[Sudoku] = {
      if (is_solved) { return Option(this) }

      var did_naked_single = true
      var did_hidden_single = true
      while (did_naked_single || did_hidden_single) {
        did_naked_single = naked_singles
        did_hidden_single = hidden_singles
      }

      if (is_solved) { return Option(this) }

      // Start guessing values for the cell with the fewest possible solutions.
      val i = puzzle.zipWithIndex
        .filter(e => e._1 == 0)
        .sortBy(i => legal_values(i._2).size)
        .head._2

      legal_values(i).map(value =>
        fill(i, value).solution
      ).filter(opz => opz.exists(_.is_solved)).map(_.get).headOption
    }

    def naked_singles: Boolean = {
      def set_single_value(e: (Int, Int)): Boolean = {
        val values = legal_values(e._2)
        if (values.size == 1) {
          puzzle = puzzle.updated(e._2, values.head)
          true
        } else {
          false
        }
      }

      puzzle.zipWithIndex
        .filter(_._1 == 0)
        .count(e => set_single_value(e)) >= 1
    }

    def hidden_singles: Boolean = {

      // Add scala to the list of languages I shouldn't be allowed near
      val hidden_singles = rows_with_index.map(e => e._2.filter(_._1 == 0))
        .map(e => e.map(n => (legal_values(n._2), n._2)))
        .map(row => (1 to size).map( value =>
          (value, row.filter(_._1.contains(value)).map(_._2)))
          .filter(e => e._2.size == 1).map(e => (e._1, e._2.head))
        ).flatten

      hidden_singles.foreach(e => puzzle = puzzle.updated(e._2, e._1))
      hidden_singles.size >= 1
    }

    def fill(i: Int, value: Int): Sudoku = {
      new Sudoku(puzzle.updated(i, value))
    }

    // True if the puzzle is completely filled,
    // and legal.
    def is_solved: Boolean = {
      puzzle.forall( e => e != 0 ) && is_legal
    }

    // True if each row, column, and box
    // doesn't have repeated values.
    def is_legal: Boolean = {
      Set(rows, cols, boxes).map(_.values.toList).forall(values =>
        // If the set of distinct values in group is equal
        // to the group there are no duplicates
        values.distinct.size == values.size
      )
    }

    // Index of the cell's row.
    def row_id(n: Int): Int = { n / size }

    // Index of the cell's column.
    def col_id(n: Int): Int = { n % size }

    // Index of the cell's box.
    def box_id(n: Int): Int = {
      val sqrt_size = Math.sqrt(size).toInt
      (row_id(n) / sqrt_size) + (col_id(n) / sqrt_size) * sqrt_size
    }

    // Map of rows by row id
    def rows: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => row_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }
    def rows_with_index: Map[Int, List[(Int, Int)]] = {
      puzzle.zipWithIndex.groupBy( e => row_id(e._2) ).map( e => (e._1, e._2) )
    }

    // Map of columns by column id
    def cols: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => col_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }
    def cols_with_index: Map[Int, List[(Int, Int)]] = {
      puzzle.zipWithIndex.groupBy( e => col_id(e._2) ).map( e => (e._1, e._2) )
    }

    // Map of boxes by box id
    def boxes: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => box_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }
    def boxes_with_index: Map[Int, List[(Int, Int)]] = {
      puzzle.zipWithIndex.groupBy( e => box_id(e._2) ).map( e => (e._1, e._2) )
    }

    // Set of all legal values for the cell n
    def legal_values(n: Int): List[Int] = {
      puzzle.apply(n) match {
        case 0 =>
        (1 to size).toList
          .diff(rows.apply(row_id(n)))
          .diff(cols.apply(col_id(n)))
          .diff(boxes.apply(box_id(n)))
        case _ => List()
      }
    }

  }

  def pretty(s: Sudoku): String = {
    val puzzle = s.puzzle
    val size = s.size

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

  val sudoku2 = new Sudoku( List(
    0, 0, 0, 0, 0, 9, 11, 0, 0, 2, 0, 0, 13, 5, 10, 0,
    9, 1, 0, 4, 0, 0, 3, 0, 10, 0, 7, 0, 0, 0, 12, 0,
    11, 0, 0, 2, 0, 0, 10, 0, 0, 9, 0, 0, 0, 0, 3, 0,
    12, 0, 0, 0, 13, 0, 0, 1, 8, 0, 11, 15, 0, 4, 6, 0,
    0, 0, 0, 0, 11, 13, 0, 16, 3, 0, 6, 10, 7, 0, 0, 0,
    0, 15, 0, 11, 8, 12, 0, 0, 13, 16, 9, 7, 0, 0, 0, 1,
    3, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 4, 6,
    6, 7, 0, 5, 4, 3, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0,
    0, 0, 0, 8, 7, 0, 0, 0, 0, 0, 10, 9, 15, 0, 1, 3,
    16, 2, 3, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 12, 0, 10,
    4, 0, 0, 0, 3, 16, 1, 15, 0, 0, 2, 8, 9, 0, 5, 0,
    0, 0, 0, 1, 5, 10, 0, 6, 4, 0, 3, 16, 11, 0, 0, 0,
    0, 6, 11, 0, 2, 1, 0, 3, 5, 0, 0, 4, 0, 0, 0, 15,
    0, 3, 0, 0, 0, 0, 9, 0, 0, 1, 0, 0, 5, 0, 0, 4,
    0, 5, 0, 0, 0, 15, 0, 4, 0, 11, 0, 0, 3, 8, 2, 12,
    0, 4, 2, 15, 0, 0, 13, 11, 0, 7, 8, 0, 0, 0, 0, 0) )

  println(pretty(sudoku))
  println(pretty(sudoku.solution.get))

  println(pretty(sudoku2))
  println(pretty(sudoku2.solution.get))
}
