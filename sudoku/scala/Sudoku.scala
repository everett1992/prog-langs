// Caleb Everett                                                     02/27/2013
// TCNJ - Programming Languages                                   Scala Project

object SudokuSolver extends App {

  class Sudoku(var puzzle: List[Int]) {
    // Check that the passed list can be treated as a square two dimentional
    //array, where each edge is a square length.
    if (puzzle.size % Math.sqrt(puzzle.size) != 0 ||
        Math.sqrt(puzzle.size) % Math.sqrt(Math.sqrt(puzzle.size)) != 0) {
      throw new IllegalArgumentException("Puzzle must be square with square sides")
    }

    // The edge-size of the sudoku puzzle.
    val size = Math.sqrt(puzzle.size).toInt

    // Return the solution to this puzzle, if one exists.
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

      legal_values(i).par.map(value =>
        new Sudoku(puzzle.updated(i, value)).solution
      ).filter(opz => opz.exists(_.is_solved)).map(_.get).headOption
    }

    // Fill in any space that only has one legal value.
    // TODO: rewrite this to remove the `set_single_value` function.
    def naked_singles: Boolean = {
      // This is explicitly defined because scala doesn't support multi line
      // simple expressions
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

    // Fill in any spaces in each row, column, or box where that space
    // is the only space that can accept that value.
    def hidden_singles: Boolean = {
      /*            ______               _ __    __     ____
                   /_  __/__  __________(_) /_  / /__  / / /
                    / / / _ \/ ___/ ___/ / __ \/ / _ \/ / /
                   / / /  __/ /  / /  / / /_/ / /  __/_/_/
                  /_/  \___/_/  /_/  /_/_.___/_/\___(_|_)

         Add scala to the list of languages I shouldn't be allowed near */
      def get_hidden_singles(groups: Map[Int, List[(Int, Int)]]) = {
        groups.par.map(e => e._2.filter(_._1 == 0))
          .map(e => e.map(n => (legal_values(n._2), n._2)))
          .map(group => (1 to size).map( value =>
            (value, group.filter(_._1.contains(value)).map(_._2)))
            .filter(e => e._2.size == 1).map(e => (e._1, e._2.head))
          ).flatten
      }

      // TODO: combine these calls functionally.
      val hidden_row_singles = get_hidden_singles(rows_with_index)
      hidden_row_singles.foreach(e => puzzle = puzzle.updated(e._2, e._1))

      val hidden_col_singles = get_hidden_singles(cols_with_index)
      hidden_col_singles.foreach(e => puzzle = puzzle.updated(e._2, e._1))

      val hidden_box_singles = get_hidden_singles(boxes_with_index)
      hidden_box_singles.foreach(e => puzzle = puzzle.updated(e._2, e._1))

      hidden_row_singles.size >= 1 || hidden_col_singles.size >= 1 || hidden_box_singles.size >= 1
    }

    // True if the puzzle is completely filled, and legal.
    def is_solved: Boolean = {
      puzzle.forall( e => e != 0 ) && is_legal
    }

    // True if each row, column, and box doesn't have any repeated values.
    def is_legal: Boolean = {
      Set(rows, cols, boxes)
        .map(_.values.toList).forall(v => v.distinct.size == v.size)
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
        (e._1, e._2.map(n => n._1)))
    }

    // Map of columns by column id
    def cols: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => col_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }

    // Map of boxes by box id
    def boxes: Map[Int, List[Int]] = {
      puzzle.zipWithIndex.groupBy( e => box_id(e._2) ).map( e =>
        (e._1, e._2.map(n => n._1))
      )
    }

    // Same as rows, cols, and boxes, but includes the index of each cell.
    def cols_with_index: Map[Int, List[(Int, Int)]] = {
      puzzle.zipWithIndex.groupBy( e => col_id(e._2) ).map( e => (e._1, e._2) )
    }
    def rows_with_index: Map[Int, List[(Int, Int)]] = {
      puzzle.zipWithIndex.groupBy( e => row_id(e._2) ).map( e => (e._1, e._2) )
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

  def parse = {
    val char_map = (" " :: (1 to 9).toList.map(_.toString) ::: (if (size > 9) ('A' to ('A' + (size - 9)).toChar).map(_.toString).toList else Nil),
      0 :: (1 to size).toList,
    ).zipped.toMap
  }

  // Print the sudoku puzzle all pretty like.
  def pretty(s: Sudoku): String = {
    val puzzle = s.puzzle
    val size = s.size

    // Map of an integer to it's alpha numeric representation.
    val char_map = (0 :: (1 to size).toList,
      " " :: (1 to 9).toList.map(_.toString) ::: (if (size > 9) ('A' to ('A' + (size - 9)).toChar).map(_.toString).toList else Nil)
    ).zipped.toMap

    // Map list of integers to list of strings, replace 0 with empty string
    val puzzle_string = puzzle.map( e => char_map.apply(e) )

    // Returns the passed List as a string, with the minor string placed inter-box
    // and the major between boxes
    val boxed = (arr: List[String], min: String, maj: String) =>
      arr.grouped(Math.sqrt(arr.size).toInt).map ( n => n.mkString(min) ).mkString(maj)

    // 2d array representation of the following string.
    // NOTE: to change the output format modify this string.
    val so =
      """┏━┯━┳━┯━┓
         ┃ │ ┃ │ ┃
         ┠─┼─╂─┼─┨
         ┃ │ ┃ │ ┃
         ┣━┿━╋━┿━┫
         ┃ │ ┃ │ ┃
         ┠─┼─╂─┼─┨
         ┃ │ ┃ │ ┃
         ┗━┷━┻━┷━┛""".split("\n").map( _.trim.toArray )

    // return the box drawing char at position x,y
    val bc = ( (x: Int, y: Int) => so.apply(x).apply(y).toString )

    // Formats the lines of the puzzle containing numbers.
    val main = puzzle_string.grouped(size).map( row =>
      bc(1,0) + boxed(row.map ( n => n ), bc(1,2), bc(1,4)) + bc(1,8) + "\n"
    ).toList

    // Horizontal inter-box lines
    val n = bc(2,0) + boxed((0 until size).toList.map(e=>bc(2,1)), bc(2,2), bc(2,4)) + bc(2,8) + "\n"
    // Horizontal box dividors
    val m = bc(4,0) + boxed((0 until size).toList.map(e=>bc(4,1)), bc(4,2), bc(4,4)) + bc(4,8) + "\n"

    bc(0,0) + boxed((0 until size).toList.map(e => bc(0,1)), bc(0,2), bc(0,4)) + bc(0,8) + "\n" +
    boxed(main, n, m) +
    bc(8,0) + boxed((0 until size).toList.map(e => bc(8,1)), bc(8,2), bc(8,4)) + bc(8,8) + "\n"
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
