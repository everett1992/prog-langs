require './square'

# Sudoku puzzle class
class Sudoku
  attr_reader :puzzle

  def initialize puzzle
    raise "Sudoku must be a square 2d array" unless puzzle.square?
    raise "Sudoku size must be a square number, was #{puzzle.size}" unless puzzle.size.square?
    @puzzle = puzzle
  end

  # Returns legal values for the passed space
  def possibles(x, y)
    # Filled spaces have no possibilities.
    return [] if filled? x, y

    # Spaces are in contention with cells in the same row, colum, or box
    contention =
      (row(x, y) + col(x, y) + box(x, y))
      .map(&:val)

    # Return all valid numbers not in contention
    (1..@puzzle.size).to_a - contention
  end

  def next_move
    naked_single || unique_candidate
  end

  # Fill any cells that have one legal
  # value
  def naked_single
    each_empty_cell do |x, y, cell|
      pos = possibles(x,y)
      if pos.length == 1
        @puzzle[x][y] = pos.first
        return Square.new(x, y, pos.first)
      end
    end
    return nil
  end

  def unique_candidate
    each_empty_cell do |x, y, cell|
      this = possibles(x, y)
      others = (row(x, y) + col(x, y) + box(x, y))
        .reject { |s| s.pos == [x,y] }
        .map { |n| possibles(*n.pos) }
        .flatten.uniq.sort

      pos = this - others
      if pos.length == 1
        puts "unique_candidate"
        @puzzle[x][y] = pos.first
        return Square.new(x, y, pos.first)
      end
    end
    return nil
  end

  # Iterate through all cells yielding x, y, and the cells
  # value to the block
  def each(&block)
    @puzzle.each_with_index do |row, x|
      row.each_with_index do |cell, y|
        yield x, y, cell
      end
    end
  end

  # Same as #each, but only yield to nil cells.
  def each_empty_cell(&block)
    each { |x, y, cell| yield x, y, cell unless cell }
  end

  def row(x, _)
    @puzzle.dup[x]
      .each_with_index.map { |val, y| Square.new(x, y, val) }
  end

  def col(_, y)
    @puzzle.transpose[y]
      .each_with_index.map { |val, x| Square.new(x, y, val) }
  end

  def box(x, y)
    n = Math.sqrt(@puzzle.size).to_i

    (((x / n) * n)...((x / n + 1) * n)).map do |x_1|
      (((y / n) * n)...((y / n + 1) * n)).map do |y_1|
        Square.new(x_1, y_1, @puzzle[x_1][y_1])
      end
    end.flatten
  end

  def filled?(x, y)
    !@puzzle[x][y].nil?
  end

  def to_s
    boxed = lambda { |arr, min, maj| arr.each_slice(Math.sqrt @puzzle.size).map { |n| n.join(min) }.join(maj) }

    main = @puzzle.map do |row|
      "┃#{boxed.call(row.map { |n| n || ' ' }, '│', '┃')}┃\n"
    end

    n = "┠#{boxed.call(Array.new(@puzzle.size, '─'), '┼', '╂')}┨\n"
    m = "┣#{boxed.call(Array.new(@puzzle.size, '━'), '┿', '╋')}┫\n"

    "┏#{boxed.call(Array.new(@puzzle.size, '━'), '┯', '┳')}┓\n" +
        boxed.call(main, n, m) +
    "┗#{boxed.call(Array.new(@puzzle.size, '━'), '┷', '┻')}┛"
  end
end
