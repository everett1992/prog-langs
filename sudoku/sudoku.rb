require './square'

# Sudoku puzzle class
class Sudoku
  attr_reader :puzzle

  def initialize puzzle
    raise "Sudoku must be a square 2d array" unless puzzle.square?
    raise "Sudoku size must be a square number, was #{puzzle.size}" unless puzzle.size.square?
    @puzzle = puzzle.each_with_index.map { |row, x| row.each_with_index.map { |cell, y| Square.new(x, y, cell) } }
  end

  # Returns legal values for the passed space
  def legal_values(x, y)
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
    each_empty_cell do |cell|
      pos = legal_values(*cell.pos)
      if pos.length == 1
        @puzzle[cell.x][cell.y].val = pos.first
        return pos.first
      end
    end
    return nil
  end

  def unique_candidate
    each_empty_cell do |cell|
      this = legal_values(*cell.pos)
      others = (row(*cell.pos) + col(*cell.pos) + box(*cell.pos))
        .reject { |s| s.pos == cell.pos }
        .map { |n| legal_values(*n.pos) }
        .flatten.uniq.sort

      pos = this - others
      if pos.length == 1
        cell.val = pos.first
        puts "unique_candidate, #{cell}"
        #puzzle[cell.x][cell.y].val = pos.first
        return pos.first
      end
    end
    return nil
  end

  # Iterate through all cells yielding x, y, and the cells
  # value to the block
  def each(&block)
    @puzzle.each do |row|
      row.each do |cell|
        yield cell
      end
    end
  end

  # Same as #each, but only yield to nil cells.
  def each_empty_cell(&block)
    each { |cell| yield cell unless cell.val }
  end

  def row(x, _)
    @puzzle[x]
  end

  def col(_, y)
    @puzzle.transpose[y]
  end

  def box(x, y)
    n = Math.sqrt(@puzzle.size).to_i

    (((x / n) * n)...((x / n + 1) * n)).map do |x_1|
      (((y / n) * n)...((y / n + 1) * n)).map do |y_1|
        @puzzle[x_1][y_1]
      end
    end.flatten
  end

  def filled?(x, y)
    !@puzzle[x][y].filled?
  end

  def to_s
    boxed = lambda { |arr, min, maj| arr.each_slice(Math.sqrt @puzzle.size).map { |n| n.join(min) }.join(maj) }

    main = @puzzle.map do |row|
      "┃#{boxed.call(row.map { |n| n.val || ' ' }, '│', '┃')}┃\n"
    end

    n = "┠#{boxed.call(Array.new(@puzzle.size, '─'), '┼', '╂')}┨\n"
    m = "┣#{boxed.call(Array.new(@puzzle.size, '━'), '┿', '╋')}┫\n"

    "┏#{boxed.call(Array.new(@puzzle.size, '━'), '┯', '┳')}┓\n" +
        boxed.call(main, n, m) +
    "┗#{boxed.call(Array.new(@puzzle.size, '━'), '┷', '┻')}┛"
  end
end
