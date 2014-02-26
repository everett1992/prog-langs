require './square'

# Sudoku puzzle class
class Sudoku
  include Enumerable
  attr_reader :pz, :cols, :rows, :boxes

  def initialize puzzle
    raise "Sudoku must be a square 2d array" unless puzzle.square?
    raise "Sudoku size must be a square number, was #{puzzle.size}" unless puzzle.size.square?
    @pz = puzzle.each_with_index.map { |row, x| row.each_with_index.map { |cell, y| Square.new(x, y, cell) } }

    @rows = @pz

    @cols = @pz.transpose

    @boxes = (0...@pz.size)
      .each_slice(Math.sqrt @pz.size).to_a
      .repeated_permutation(2)
      .map { |f, s| f.product(s).map { |x, y| @pz[x][y] } }
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
    possible_values.to_a - contention
  end

  def next_move
    naked_single || unique_candidate
  end

  def solve
    while next_move
    end
  end

  # Fill any cells that have one legal
  # value
  def naked_single
    each_empty_cell do |cell|
      vals = legal_values(*cell.pos)
      if vals.length == 1
        cell.val = vals.first
        return cell
      end
    end
    return nil
  end

  def unique_candidate
    [@rows, @cols, @boxes].each do |group_type|
      group_type.each do |group|
        Hash[possible_values.map do |val|
          [val, group.select { |cell| legal_values(*cell.pos).include? val }]
        end].each do |val, cells|
          if cells.size == 1
            cells.first.val = val
            return cells.first
          end
        end
      end
    end
    return nil
  end

  # Iterate through all cells yielding x, y, and the cells
  # value to the block
  def each(&block)
    @pz.each do |row|
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
    rows[x]
  end

  def col(_, y)
    cols[y]
  end

  def box(x, y)
    n = Math.sqrt(@pz.size).to_i
    boxes[(x / n * n) + (y / n)]
  end

  def filled?(x, y)
    @pz[x][y].filled?
  end

  def to_s
    boxed = lambda { |arr, min, maj| arr.each_slice(Math.sqrt @pz.size).map { |n| n.join(min) }.join(maj) }

    main = @pz.map do |row|
      "┃#{boxed.call(row.map { |n| n.val || ' ' }, '│', '┃')}┃\n"
    end

    n = "┠#{boxed.call(Array.new(@pz.size, '─'), '┼', '╂')}┨\n"
    m = "┣#{boxed.call(Array.new(@pz.size, '━'), '┿', '╋')}┫\n"

    "┏#{boxed.call(Array.new(@pz.size, '━'), '┯', '┳')}┓\n" +
        boxed.call(main, n, m) +
    "┗#{boxed.call(Array.new(@pz.size, '━'), '┷', '┻')}┛"
  end

  def self.blank(n)
    Sudoku.new Array.new(n, Array.new(n, nil))
  end

  def possible_values
    (1..@pz.size)
  end

  def solved?
    all?(&:filled?)
  end
end
