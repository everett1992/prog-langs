require './generics'
class Sudoku

  # Create a sudoku puzzle from an array of numbers _and_ charecters
  def self.parse(puzzle)
    char_map = Sudoku.char_map(Math.sqrt(puzzle.size).to_i).invert
    Sudoku.new puzzle.map { |n| char_map[n] }
  end

  # Create a sudoku puzzle from an array of numbers.
  def self.char_map(n)
    Hash[[nil, *(1..n).to_a].zip (n-9).times.reduce([' ',*(1..9).to_a, 'A']) { |r, _| r << r.last.succ }]
  end

  def initialize(puzzle)
    @size = Math.sqrt(puzzle.size).to_i
    unless (@size % Math.sqrt(@size) == 0) || @size == 2
      raise "A sudoku puzzle must have a quardratic number of cells, was #{puzzle.size}"
    end

    @pz = puzzle
  end

  # Find a solution to the puzzle if one exists.
  def solution

    return self if solved?

    while [hidden_singles, naked_singles].any?
    end

    return self if solved?


    # Brute force
    i = each_empty_cell.sort_by { |cell, n| legal_values(n).size }.first.try :last

    legal_values(i).map do |value|
      fill(i, value).solution
    end.compact.select { |n| n.solved? }.first
  end

  # write any values where a cell has one legal value.
  def naked_singles
    each_empty_cell.any? do |cell, n|
      vals = legal_values(n)
      if vals.length == 1
        @pz[n] = vals.first
        true
      end
    end
  end

  # write a value when only one cell in a group
  # can legally have that value.
  def hidden_singles
    rows_with_index.any? do |_, row|
      possible_values = row.map { |cell, n| [legal_values(n), n] }
      Hash[(1..@size).map do |value|
        [value, possible_values.select { |values, index| values.include? value }.map(&:last)]
      end].any? do |value, indexes|
        if indexes.size == 1
          @pz[indexes.first] = value
          true
        end
      end
    end
  end

  # Is the puzzle complete, and legal?
  def solved?
    first_empty_index.nil? && legal?
  end

  # Is the puzzle legal?
  def legal?
    [rows, columns, boxes].all? do |group|
      group.all? { |_, n| n.uniq == n }
    end
  end

  # All legal values for cell n.
  def legal_values n
    return [] if @pz[n]
    (((1..@size).to_a - rows[row_id n]) - columns[column_id n]) - boxes[box_id n]
  end

  # The row index of the nth cell.
  def row_id n
    n % @size
  end

  # The column index of the nth cell.
  def column_id n
    n / @size
  end

  # The box index of the nth cell.
  def box_id n
    sqrt_size = Math.sqrt(@size).to_i
    (row_id(n) / sqrt_size) + (column_id(n) / sqrt_size) * sqrt_size
  end

  # Array of the puzzles rows.
  def rows
    @pz.group_by.each_with_index { |val, n| row_id(n) }
  end

  def rows_with_index
    @pz.each_with_index.group_by { |val, n| row_id(n) }
  end

  # Array of the puzzles columns.
  def columns
    @pz.group_by.each_with_index { |val, n| column_id(n) }
  end

  def columns_with_index
    @pz.each_with_index.group_by { |val, n| column_id(n) }
  end

  # Array of the puzzles boxes.
  def boxes
    @pz.group_by.each_with_index { |val, n| box_id(n) }
  end

  def boxes_with_index
    @pz.each_with_index.group_by { |val, n| box_id(n) }
  end

  # The index of the first empty cell.
  def first_empty_index
    #_, index = @pz.lazy.each_with_index.select { |value, n| value.nil? }.first
    each_empty_cell.first.try :last
  end

  def each(&block)
    if block
      @pz.each_with_index { |*n| yield n }
    else
      @pz.each_with_index
    end
  end

  def each_empty_cell(&block)
    if block
      each { |n, i| yield n, i if n.nil? }
    else
      each.select { |n, i| n.nil? }
    end
  end

  # A copy of this sudoku puzzle with the nth cell set to value.
  def fill n, value
    pz = @pz.dup
    pz[n] = value
    Sudoku.new pz
  end

  # Print the puzzle.
  def to_s
    char_map = Sudoku.char_map(@size)
    boxed = lambda { |arr, min, maj| arr.each_slice(Math.sqrt @size).map { |n| n.join(min) }.join(maj) }

    main = @pz.each_slice(@size).map do |row|
      "|#{boxed.call(row.map { |n| char_map[n] }, ' ', '|')}|\n"
    end

    n = "|#{boxed.call(Array.new(@size, ' '), ' ', '|')}|\n"
    m = "|#{boxed.call(Array.new(@size, '-'), '-', '+')}|\n"

    "+#{boxed.call(Array.new(@size, '-'), '-', '+')}+\n" +
        boxed.call(main, n, m) +
    "+#{boxed.call(Array.new(@size, '-'), '-', '+')}+"
  end
end
