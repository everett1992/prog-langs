class Sudoku
  def initialize(puzzle)
    @size = Math.sqrt(puzzle.size).to_i
    unless (@size % Math.sqrt(@size) == 0) || @size == 2
      raise "A sudoku puzzle must have a quardratic number of cells, was #{puzzle.size}"
    end

    @pz = puzzle
  end

  def solve
    i = first_empty_index

    return self if solved?

    legal_values(i).map do |value|
      fill(i, value).solve
    end.compact.select { |n| n.solved? }.first
  end

  def solved?
    first_empty_index.nil? && legal?
  end

  def legal?
    [rows, columns, boxes].all? do |group|
      group.all? { |_, n| n.uniq == n }
    end
  end

  def legal_values i
    (1..@size).to_a - rows[row_id i] - columns[column_id i] - boxes[box_id i]
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

  # Array of the puzzles columns.
  def columns
    @pz.group_by.each_with_index { |val, n| column_id(n) }
  end

  # Array of the puzzles boxes.
  def boxes
    @pz.group_by.each_with_index { |val, n| box_id(n) }
  end

  # The index of the first empty cell.
  def first_empty_index
    _, index = @pz.lazy.each_with_index.select { |value, n| value.nil? }.first
    return index
  end

  # A copy of this sudoku puzzle with the nth cell set to value.
  def fill n, value
    pz = @pz.dup
    pz[n] = value
    Sudoku.new pz
  end

  # Print the puzzle.
  def to_s
    boxed = lambda { |arr, min, maj| arr.each_slice(Math.sqrt @size).map { |n| n.join(min) }.join(maj) }

    main = @pz.each_slice(@size).map do |row|
      "┃#{boxed.call(row.map { |n| n || ' ' }, '│', '┃')}┃\n"
    end

    n = "┠#{boxed.call(Array.new(@size, '─'), '┼', '╂')}┨\n"
    m = "┣#{boxed.call(Array.new(@size, '━'), '┿', '╋')}┫\n"

    "┏#{boxed.call(Array.new(@size, '━'), '┯', '┳')}┓\n" +
        boxed.call(main, n, m) +
    "┗#{boxed.call(Array.new(@size, '━'), '┷', '┻')}┛"
  end
end

ss = [
  7,   9,   nil, nil, nil, nil, 3,   nil, nil,
  nil, nil, nil, nil, nil, 6,   9,   nil, nil,
  8,   nil, nil, nil, 3,   nil, nil, 7,   6  ,
  nil, nil, nil, nil, nil, 5,   nil, nil, 2  ,
  nil, nil, 5,   4,   1,   8,   7,   nil, nil,
  4,   nil, nil, 7,   nil, nil, nil, nil, nil,
  6,   1,   nil, nil, 9,   nil, nil, nil, 8  ,
  nil, nil, 2,   3,   nil, nil, nil, nil, nil,
  nil, nil, 9,   nil, nil, nil, nil, 5,   4  ,
]


sudoku = Sudoku.new(ss)
puts sudoku

puts sudoku.solve
