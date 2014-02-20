class Array
  # An array is square if it is at least 2d, and each sub array is
  # the same length as the number of sub arrays.
  def square?
    (self.map(&:length) << self.length).uniq.length == 1
  end
end

class Numeric
  # A number is square if it's square root is an integer
  def square?
    self % Math.sqrt(self) == 0
  end
end

class Sudoku
  attr_reader :puzzle
  def initialize puzzle
    raise "Sudoku must be a square 2d array" unless puzzle.square?
    raise "Sudoku size must be a square number, was #{puzzle.size}" unless puzzle.size.square?
    @puzzle = puzzle
  end

  def row(x, y)
    @puzzle.dup[x]
  end

  def column(x, y)
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

  def to_s
    box = lambda { |arr, min, maj| arr.each_slice(Math.sqrt @puzzle.size).map { |n| n.join(min) }.join(maj) }

    main = @puzzle.map do |row|
      "┃#{box.call(row.map { |n| n || ' ' }, '│', '┃')}┃\n"
    end

    n = "┠#{box.call(Array.new(@puzzle.size, '─'), '┼', '╂')}┨\n"
    m = "┣#{box.call(Array.new(@puzzle.size, '━'), '┿', '╋')}┫\n"

    "┏#{box.call(Array.new(@puzzle.size, '━'), '┯', '┳')}┓\n" +
        box.call(main, n, m) +
    "┗#{box.call(Array.new(@puzzle.size, '━'), '┷', '┻')}┛"
  end
end

# Test puzzle, I don't feel like writing an input
# function yet so it's hardcoded.
ss = [
  [7,   9,   nil, nil, nil, nil, 3,   nil, nil],
  [nil, nil, nil, nil, nil, 6,   9,   nil, nil],
  [8,   nil, nil, nil, 3,   nil, nil, 7,   6  ],
  [nil, nil, nil, nil, nil, 5,   nil, nil, 2  ],
  [nil, nil, 5,   4,   1,   8,   7,   nil, nil],
  [4,   nil, nil, 7,   nil, nil, nil, nil, nil],
  [6,   1,   nil, nil, 9,   nil, nil, nil, 8  ],
  [nil, nil, 2,   3,   nil, nil, nil, nil, nil],
  [nil, nil, 9,   nil, nil, nil, nil, 5,   4  ],
]


sudoku = Sudoku.new(ss)
puts sudoku

#p sudoku.row(0,0)
#p sudoku.column(0,0)
#p sudoku.box(0,0)
