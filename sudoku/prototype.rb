require './generics'
require './sudoku'

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
while sudoku.next_move
end
puts sudoku
