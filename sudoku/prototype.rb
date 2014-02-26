require './generics'
require './sudoku'

# Test puzzle, I don't feel like writing an input
# function yet so it's hardcoded.
pzls = [
  [
    [7,   9,   nil, nil, nil, nil, 3,   nil, nil],
    [nil, nil, nil, nil, nil, 6,   9,   nil, nil],
    [8,   nil, nil, nil, 3,   nil, nil, 7,   6  ],
    [nil, nil, nil, nil, nil, 5,   nil, nil, 2  ],
    [nil, nil, 5,   4,   1,   8,   7,   nil, nil],
    [4,   nil, nil, 7,   nil, nil, nil, nil, nil],
    [6,   1,   nil, nil, 9,   nil, nil, nil, 8  ],
    [nil, nil, 2,   3,   nil, nil, nil, nil, nil],
    [nil, nil, 9,   nil, nil, nil, nil, 5,   4  ]
  ],
  [
    [nil, 1,   nil, nil, nil, 3,   nil, 5,   nil],
    [9,   nil, nil, nil, nil, 2,   nil, nil, 1  ],
    [nil, nil, 3,   9,   nil, 6,   nil, nil, 8  ],
    [nil, nil, 5,   nil, 2,   nil, 9,   1,   nil],
    [nil, nil, nil, nil, nil, nil, nil, nil, nil],
    [nil, 8,   2,   nil, 9,   nil, 5,   nil, nil],
    [1,   nil, nil, 3,   nil, 7,   8,   nil, nil],
    [8,   nil, nil, 2,   nil, nil, nil, nil, 4  ],
    [nil, 6,   nil, 8,   nil, nil, nil, 9,   nil]
  ],
  [
    [8,   nil, nil, nil, nil, 4,   2,   nil, nil],
    [3,   nil, nil, nil, 5,   nil, nil, 6,   nil],
    [5,   nil, nil, nil, 3,   2,   nil, nil, nil],
    [nil, nil, nil, nil, nil, nil, nil, 4,   2, ],
    [nil, 2,   1,   nil, nil, nil, 3,   8,   nil],
    [4,   7,   nil, nil, nil, nil, nil, nil, nil],
    [nil, nil, nil, 3,   9,   nil, nil, nil, 6  ],
    [nil, 8,   nil, nil, 7,   nil, nil, nil, 5  ],
    [nil, nil, 6,   5,   nil, nil, nil, nil, 9  ]
  ],
  [
    [8,   5,   nil, nil, nil, 2,   4,   nil, nil],
    [7,   2,   nil, nil, nil, nil, nil, nil, 9  ],
    [nil, nil, 4,   nil, nil, nil, nil, nil, nil],
    [nil, nil, nil, 1,   nil, 7,   nil, nil, 2  ],
    [3,   nil, 5,   nil, nil, nil, 9,   nil, nil],
    [nil, 4,   nil, nil, nil, nil, nil, nil, nil],
    [nil, nil, nil, nil, 8,   nil, nil, 7,   nil],
    [nil, 1,   7,   nil, nil, nil, nil, nil, nil],
    [nil, nil, nil, nil, 3,   6,   nil, 4,   nil]
  ],
  [
    [nil, nil, 5, 3, nil, nil, nil, nil, nil],
    [8, nil, nil, nil, nil, nil, nil, 2, nil],
    [nil, 7, nil, nil, 1, nil, 5, nil, nil  ],
    [4, nil, nil, nil, nil, 5, 3, nil, nil  ],
    [nil, 1, nil, nil, 7, nil, nil, nil, 6  ],
    [nil, nil, 3, 2, nil, nil, nil, 8, nil  ],
    [nil, 6, nil, 5, nil, nil, nil, nil, 9  ],
    [nil, nil, 4, nil, nil, nil, nil, 3, nil],
    [nil, nil, nil, nil, nil, 9, 7, nil, nil],
  ]
]


pzls.each do |pz|
  puts "-- puzzle"
  sudoku = Sudoku.new(pz)
  puts sudoku
  sudoku.solve
  puts sudoku
end
