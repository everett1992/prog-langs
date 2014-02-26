require 'benchmark'
require './generics'
require './sudoku'

# Test puzzle, I don't feel like writing an input
# function yet so it's hardcoded.
pzls = [
  [
    7,   9,   nil, nil, nil, nil, 3,   nil, nil,
    nil, nil, nil, nil, nil, 6,   9,   nil, nil,
    8,   nil, nil, nil, 3,   nil, nil, 7,   6  ,
    nil, nil, nil, nil, nil, 5,   nil, nil, 2  ,
    nil, nil, 5,   4,   1,   8,   7,   nil, nil,
    4,   nil, nil, 7,   nil, nil, nil, nil, nil,
    6,   1,   nil, nil, 9,   nil, nil, nil, 8  ,
    nil, nil, 2,   3,   nil, nil, nil, nil, nil,
    nil, nil, 9,   nil, nil, nil, nil, 5,   4  
  ],
  [
    nil, 1,   nil, nil, nil, 3,   nil, 5,   nil,
    9,   nil, nil, nil, nil, 2,   nil, nil, 1  ,
    nil, nil, 3,   9,   nil, 6,   nil, nil, 8  ,
    nil, nil, 5,   nil, 2,   nil, 9,   1,   nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, 8,   2,   nil, 9,   nil, 5,   nil, nil,
    1,   nil, nil, 3,   nil, 7,   8,   nil, nil,
    8,   nil, nil, 2,   nil, nil, nil, nil, 4  ,
    nil, 6,   nil, 8,   nil, nil, nil, 9,   nil
  ],
  [
    8,   nil, nil, nil, nil, 4,   2,   nil, nil,
    3,   nil, nil, nil, 5,   nil, nil, 6,   nil,
    5,   nil, nil, nil, 3,   2,   nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, 4,   2  ,
    nil, 2,   1,   nil, nil, nil, 3,   8,   nil,
    4,   7,   nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, 3,   9,   nil, nil, nil, 6  ,
    nil, 8,   nil, nil, 7,   nil, nil, nil, 5  ,
    nil, nil, 6,   5,   nil, nil, nil, nil, 9  
  ],
  [
    8,   5,   nil, nil, nil, 2,   4,   nil, nil,
    7,   2,   nil, nil, nil, nil, nil, nil, 9  ,
    nil, nil, 4,   nil, nil, nil, nil, nil, nil,
    nil, nil, nil, 1,   nil, 7,   nil, nil, 2  ,
    3,   nil, 5,   nil, nil, nil, 9,   nil, nil,
    nil, 4,   nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, 8,   nil, nil, 7,   nil,
    nil, 1,   7,   nil, nil, nil, nil, nil, nil,
    nil, nil, nil, nil, 3,   6,   nil, 4,   nil
  ],
  [
    nil, nil, 5, 3, nil, nil, nil, nil, nil,
    8, nil, nil, nil, nil, nil, nil, 2, nil,
    nil, 7, nil, nil, 1, nil, 5, nil, nil  ,
    4, nil, nil, nil, nil, 5, 3, nil, nil  ,
    nil, 1, nil, nil, 7, nil, nil, nil, 6  ,
    nil, nil, 3, 2, nil, nil, nil, 8, nil  ,
    nil, 6, nil, 5, nil, nil, nil, nil, 9  ,
    nil, nil, 4, nil, nil, nil, nil, 3, nil,
    nil, nil, nil, nil, nil, 9, 7, nil, nil,
  ],

  [
    nil, nil, 'C', nil, 9,   'B', nil, nil, nil, nil, nil, nil, nil, nil, nil, nil,
    nil, 2,   nil, nil, nil, 'D', 'G', nil, 7,   'B', 6,   nil, nil, 1,   nil, 'C',
    nil, 1,   nil, 3,   nil, 'A', nil, 2,   'G', nil, nil, nil, nil, nil, nil, nil,
    nil, nil, nil, 'D', 1,   3,   'F', nil, 'A', nil, nil, nil, nil, nil, nil, nil,
    5,   6,   nil, nil, 3,   nil, nil, 9,   'F', nil, nil, nil, 4,   nil, nil, nil,
    'B', nil, 9,   4,   nil, nil, 7,   'C', nil, 2,   nil, nil, nil, 'A', 1,   nil,
    'D', nil, nil, nil, 'G', nil, 1,   nil, 5,   nil, nil, 8,   nil, nil, nil, 'E',
    'E', nil, 'F', 2,   nil, nil, 5,   'A', nil, nil, nil, 'G', 'B', nil, nil, nil,
    nil, nil, nil, 8,   'B', nil, nil, nil, 3,   4,   nil, nil, 'A', 'C', nil, 'G',
    'G', nil, nil, nil, 'E', nil, nil, 5,   nil, 9,   nil, 'A', nil, nil, nil, 7,
    nil, 4,   3,   nil, nil, nil, 9,   nil, 'E', 'D', nil, nil, 'F', 2,   nil, 'B',
    nil, nil, nil, 'E', nil, nil, nil, 3,   8,   nil, nil, 'F', nil, nil, 6,   5,
    nil, nil, nil, nil, nil, nil, nil, 'G', nil, 5,   2,   'E', 6,   nil, nil, nil,
    nil, nil, nil, nil, nil, nil, nil, 'F', 4,   nil, 'G', nil, 'D', nil, 2,   nil,
    'F', nil, 2,   nil, nil, 7,   'A', 1,   nil, 8,   3,   nil, nil, nil, 'C', nil,
    nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, 'D', 6,   nil, 8,   nil, nil,
  ]
]




Benchmark.bm do |bm|
  pzls.take(6).each_with_index do |ss, n|
    bm.report do
      puts
      puts "-- puzzle #{n}"
      puts sudoku = Sudoku.parse(ss)
      puts sudoku.solution
    end
  end
end
