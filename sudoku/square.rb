class Square
  attr_reader :x, :y
  attr_accessor :val

  def initialize(x, y, val)
    @x, @y, @val = x, y, val
  end
  
  def filled?
    !val.nil?
  end

  def pos
    [@x, @y]
  end

  def to_s
    "#{@x}, #{@y}: #{@val}"
  end
  alias :inspect :to_s
end
