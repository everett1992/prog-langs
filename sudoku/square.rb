class Square
  attr_reader :x, :y, :val

  def initialize(x, y, val)
    @x, @y, @val = x, y, val
  end

  def pos
    [@x, @y]
  end

  def to_s
    "#{@x}, #{@y}: #{@val}"
  end
end
