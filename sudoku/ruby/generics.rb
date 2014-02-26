# Reopening standard classes to add functionality.
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

class Object
  def try method
    send method if self.respond_to? method
  end
end
