#!/bin/env ruby

require 'nokogiri'
require 'open-uri'

# Add handy blank method to strings.
class String
  def blank?
    self.nil? || self == ""
  end
end

class User
  attr_accessor :id, :name

  def initialize id, name=nil
    @id, @name = id, name
  end

  # If a user doesn't have a name use the id instead
  def name
    @name || @id
  end

  # Returns the address of a users following page.
  def activity_url
    "https://github.com/#{id}?tab=activity"
  end

  # Returns the address of a users following page.
  def following_url
    "https://github.com/#{name}/following"
  end

  # Returns the address of a users followers page.
  def followers_url
    "https://github.com/#{name}/followers"
  end

  def combined_followees
    following | followers
  end

  def following
    get_users following_url
  end

  def followers
    get_users followers_url
  end

  def to_s
    "#{name}: #{id}"
  end

  def hash
    self.id.hash
  end

  def eql? other
    other.id == self.id
  end

  private

  def get_users url
    Nokogiri::HTML(open(url)).css('li.follow-list-item').map do |elem|
      # Get the account id from the image link.
      id = elem.css('> a').first[:href][1..-1]
      # Get the account name from the image alt text (github doesn't use great html markup)
      name = elem.css('> a > img').first[:alt]
      name = nil if name.blank?

      User.new id, name
    end
  end
end


user = User.new 'everett1992'

puts '-- combined'
puts user.combined_followees
