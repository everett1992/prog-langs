#!/bin/env ruby

require 'json'
require 'open-uri'

# Add handy blank method to strings.
class String
  def blank?
    self.nil? || self == ""
  end
end

# Authenticated interaction with github.
def github url
  open url, http_basic_authentication: [ENV['GITHUB_OAUTH_KEY'], '']
end

class User
  attr_accessor :login, :name

  def initialize login
    @login = login
  end

  def user_url
    "https://api.github.com/users/#{login}"
  end

  def events_url
    "https://api.github.com/users/#{login}/events/public"
  end

  def following_url
    "https://api.github.com/users/#{login}/following"
  end

  def followers_url
    "https://api.github.com/users/#{login}/followers"
  end

  def peers
    following | followers
  end

  def following
    get_users following_url
  end

  def followers
    get_users followers_url
  end

  def to_s
    login
  end

  def hash
    self.login.hash
  end

  def eql? other
    other.login == self.login
  end

  private

  def get_users url
    begin
      JSON.parse(github(url).read).map do |user|
        User.new user['login']
      end
    rescue OpenURI::HTTPError => ex
      puts "Error fetching #{url} : #{ex.message}"
      puts ex.backtrace
    end
  end
end


user = User.new 'everett1992'

puts user.name

puts '-- peers'
puts user.peers
