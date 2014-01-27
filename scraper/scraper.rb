#!/bin/env ruby

require 'json'
require 'open-uri'

# Add handy blank method to strings.
class String
  def blank?
    self.nil? || self == ""
  end
end

class Numeric
  def seconds
    self;
  end
  alias :second :seconds

  def minutes
    self.seconds * 60
  end
  alias :minute :minutes

  def hours
    self.minutes * 60
  end
  alias :hour :hours

  def days
    self.hours * 24
  end
  alias :day :days

  def weeks
    self.days * 7
  end
  alias :week :weeks

  def months
    self.weeks * 4
  end
  alias :month :months

  def ago
    Time.now - self
  end
end

# Authenticated curl from github.
def github url
  open url, http_basic_authentication: [ENV['GITHUB_OAUTH_KEY'], '']
end

class User
  attr_accessor :login
  attr_reader :user_url, :events_url, :following_url, :followers_url

  def initialize login
    @login = login

    #:: Defined API endpoints
    @user_url = "https://api.github.com/users/#{login}"
    @events_url = "https://api.github.com/users/#{login}/events/public"
    @following_url = "https://api.github.com/users/#{login}/following"
    @followers_url = "https://api.github.com/users/#{login}/followers"
  end


  # The union of following, and followers
  def peers
    [:following, :followers].map do |m|
      Thread.new { self.method(m).call }
    end.map(&:value).flatten.uniq
  end

  # Array of following Users
  def following
    get_users following_url
  end

  # Array of follower Users
  def followers
    get_users followers_url
  end

  def events
    JSON.parse(github(events_url).read).map { |event| Event.new event }
  end

  def to_s
    login
  end

  # Users are uniquely identified by their login.
  def hash
    login.hash
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

class Event
  attr_accessor :type, :repo_name, :created_at

  def initialize hash
    @type = hash['type']
    @repo_name = hash['repo']['name']
    @created_at = Time.parse(hash['created_at'])
  end

  def to_s
    "#{created_at} - #{type}: #{repo_name}"
  end
end


user = User.new 'everett1992'

puts user.login

puts user.events.select { |event| event.created_at > 1.day.ago }
