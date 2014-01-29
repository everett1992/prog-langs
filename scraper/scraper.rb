#!/bin/env ruby

require 'benchmark'
require 'json'
require 'open-uri'

# Add handy blank method to Strings.
class String
  def blank?
    self.nil? || self == ""
  end
end

# Add time units to Numerics
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
    @peers ||= [:following, :followers].map do |m|
      Thread.new { self.method(m).call }
    end.map(&:value).flatten.uniq
  end

  # Array of following Users
  def following
    @following ||= get_users following_url
  end

  # Array of follower Users
  def followers
    @followers ||= get_users followers_url
  end

  def days_events
    events.select { |event| event.created_at > 1.day.ago }
  end

  def weeks_events
    events.select { |event| event.created_at > 1.week.ago }
  end

  def months_events
    events.select { |event| event.created_at > 1.month.ago }
  end

  def events
    @events ||= JSON.parse(github(events_url).read).map { |event| Event.new event }
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
  attr_accessor :type, :repo_name, :created_at, :ref_type

  def initialize hash
    @type = hash['type']
    @repo_name = hash['repo']['name']
    @ref_type = hash['payload']['ref_type']
    @created_at = Time.parse(hash['created_at'])
  end

  def to_s
    "#{created_at} - #{type}: #{repo_name}"
  end
end

def summarize_events events
  report = Array.new
  events = events.group_by(&:type)

  events.each_key do |type|
    # Count WatchEvents then list the watched repos.
    case type
      when 'WatchEvent'
        report << "Watched #{events['WatchEvent'].count} repositories"
      when 'PushEvent'
        events[type].group_by(&:repo_name).each do |name, repo_events|
          report << "#{repo_events.count} #{repo_events.count == 1 ? "push" : "pushes"} to #{name}"
        end
      else
        report << "Unknown event '#{type}'"
    end
    # Count PushEvents to each repository
    #events['PushEvent'].each do |event|
    #end

    # Count CreateEvents
    #events['CreateEvent'].each do |event|
    #end
  end

  return report
end

def main username
  user = User.new username
  puts user.login

  puts "Fetching peers activity..."

  # Events are retrieved from the network, and cached.
  # Do all of the event requests at once, in threads, then
  # handle them sequentially
  user.peers.map do |peer|
    Thread.new { peer.events}
  end.each(&:join)

  user.peers.each do |peer|
    if peer.weeks_events.length > 0
      puts "-- #{peer.login}"
      puts summarize_events peer.weeks_events
    end
  end


end

main 'everett1992'
