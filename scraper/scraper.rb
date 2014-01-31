#!/bin/env ruby

require 'optparse'
require 'json'
require 'open-uri'

# Add time units to Numerics
class Numeric

  def seconds
    self
  end
  alias :second :seconds

  def minutes
    self * 60.seconds
  end
  alias :minute :minutes

  def hours
    self * 60.minutes
  end
  alias :hour :hours

  def days
    self * 24.hours
  end
  alias :day :days

  def weeks
    self * 7.days
  end
  alias :week :weeks

  def months
    self * 4.weeks
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

  def weeks_events
    events.select { |event| event.created_at > 1.week.ago }
  end

  def events
    @events ||= JSON.parse(github(events_url).read)
      .map { |event| Event.new event }
  end

  # time must be one of [:seconds, :minutes, :hours, :days, :weeks, :months]
  def summarize_events(time=:weeks)
    report = Array.new
    events = @events.select { |e| e.created_at > 1.method(time).call.ago }
      .group_by(&:type)

    events.each_key do |type|
      # Count WatchEvents then list the watched repos.
      case type
        when 'WatchEvent'
          report << "Watched #{events['WatchEvent'].count} repositories"
        when 'PushEvent'
          events[type].group_by(&:repo_name).each do |name, repo_events|
            count = repo_events.count
            pushes = count == 1 ? 'push' : 'pushes'
            report << "#{count} #{pushes} to #{name}"
          end
        when 'CreateEvent'
          events[type].group_by(&:ref_type).each do |ref_type, type_events|
            type_events.each do |event|
              case ref_type
              when 'repository'
                report << "Created repository #{event.repo_name}"
              when 'branch'
                report << "Created branch #{event.branch} in #{event.repo_name}"
              else
                report << "Created #{ref_type}"
              end
            end
          end
        else
          report << "Unknown event '#{type}'"
      end
    end
    return report
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
  attr_accessor :type, :repo_name, :created_at, :ref_type, :branch

  def initialize hash
    @type = hash['type']
    @repo_name = hash['repo']['name']
    @ref_type = hash['payload']['ref_type']
    @branch = hash['payload']['master_branch']
    @created_at = Time.parse(hash['created_at'])
  end

  def to_s
    "#{created_at} - #{type}: #{repo_name}"
  end
end

def print_user_events username, time_frame=:weeks

  user = User.new username
  puts user.login

  puts "Fetching #{user.login}'s peers activity in the past #{time_frame}..."

  # Events are retrieved from the network, and cached.
  # Do all of the event requests at once, in threads, then
  # handle them sequentially
  user.peers.map do |peer|
    Thread.new { peer.events}
  end.each(&:join)

  user.peers.each do |peer|
    if peer.weeks_events.length > 0
      puts "-- #{peer.login}"
      puts peer.summarize_events time_frame
      puts
    end
  end
end

def main
  # Default options
  o = OpenStruct.new(time: :week, username: nil)

  opt_parser = OptionParser.new do |opts|
    opts.banner = "Usage: #{$0} [options] username"

    times = [:hour, :day, :week, :month]
    message = ["Show messages from the last TIME (defaults to #{o.time})",
        "Avaliable options are (#{times.join(', ')})"]

    opts.on('-t', '--time=TIME', times, *message) do |time|
      o.time = time
    end

    opts.on_tail('-h', '--help', 'Show this message') do
      puts opts
      exit
    end
  end

  begin
    opt_parser.parse! ARGV
  rescue OptionParser::ParseError => ex
    $stderr.puts "#{$0}: #{ex.message}"
    $stderr.puts "Try '#{$0} --help' for more information"
    exit
  end

  o.username = ARGV.first
  if o.username.nil?
    $stderr.puts "#{$0}: missing username operand"
    $stderr.puts "Try '#{$0} --help' for more information"
    exit
  end

  print_user_events o.username, o.time
end

main # call main
