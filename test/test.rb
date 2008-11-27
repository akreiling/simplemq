#!/usr/bin/env ruby
#
require 'rubygems'
require 'benchmark'
require 'stomp'

out_count = (ARGV.shift || 1).to_i
in_count = 0
client = Stomp::Client.open('test','test','localhost')
client.subscribe('/queue/screenshots') do |msg|
  puts msg.to_s
  in_count += 1
end
t = Benchmark.realtime do
  out_count.times { client.send('/queue/screenshots', 'This is a test') }
end
# sleep 5
puts "sent %d frames in %0.2fs (%0.2f frames/s)" % [out_count, t, out_count / t]
puts "got %d frames" % [in_count]
