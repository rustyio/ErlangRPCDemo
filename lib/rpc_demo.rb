#!/usr/bin/env ruby
require 'rubygems'
require 'net/http'
require 'json'
require 'bertrpc'
require 'beefcake'
require 'socket'
require 'benchmark'

def webmachine_sequence(n)
  url = "http://localhost:8001/sequence/#{n}"
  resp = Net::HTTP.get_response(URI.parse(url))
  sequence = JSON.parse(resp.body)
  # puts "Webmachine: #{sequence}"
end

def spooky_sequence(n)
  url = "http://localhost:8002/sequence/#{n}"
  resp = Net::HTTP.get_response(URI.parse(url))
  sequence = JSON.parse(resp.body)
  # puts "Spooky: #{sequence}"
end

def bert_sequence(n)
  svc = BERTRPC::Service.new('localhost', 9999)
  sequence = svc.call.ernie_sequence.sequence(n)[1]
  # puts "BERT: #{sequence}"
end

class SequenceRequest
  include Beefcake::Message
  MsgCode = 1
  required :n, :int32, 1
end

class SequenceResponse
  include Beefcake::Message
  MsgCode = 2
  repeated :sequence, :string, 1
end

class SequenceError
  include Beefcake::Message
  MsgCode = 3
  repeated :message, :string, 1
end

def pb_sequence(n)
  Socket.tcp("localhost", 8003) do |socket|
    req = SequenceRequest.new(:n => n)
    write_protobuff(socket, SequenceRequest::MsgCode, req)
    resp = read_protobuf(socket)
    # puts "PB: #{resp.sequence}"
  end
end

def pb_reuse_sequence(n)
  socket ||= Socket.tcp("localhost", 8003)
  req = SequenceRequest.new(:n => n)
  write_protobuff(socket, SequenceRequest::MsgCode, req)
  resp = read_protobuf(socket)
  # puts "PB: #{resp.sequence}"
end

def write_protobuff(socket, message_code, message)
  encoded = message.encode
  header = [encoded.length + 1, message_code].pack("NC")
  socket.write(header + encoded)
end

def read_protobuf(socket)
  header = socket.read(5)
  size, code = header.unpack("NC")
  message = socket.read(size-1)
  case code
  when SequenceResponse::MsgCode
    SequenceResponse.decode(message)
  when SequenceError::MsgCode
    SequenceError.decode(message)
  end
end

threads = ARGV[0].to_i
calls = ARGV[1].to_i
iterations = ARGV[2].to_i
length = ARGV[3].to_i

# puts "Running #{threads} threads * #{calls} times * #{iterations} iterations with length #{length}"

puts "Webmachine, Spooky, BERT, PB, PB-Reuse"

threads.times do |i|
  Thread.new do
    srand()

    calls.times do
      sleep(rand(1000) / 1000.0)

      t1 = Benchmark::measure do
        iterations.times do
          webmachine_sequence(length)
        end
      end

      sleep(rand(100) / 1000.0)

      t2 = Benchmark::measure do
        iterations.times do
          spooky_sequence(length)
        end
      end

      sleep(rand(100) / 1000.0)

      t3 = Benchmark::measure do
        iterations.times do
          bert_sequence(length)
        end
      end

      sleep(rand(100) / 1000.0)

      t4 = Benchmark::measure do
        iterations.times do
          pb_sequence(length)
        end
      end

      sleep(rand(100) / 1000.0)

      t5 = Benchmark::measure do
        iterations.times do
          pb_reuse_sequence(length)
        end
      end

      puts "#{t1.real * 1000}, #{t2.real * 1000}, #{t3.real * 1000}, #{t4.real * 1000}, #{t5.real * 1000}"
    end

    $finished += 1
  end
end

# Wait for Control-C
$finished = 0
while ($finished < threads)
  sleep 1
end


