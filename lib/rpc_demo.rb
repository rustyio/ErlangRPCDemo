#!/usr/bin/env ruby
require 'rubygems'
require 'net/http'
require 'json'
require 'bertrpc'
require 'beefcake'
require 'socket'

def webmachine_test()
  url = 'http://localhost:8001/sequence/5'
  resp = Net::HTTP.get_response(URI.parse(url))
  sequence = JSON.parse(resp.body)
  puts "Webmachine: #{sequence}"
end

def spooky_test()
  url = 'http://localhost:8002/sequence/5'
  resp = Net::HTTP.get_response(URI.parse(url))
  sequence = JSON.parse(resp.body)
  puts "Spooky: #{sequence}"
end

def bert_test()
  svc = BERTRPC::Service.new('localhost', 9999)
  sequence = svc.call.ernie_sequence.sequence(5)[1]
  puts "BERT: #{sequence}"
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

def pb_test()
  Socket.tcp("localhost", 8003) do |socket|
    req = SequenceRequest.new(:n => 5)
    write_protobuff(socket, SequenceRequest::MsgCode, req)
    resp = read_protobuf(socket)
    puts "PB: #{resp.sequence}"
  end
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

webmachine_test()
spooky_test()
bert_test()
pb_test()
