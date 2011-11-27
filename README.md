# Erlang RPC Demo Code

This project was created for an Erlang DC 2011 talk presented by Rusty Klophaus (http://erlangdc.com/speakers/rusty_klophaus).

It contains sample code demonstrating cross-language RPC where Erlang
is the server. In other words, it demonstrates how to call into an
Erlang service with an Erlang or Ruby client.

+ Expose an Erlang service over REST (via Webmachine and Spooky), BERT-RPC, and Protocol Buffers.
+ Connect an Erlang client to the Erlang service over REST, BERT-RPC, or Protocol Buffers.
+ Connect a Ruby client to the Erlang service over REST, BERT-RPC, or Protocol Buffers.

The service generates a list of numbers. For example:

```erlang
sequence(5) -> {ok, ["1", "2", "3", "4", "5"]}.
```

The service is exposed on the following endpoints:

+ REST via Webmachine (http://localhost:8001/sequence/5)
+ REST via Spooky (http://localhost:8002/sequence/5)
+ Ernie (port 9999, `{call, ernie_sequence, sequence, [5]}`)
+ Protocol Buffers (port 8003)

This project also contains a benchmarking harness for Basho Bench that exercises the Erlang service via the four provided endpoints.

# Files

### Misc.

+ `src/sequence.erl` - Contains the "service" logic. Simply generates a list of numbers.
+ `lib/rpc_demo.rb` - Ruby clients.
+ `src/rpc_demo.erl` - Helper functions and Erlang clients.

### REST Interface

+ `src/spooky_sequence.erl` - Callback module for REST interface. (Spooky)
+ `src/webmachine_sequence.erl` - Callback module for REST interface. (Webmachine)

### BERT-RPC Interface

+ `src/ernie_sequence.erl` - Callback module for BERT-RPC interface (Ernie server).

### Protocol Buffers

+ `src/protobuff_server_listener.erl` - Waits for incoming PB connections
+ `src/protobuff_server_sup.erl` - Supervise PB connections.
+ `src/protobuff_server.erl` - Serve a PB connection.
+ `src/rpc_demo.proto` - Protocol Buffers definition file.
+ `src/gen_nb_server.erl` - Non-blocking server. Used by `protobuff_server_listener`.

### Benchmarking

+ `src/rpc_demo_basho_bench_driver.erl` - Driver for Basho Bench performance test.
+ `priv/rpc_demo.config` - Basho Bench config file.

# Benchmarking

### Test 1 - Encoding and Decoding

Objective: Demonstrate which encoding scheme (JSON, BERT, or PB) is the fastest.

### Test 2 - Single Hop, Small Payload

Objective: Demonstrate which interface is the fastest with a small payload (`sequence(5)`, 44 bytes) over a single hop. (Ping times <1 ms.)

### Test 3 - Single Hop, Large Payload

Objective: Demonstrate which interface is the fastest with a large payload (`sequence(20000)`, 190kb) over a single hop. (Ping times <1 ms.)

### Test 4 - Multiple Hops, Small Payload

Objective: Demonstrate which interface is the fastest with a small payload (`sequence(5)`, 44 bytes) over multiple hops. (Ping times ~50 ms.)

### Test 5 - Multiple Hops, Large Payload

Objective: Demonstrate which interface is the fastest with a large payload (`sequence(20000)`, 190kb) over multiple hops. (Ping times ~50 ms.)



