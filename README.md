# Erlang RPC Demo Code

This project demonstrates how to use REST, Ernie, and Protocol Buffers for cross-language RPC, allowing Ruby to call into Erlang.

This code will be used for my Erlang DC 2011 talk (http://erlangdc.com/speakers/rusty_klophaus)

It sets up four endpoints:

+ REST via Webmachine (http://localhost:8001/sequence/5)
+ REST via Spooky (http://localhost:8001/sequence/5)
+ Ernie (port 9999, `{call, ernie_sequence, sequence, [5]}`)
+ Protocol Buffers

This project also contains benchmarking scripts, allowing us to compare the difference in overhead between the four endpoints listed above.

Notes:

+ For HTTP benchmark, try with keepalive off, then on
+ For PB benchmark, reuse connection