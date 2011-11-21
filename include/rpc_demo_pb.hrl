-record(sequencerequest, {
    n = erlang:error({required, n})
}).

-record(sequenceresponse, {
    sequence
}).

-record(sequenceerror, {
    message = erlang:error({required, message})
}).

