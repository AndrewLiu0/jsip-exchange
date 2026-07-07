open! Core
module Rpc = Async_rpc_kernel.Rpc

let stats_rpc =
  Rpc.Pipe_rpc.create
    ~name:"exchange-stats"
    ~version:1
    ~bin_query:Unit.bin_t
    ~bin_response:Snapshot.bin_t
    ~bin_error:Error.bin_t
    ()
;;
