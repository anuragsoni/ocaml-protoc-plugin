module type Message = sig
  type t

  val from_proto : Reader.t -> t
  val to_proto : t -> Writer.t
end

module type Rpc = sig
  val name : string

  type request
  type response

  module Request : Message with type t = request
  module Response : Message with type t = response
end

let make_client_functions
  (type req rep)
  (module Rpc : Rpc with type request = req and type response = rep)
  =
  Rpc.Request.to_proto, Rpc.Response.from_proto
;;

let make_service_functions
  (type req rep)
  (module Rpc : Rpc with type request = req and type response = rep)
  =
  Rpc.Request.from_proto, Rpc.Response.to_proto
;;
