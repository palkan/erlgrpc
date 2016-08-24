$LOAD_PATH.unshift(File.join(File.dirname(__FILE__), '..', 'rubyrpc'))

require 'logger'
require 'grpc'
require 'rpc_services'

# Set GRPC logger
module GRPC
  def self.logger
    @logger ||= ::Logger.new(STDOUT)
  end
end

module Testmath
  class Handler < Testmath::Calculator::Service # :nodoc:
    def add(request, _unused_call)
      do_sleep
      reply = Testmath::OperationReply.new(result: request.a + request.b)
      GRPC.logger.info "Sending reply for Add: #{reply.result}"
      reply
    end

    def multiply(request, _unused_call)
      do_sleep
      reply = Testmath::OperationReply.new(result: request.a * request.b)
      GRPC.logger.info "Sending reply for Multiply: #{reply.result}"
      reply
    end

    private

    def do_sleep
      sleep(rand(5) / 10.0)
    end
  end
  # Wrapper over GRPC server
  module Server
    def self.start
      host = ENV['HOST'] || 'localhost:50051'
      s = GRPC::RpcServer.new
      s.add_http2_port(host, :this_port_is_insecure)
      s.handle(Handler)
      GRPC.logger.info "RPC server is listening on #{host}"
      s.run_till_terminated
    end
  end
end

Testmath::Server.start
