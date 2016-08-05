package main

import (
  "flag"
  "log"
  "math/rand"
  "sync"
  "time"

  pb "./protos"
  pool "./pool"

  "golang.org/x/net/context"
  "google.golang.org/grpc"
)

type Remote struct {
  pool pool.Pool
}

var rpc = Remote {}

func (rpc *Remote) Init(host string) {
  factory := func() (*grpc.ClientConn, error) { 
    return grpc.Dial(host, grpc.WithInsecure())
  }

  p, err := pool.NewChannelPool(10, 50, factory)
    
  if err != nil {
    log.Fatalf("failed to create pool: %v", err)
  }

  rpc.pool = p
}

func (rpc *Remote) GetConn() pool.PoolConn {
  pc, err := rpc.pool.Get()

  if err != nil {
    log.Fatalf("failed to retrieve connection: %v", err)
  }

  return pc;
}

func (rpc *Remote) Add(a int32, b int32) int32 {
  conn := rpc.GetConn()
  defer conn.Close()
  client := pb.NewCalculatorClient(conn.Conn)

  op := func() (*pb.OperationReply, error) {
    return client.Add(context.Background(), &pb.OperationRequest{A: a, B: b})
  }
  
  response, err := retry(10, op)

  if err != nil {
      log.Println("RPC Error: %v", err)
      return 0
  }

  return response.Result
}

func (rpc *Remote) Multiply(a int32, b int32) int32 {
  conn := rpc.GetConn()
  client := pb.NewCalculatorClient(conn.Conn)

  op := func() (*pb.OperationReply, error) {
    return client.Multiply(context.Background(), &pb.OperationRequest{A: a, B: b})
  }

  response, err := retry(10, op)

  if err != nil {
    conn.Conn.Close()
    log.Println("RPC Error: %v", err)
    return 0
  }

  conn.Close()
  return response.Result
}

func (rpc *Remote) Close() {
  rpc.pool.Close()
}

func IsExhausted(err error) bool {
  return grpc.Code(err) == 8
}

func retry(attempts int, callback func() (*pb.OperationReply, error)) (res *pb.OperationReply, err error) {
  for i := 0; ; i++ {
    res, err = callback()

    if err == nil {
      if i > 0 {
        log.Printf("Attempts spent %v", i)
      }
      return res, nil
    }

    if i >= (attempts - 1) {
      return nil, err
    }

    time.Sleep(500 * time.Millisecond)
  }
  return nil, err
}

func RandomCall() {
  a := int32(rand.Intn(100))
  b := int32(rand.Intn(100))
  if rand.Intn(10) > 5 {
    log.Printf("Add %v + %v = %v", a, b, rpc.Add(a, b))
  } else {
    log.Printf("Multiply %v * %v = %v", a, b, rpc.Multiply(a, b))    
  }
}

func main() {
  log.SetFlags(0)

  rpchost := flag.String("rpc", "localhost:50051", "rpc service address")
  count := flag.Int("count", 10, "number of parallel calls")
  simple := flag.Bool("simple", false, "invoke simple call")
  flag.Parse()

  rpc.Init(*rpchost)
  defer rpc.Close()

  if *simple {
    log.Printf("Add 2 + 3 = %v", rpc.Add(2, 3))
  } else {
    var wg sync.WaitGroup

    start := time.Now()

    for i := 0;  i < *count; i++ {
      wg.Add(1)
      time.Sleep(10 * time.Millisecond)
      go func(i int) {
        defer wg.Done()
        RandomCall()
        }(i)
    }

    wg.Wait()

    elapsed := time.Since(start)
    log.Printf("Time spent %s", elapsed)
    log.Printf("Total connections: %v", rpc.pool.Len())
  }
}
