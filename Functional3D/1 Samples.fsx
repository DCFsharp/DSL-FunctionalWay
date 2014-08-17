#r "references/OpenTK.dll"
#r "references/OpenTK.GLControl.dll"
#load "functional3d.fs"

open Functional3D
open System.Drawing

// ------------------------------------------------------------------

( Fun.color Color.Yellow Fun.cylinder ) $
( Fun.cone
  |> Fun.color Color.Red 
  |> Fun.translate (0.0, 0.0, -1.0) )

// ------------------------------------------------------------------

let tower x z = 
  (Fun.cylinder
     |> Fun.scale (1.0, 1.0, 3.0) 
     |> Fun.translate (0.0, 0.0, 1.0)
     |> Fun.color Color.DarkGoldenrod ) $ 
  (Fun.cone 
     |> Fun.scale (1.3, 1.3, 1.3) 
     |> Fun.translate (0.0, 0.0, -1.0)
     |> Fun.color Color.Red )
  |> Fun.rotate (90.0, 0.0, 0.0)
  |> Fun.translate (x, 0.5, z)

// Create one tower
tower 0.0 0.0

// Now we can easily compose towers!
tower -2.0 0.0 $ tower 2.0 0.0
                                                                                                                                
// ------------------------------------------------------------------

let sizedCube height = 
  Fun.cube 
  |> Fun.scale (0.5, height, 1.0) 
  |> Fun.translate (-0.5, height/2.0 - 1.0, 0.0)

let twoCubes =
  sizedCube 0.8 $ (sizedCube 1.0 |> Fun.translate (0.5, 0.0, 0.0))

let block = 
  [ for offset in -4.0 .. +4.0 ->
      twoCubes |> Fun.translate (offset, 0.0, 0.0) ]
  |> Seq.reduce ($)
  |> Fun.scale (0.5, 2.0, 0.3)
  |> Fun.color Color.DarkGray
  
// ------------------------------------------------------------------

let wall offs rotate = 
  let rotationArg = if rotate then (0.0, 90.0, 0.0) else (0.0, 0.0, 0.0)
  let translationArg = if rotate then (offs, 0.0, 0.0) else (0.0, 0.0, offs)
  block |> Fun.rotate rotationArg |> Fun.translate translationArg

tower -2.0 -2.0 $ tower 2.0 -2.0 $ 
  tower -2.0 2.0 $ tower 2.0 2.0 $
  wall -2.0 true $ wall 2.0 true $
  wall -2.0 false $ wall 2.0 false

// ------------------------------------------------------------------
// Recursion 
// ------------------------------------------------------------------

let pattern = 
  [| [| [| 1; 1; 1; |]; [| 1; 0; 1 |]; [| 1; 1; 1 |] |]
     [| [| 1; 0; 1; |]; [| 0; 0; 0 |]; [| 1; 0; 1 |] |]
     [| [| 1; 1; 1; |]; [| 1; 0; 1 |]; [| 1; 1; 1 |] |] |]
  |> Array3D.fromCube

let rec generate depth = 
  [ for x in -1 .. 1 do
    for y in -1 .. 1 do
    for z in -1 .. 1 do
      if pattern.[x, y, z] = 1 then 
        let size = 3.0 ** float depth
        let ofs = float x * size, float y * size, float z * size
        let sub = if depth = 0 then Fun.cube
                  else generate (depth - 1) 
        yield Fun.translate ofs sub ]
  |> List.reduce ($)
  |> Fun.color Color.ForestGreen

// Generate fractal with various level of detail
  
Fun.setDistance(-20.0)

generate 0
generate 1

Fun.setDistance(-60.0)
generate 2

// ------------------------------------------------------------------
// Trees are an example of recursive structure
// ------------------------------------------------------------------

let random = System.Random()

let noise k x =
  x + (k * x * (random.NextDouble() - 0.5))

let color() = 
  [| Color.Red; Color.Orange; 
     Color.Yellow |].[random.Next 3]

let trunk (width,length) = 
  Fun.cylinder
  |> Fun.translate (0.0,0.0,0.5) |> Fun.scale (width,width,length)  
        
let fruit (size) = 
  Fun.sphere
  |> Fun.color (color()) |> Fun.scale (size,size,size)

let example = trunk (1.0,5.0) $ fruit 2.0


// Recursive tree
let rec tree trunkLength trunkWidth w n = 
  let moveToEndOfTrunk = Fun.translate (0.0,0.0,trunkLength)
  if n <= 1 then
    trunk (trunkWidth,trunkLength) $  // branch and end with
    (fruit (3.0 * trunkWidth) |> moveToEndOfTrunk)  // fruit
  else 
    // generate branch
    let branch angleX angleY = 
      let branchLength = trunkLength * 0.92 |> noise 0.2  // reduce length
      let branchWidth  = trunkWidth  * 0.65 |> noise 0.2  // reduce width
      tree branchLength branchWidth w (n-1) 
      |> Fun.rotate (angleX,angleY,0.0) |> moveToEndOfTrunk
      
    trunk (trunkWidth,trunkLength)  // branch and follow by several
      $ branch  w  0.0              // smaller branches with rotation +/- w
      $ branch -w  0.0
      $ branch 0.0   w
      $ branch 0.0  -w

let plant = 
  tree 4.0(*long*) 0.8(*wide*) 40.0(*angle*) 4(*levels*)
  |> Fun.rotate (90.0, 180.0, 90.0)
  |> Fun.translate (0.0, -6.0, 0.0)


Fun.resetRotation()




open System.Collections.Generic
open System


type Instruction<'T> =
        | Enqueue of 'T * (unit -> unit)
        | Dequeue of ('T -> unit);;

type AsyncBoundedQueue<'T>(capacity: int) =

        let waitingConsumers, elts, waitingProducers = Queue(), Queue<'T>(), Queue()

        let rec balance() =
          if elts.Count > 0 && waitingConsumers.Count > 0 then
            elts.Dequeue() |> waitingConsumers.Dequeue()
            balance()
          elif elts.Count < capacity && waitingProducers.Count > 0 then
            let x, reply = waitingProducers.Dequeue()
            reply()
            elts.Enqueue x
            balance()

        let agent = MailboxProcessor.Start(fun inbox ->
          async { while true do
                    let! msg = inbox.Receive()
                    match msg with
                    | Enqueue(x, reply) ->
                        waitingProducers.Enqueue (x, reply)
                    | Dequeue reply ->
                        waitingConsumers.Enqueue reply
                    balance() })

        member __.AsyncEnqueue x =
          agent.PostAndAsyncReply(fun reply -> Enqueue(x, reply.Reply))
        member __.AsyncDequeue() =
          agent.PostAndAsyncReply(fun reply -> Dequeue reply.Reply)

        interface System.IDisposable with
          member __.Dispose() = (agent :> System.IDisposable).Dispose();;


while true do
        let n = 3000
        use queue = new AsyncBoundedQueue<_>(10)
        [ async { let timer = System.Diagnostics.Stopwatch.StartNew()
                  for i in 1..n do
                    do! queue.AsyncEnqueue i
                   // ts.Add timer.Elapsed.TotalSeconds
                  return "Producing", float n / timer.Elapsed.TotalSeconds }
          async { let timer = System.Diagnostics.Stopwatch.StartNew()
                  for i in 1..n do
                    let! _ = queue.AsyncDequeue()
                    while timer.Elapsed.TotalMilliseconds < float i do
                      do! Async.Sleep 1
                  return "Consuming", float n / timer.Elapsed.TotalSeconds } ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Seq.iter (fun (s, t) -> printfn "%s at %0.0f msgs/s" s t);


type message =
    | Finished of AsyncReplyChannel<string Set>
    | Visit of string * AsyncReplyChannel<string Set>;;


open System.Text.RegularExpressions;;

let link = Regex("href=\"([^\"]+)");;

let crawl (baseUri: string) =
    let timer = System.Diagnostics.Stopwatch.StartNew()
    let baseUri = System.Uri baseUri
    use box = MailboxProcessor.Start(fun inbox ->
      let rec loop n visited = async {
          let! msg = inbox.Receive()
          match msg with
          | Finished reply ->
              do! recur (n-1) visited reply
          | Visit(uri, reply) ->
              if Set.contains uri visited then
                do! recur n visited reply
              else
                async {
                  use client = new System.Net.WebClient()
                  let! html = client.AsyncDownloadString baseUri
                  for url: Match in link.Matches html |> Seq.cast do
                    let uri = ref null
                    if System.Uri.TryCreate(baseUri, url.Groups.[1].Value, uri) then
                      inbox.Post(Visit(string !uri, reply))
                  inbox.Post(Finished reply)
                } |> Async.Start
                do! recur (n+1) (Set.add uri visited) reply
        }
      and recur n visited (reply: AsyncReplyChannel<_>) = async {
          if n=0 && inbox.CurrentQueueLength=0 then
            reply.Reply visited
          else
            do! loop n visited
        }
      loop 0 Set.empty)
    box.PostAndReply(fun reply -> Visit(string baseUri, reply));;

crawl "http://www.expert-fsharp.com"


let rec fib = function
    | 0 | 1 as n -> n
    | n -> fib(n-1) + fib(n-2);;

> let rec fib i = 
        function 
            | 0 
            | 1 as n -> n
            | n when n<=i ->
                fib i (n-1) + fib i (n-2) | n ->
                let p = System.Threading.Tasks.Future.Create(fun () -> fib i (n-2)) 
                let q = fib i (n-1)
                p.Value + q;;


    val fib : int -> int > time fib 30;;