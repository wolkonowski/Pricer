namespace ViewModel.MonteCarlo
module MonteCarlo =
    let expFunction (r:double) (sg:double) (t:double) (n:double) (random:double) = (r-sg*sg/2.0)*t/n + sg * random * sqrt(t/n)
    
    let randomUnif (generator:System.Random) : double = generator.NextDouble()
    let random generator =  //TOCHECK THAT NORMAL
        let r1 = randomUnif generator
        let r2 = randomUnif generator
        sqrt(-2.0*log(r1)) * sin(2.0*System.Math.PI*r2)
    let nextstep S0 n r sg t generator = S0*exp(expFunction r sg t n <| random generator)

    let prices S0 n r sq t generator =  Seq.unfold (fun (i:double) -> Some((i,nextstep i n r sq t generator))) S0

    let calc n S0 r sg t generator = 
        let pricesSeq = prices S0 (double(n)) r sg t generator |> Seq.take n
        pricesSeq
    let runs N n S0 r sg t seed = 
        let generator = new System.Random(seed)
        let results = Seq.init N <| (fun _ -> calc n S0 r sg t generator)
        results

    let avgPrice (seq:seq<seq<double>>) = 
        seq |> Seq.map (fun x -> Seq.average x) |> Seq.average
    let maxPrice (seq:seq<seq<double>>) = 
        seq |> Seq.map (fun x -> Seq.max x) |> Seq.average
    let minPrice (seq:seq<seq<double>>) = 
        seq |> Seq.map (fun x -> Seq.min x) |> Seq.average

    let americanCall (N:int) (steps:int) (S0:double) (K:double) (r:double) (sg:double) (T:double) (seed:int )= 
        let m = maxPrice <| runs N steps S0 r sg T seed
        max (m-K) 0.0
    let americanPut (N:int) (steps:int) (S0:double) (K:double) (r:double) (sg:double) (T:double) (seed:int )= 
        let m = minPrice <| runs N steps S0 r sg T seed
        max (K-m) 0.0
    let avgFPrice (seq:seq<seq<double>>) = 
        seq |> Seq.map (fun x -> x |> Seq.rev |> Seq.head) |> Seq.average
    let europeanCall (N:int) (steps:int) (S0:double) (K:double) (r:double) (sg:double) (T:double) (seed:int )= 
        let m = avgFPrice <| runs N steps S0 r sg T seed
        max (m-K) 0.0
    let europeanPut (N:int) (steps:int) (S0:double) (K:double) (r:double) (sg:double) (T:double) (seed:int )= 
        let m = avgFPrice <| runs N steps S0 r sg T seed
        max (K-m) 0.0
    let asianPut (N:int) (steps:int) (S0:double) (K:double) (r:double) (sg:double) (T:double) (seed:int ) = 
        0
    let asianCall (N:int) (steps:int) (S0:double) (K:double) (r:double) (sg:double) (T:double) (seed:int ) = 
        0
    