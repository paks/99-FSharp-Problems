// [snippet: Ninety-Nine F# Problems - Problems 31 - 41 - Arithmetic]
/// Ninety-Nine F# Problems - Problems 31 - 41 
///
/// These are F# solutions of Ninety-Nine Haskell Problems 
/// (http://www.haskell.org/haskellwiki/H-99:_Ninety-Nine_Haskell_Problems), 
/// which are themselves translations of Ninety-Nine Lisp Problems
/// (http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html)
/// and Ninety-Nine Prolog Problems
/// (https://sites.google.com/site/prologsite/prolog-problems).
///
/// If you would like to contribute a solution or fix any bugs, send 
/// an email to paks at kitiara dot org with the subject "99 F# problems". 
/// I'll try to update the problem as soon as possible.
///
/// The problems have different levels of difficulty. Those marked with a single asterisk (*) 
/// are easy. If you have successfully solved the preceeding problems you should be able to 
/// solve them within a few (say 15) minutes. Problems marked with two asterisks (**) are of 
/// intermediate difficulty. If you are a skilled F# programmer it shouldn't take you more than 
/// 30-90 minutes to solve them. Problems marked with three asterisks (***) are more difficult. 
/// You may need more time (i.e. a few hours or more) to find a good solution
///
/// Though the problems number from 1 to 99, there are some gaps and some additions marked with 
/// letters. There are actually only 88 problems.
// [/snippet]

// [snippet: (**) Problem 31 : Determine whether a given integer number is prime.]
/// Example: 
/// * (is-prime 7)
/// T
///  
/// Example in F#: 
/// 
/// > isPrime 7;;
/// val it : bool = true

(*[omit:(Solution 1)]*)
//naive solution
let isPrime n = 
    let sqrtn n = int <| sqrt (float n)
    seq { 2 .. sqrtn n } |> Seq.exists(fun i -> n % i = 0) |> not
(*[/omit]*)

(*[omit:(Solution 2)]*)
// Miller-Rabin primality test
open System.Numerics

let pow' mul sq x' n' = 
    let rec f x n y = 
        if n = 1I then
            mul x y
        else
            let (q,r) = BigInteger.DivRem(n, 2I)
            let x2 = sq x
            if r = 0I then
                f x2 q y
            else
                f x2 q (mul x y)
    f x' n' 1I
        
let mulMod (a :bigint) b c = (b * c) % a
let squareMod (a :bigint) b = (b * b) % a
let powMod m = pow' (mulMod m) (squareMod m)
let iterate f = Seq.unfold(fun x -> let fx = f x in Some(x,fx))

///See: http://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
let millerRabinPrimality n a =
    let find2km n = 
        let rec f k m = 
            let (q,r) = BigInteger.DivRem(m, 2I)
            if r = 1I then
                (k,m)
            else
                f (k+1I) q
        f 0I n
    let n' = n - 1I
    let iter = Seq.tryPick(fun x -> if x = 1I then Some(false) elif x = n' then Some(true) else None)
    let (k,m) = find2km n'
    let b0 = powMod n a m

    match (a,n) with
        | _ when a <= 1I && a >= n' -> 
            failwith (sprintf "millerRabinPrimality: a out of range (%A for %A)" a n)
        | _ when b0 = 1I || b0 = n' -> true
        | _  -> b0 
                 |> iterate (squareMod n) 
                 |> Seq.take(int k)
                 |> Seq.skip 1 
                 |> iter 
                 |> Option.exists id 

///For Miller-Rabin the witnesses need to be selected at random from the interval [2, n - 2]. 
///More witnesses => better accuracy of the test.
///Also, remember that if Miller-Rabin returns true, then the number is _probable_ prime. 
///If it returns false the number is composite.
let isPrimeW witnesses = function
    | n when n < 2I -> false
    | n when n = 2I -> true
    | n when n = 3I -> true
    | n when n % 2I = 0I -> false
    | n             -> witnesses |> Seq.forall(millerRabinPrimality n)

// let isPrime' = isPrimeW [2I;3I] // Two witnesses
// let p = pown 2I 4423 - 1I // 20th Mersenne prime. 1,332 digits
// isPrime' p |> printfn "%b";;
// Real: 00:00:03.184, CPU: 00:00:03.104, GC gen0: 12, gen1: 0, gen2: 0
// val it : bool = true
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 32 : Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.]
/// Example: 
/// * (gcd 36 63)
/// 9
///  
/// Example in F#: 
/// 
/// > [gcd 36 63; gcd (-3) (-6); gcd (-3) 6];;
/// val it : int list = [9; 3; 3]

(*[omit:(Solution)]*)
let rec gcd a b =
    if b = 0 then
        abs a
    else
        gcd b (a % b)
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 33 : Determine whether two positive integer numbers are coprime.]
/// Two numbers are coprime if their greatest common divisor equals 1.
///  
/// Example: 
/// * (coprime 35 64)
/// T
///  
/// Example in F#: 
/// 
/// > coprime 35 64;;
/// val it : bool = true

(*[omit:(Solution)]*)
// using problem 32
let coprime a b = gcd a b = 1
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 34 : Calculate Euler's totient function phi(m).]
/// Euler's so-called totient function phi(m) is defined as the number of 
/// positive integers r (1 <= r < m) that are coprime to m.
///  
/// Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
///  
/// Example: 
/// * (totient-phi 10)
/// 4
///  
/// Example in F#: 
/// 
/// > totient 10;;
/// val it : int = 4

(*[omit:(Solution)]*)
// naive implementation. For a better solution see problem 37
let totient n = seq { 1 .. n - 1} |> Seq.filter (gcd n >> (=) 1) |> Seq.length
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 35 : Determine the prime factors of a given positive integer.]
/// Construct a flat list containing the prime factors in ascending order.
///  
/// Example: 
/// * (prime-factors 315)
/// (3 3 5 7)
///  
/// Example in F#: 
/// 
/// > primeFactors 315;;
/// val it : int list = [3; 3; 5; 7]

(*[omit:(Solution)]*)
let primeFactors n =
    let sqrtn n = int <| sqrt (float n)
    let get n =
        let sq = sqrtn n
        // this can be made faster by using a prime generator like this one : 
        // https://github.com/paks/ProjectEuler/tree/master/Euler/Primegen
        seq { yield 2; yield! seq {3 .. 2 .. sq} } |> Seq.tryFind (fun x -> n % x = 0) 
    let divSeq = n |> Seq.unfold(fun x ->
        if x = 1 then
            None
        else
            let sq = sqrtn x
            match get x with
                | None -> Some(x, 1) // x it's prime
                | Some(divisor) -> Some(divisor, x/divisor))
    divSeq |> List.ofSeq
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 36 : Determine the prime factors of a given positive integer.]
/// 
/// Construct a list containing the prime factors and their multiplicity. 
/// 
/// Example: 
/// * (prime-factors-mult 315)
/// ((3 2) (5 1) (7 1))
///  
/// Example in F#: 
/// 
/// > primeFactorsMult 315;;
/// [(3,2);(5,1);(7,1)]

(*[omit:(Solution)]*)
// using problem 35
let primeFactorsMult n =
    let sqrtn n = int <| sqrt (float n)
    let get n =
        let sq = sqrtn n
        // this can be made faster by using a prime generator like this one : 
        // https://github.com/paks/ProjectEuler/tree/master/Euler/Primegen
        seq { yield 2; yield! seq {3 .. 2 .. sq} } |> Seq.tryFind (fun x -> n % x = 0) 
    let divSeq = n |> Seq.unfold(fun x ->
        if x = 1 then
            None
        else
            let sq = sqrtn x
            match get x with
                | None -> Some(x, 1) // x it's prime
                | Some(divisor) -> Some(divisor, x/divisor))
    divSeq |> Seq.countBy id |> List.ofSeq
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 37 : Calculate Euler's totient function phi(m) (improved).]
/// See problem 34 for the definition of Euler's totient function. If the list of the prime 
/// factors of a number m is known in the form of problem 36 then the function phi(m) 
/// can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of 
/// prime factors (and their multiplicities) of a given number m. Then phi(m) can be 
/// calculated with the following formula:
///  phi(m) = (p1 - 1) * p1 ** (m1 - 1) + 
///          (p2 - 1) * p2 ** (m2 - 1) + 
///          (p3 - 1) * p3 ** (m3 - 1) + ...
///  
/// Note that a ** b stands for the b'th power of a. 
/// 
/// Note: Actually, the official problems show this as a sum, but it should be a product.
/// > phi 10;;
/// val it : int = 4

(*[omit:(Solution)]*)
// using problem 36
let phi = primeFactorsMult >> Seq.fold(fun acc (p,m) -> (p - 1) * pown p (m - 1) * acc) 1
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 38 : Compare the two methods of calculating Euler's totient function.]
/// Use the solutions of problems 34 and 37 to compare the algorithms. Take the 
/// number of reductions as a measure for efficiency. Try to calculate phi(10090) as an 
/// example.
///  
/// (no solution required) 
/// 
// [/snippet]

// [snippet: (*) Problem 39 : A list of prime numbers.]
/// Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
///  
/// Example in F#: 
/// 
/// > primesR 10 20;;
/// val it : int list = [11; 13; 17; 19]

(*[omit:(Solution)]*)
// using problem 31
let primeR a b = seq { a .. b } |> Seq.filter isPrime |> List.ofSeq
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 40 : Goldbach's conjecture.]
/// Goldbach's conjecture says that every positive even number greater than 2 is the 
/// sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts 
/// in number theory that has not been proved to be correct in the general case. It has 
/// been numerically confirmed up to very large numbers (much larger than we can go 
/// with our Prolog system). Write a predicate to find the two prime numbers that sum up 
/// to a given even integer.
///  
/// Example: 
/// * (goldbach 28)
/// (5 23)
///  
/// Example in F#: 
/// 
/// *goldbach 28
/// val it : int * int = (5, 23)

(*[omit:(Solution)]*)
// using problem 31. Very slow on big numbers due to the implementation of primeR. To speed this up use a prime generator.
let goldbach n =
    let primes = primeR 2 n |> Array.ofList
    let rec findPairSum (arr: int array) front back =
        let sum = arr.[front] + arr.[back]
        match compare sum n with
            | -1 -> findPairSum arr (front + 1) back
            |  0 -> Some(arr.[front] , arr.[back])
            |  1 -> findPairSum arr front (back - 1)
            |  _ -> failwith "not possible"
    Option.get <| findPairSum primes 0 (primes.Length - 1)
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 41 : Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.]
/// In most cases, if an even number is written as the sum of two prime numbers, one of 
/// them is very small. Very rarely, the primes are both bigger than say 50. Try to find 
/// out how many such cases there are in the range 2..3000.
///  
/// Example: 
/// * (goldbach-list 9 20)
/// 10 = 3 + 7
/// 12 = 5 + 7
/// 14 = 3 + 11
/// 16 = 3 + 13
/// 18 = 5 + 13
/// 20 = 3 + 17
/// * (goldbach-list 1 2000 50)
/// 992 = 73 + 919
/// 1382 = 61 + 1321
/// 1856 = 67 + 1789
/// 1928 = 61 + 1867
///  
/// Example in F#: 
/// 
/// > goldbachList 9 20;;
/// val it : (int * int) list =
///   [(3, 7); (5, 7); (3, 11); (3, 13); (5, 13); (3, 17)]
/// > goldbachList' 4 2000 50
/// val it : (int * int) list = [(73, 919); (61, 1321); (67, 1789); (61, 1867)]

(*[omit:(Solution)]*)
let goldbachList a b =
    let start = if a % 2 <> 0 then a + 1 else a
    seq { start .. 2 .. b } |> Seq.map goldbach |> List.ofSeq

let goldbachList' a b limit = goldbachList a b |> List.filter(fst >> (<) limit)
(*[/omit]*)
// [/snippet]
