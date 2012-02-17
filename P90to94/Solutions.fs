// [snippet: Ninety-Nine F# Problems - Problems 90 - 94 - Miscellaneous problems]
/// Ninety-Nine F# Problems - Problems 90 - 94
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
///
// [/snippet]

// [snippet: (**) Problem 90 : Eight queens problem]
/// This is a classical problem in computer science. The objective is to place eight queens on a 
/// chessboard so that no two queens are attacking each other; i.e., no two queens are in the 
/// same row, the same column, or on the same diagonal.
///  
/// Hint: Represent the positions of the queens as a list of numbers 1..N. Example: 
/// [4,2,7,3,6,8,5,1] means that the queen in the first column is in row 4, the queen in the 
/// second column is in row 2, etc. Use the generate-and-test paradigm.
///  
/// Example in F#: 
/// 
/// > queens 8 |> Seq.length;;
/// val it : int = 92
/// > queens 8 |> Seq.head;;
/// val it : int list = [1; 5; 8; 6; 3; 7; 2; 4]
/// > queens 20 |> Seq.head;;
/// val it : int list =
///  [1; 3; 5; 2; 4; 13; 15; 12; 18; 20; 17; 9; 16; 19; 8; 10; 7; 14; 6; 11]

(*[omit:(Solution)]*)
// instead of solving the problem for 8 queens lets solve if for N queens.
// To solve the problem we are going to start with an empty board and then we're going
// add queen to it for each row. Elimitating invalid solutions. To do that we need a function
// (invalidPosition) that detects if one queen is in conflict with another one. And another 
// function (validSolution) that would test if the queen that we're adding is not in 
// conflict with any queen already on the board. 
// Also, the solution is going to return a a sequence of solutions instead of a list.
// That way we can get one solution realy fast if that is only what we care. For example 
// getting all the solutions for a 20x20 board would take a long time, but finding 
// the first solution only takes 5 seconds.
// 

let queens n =
    let invalidPosition (x1, y1) (x2, y2) = (x1 = x2) || (y1 = y2) || abs (x1 - x2) = abs (y1 - y2)
    let validSolution (queen, board) = board |> Seq.exists (invalidPosition queen) |> not
    // With the function "loop", we're going to move one column at time, placing queens
    // on each row and creating new boards with only valid solutions.
    let rec loop boards y =
        if y = 0 then
            boards
        else
            let boards' = boards 
                       |> Seq.collect(fun board -> [1 .. n] |> Seq.map(fun x -> (x,y),board))
                       |> Seq.filter validSolution 
                       |> Seq.map(fun (pos, xs) -> pos::xs)
            loop boards' (y - 1)
    loop (Seq.singleton([])) n |> Seq.map (List.rev >> List.map fst)
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 91 : Knight's tour]
/// Another famous problem is this one: How can a knight jump on an NxN chessboard in such a way 
/// that it visits every square exactly once? A set of solutions is given on the The_Knights_Tour
/// page.
///  
/// Hints: Represent the squares by pairs of their coordinates of the form X/Y, where both X and 
/// Y are integers between 1 and N. (Note that '/' is just a convenient functor, not division!) 
/// Define the relation jump(N,X/Y,U/V) to express the fact that a knight can jump from X/Y to U/V 
/// on a NxN chessboard. And finally, represent the solution of our problem as a list of N*N knight
/// positions (the knight's tour).
///  
/// There are two variants of this problem: 
/// 1. find a tour ending at a particular square 
/// 2. find a circular tour, ending a knight's jump from the start (clearly it doesn't matter where 
///    you start, so choose (1,1))
///  
/// Example in F#: 
/// 
/// > knightsTour 8 (1,1) |> Seq.head;;
/// val it : (int * int) list =
///   [(4, 3); (6, 4); (5, 6); (4, 8); (3, 6); (5, 5); (6, 3); (4, 4); (2, 3);
///    (1, 5); (3, 4); (5, 3); (6, 5); (4, 6); (2, 7); (3, 5); (5, 4); (6, 6);
///    (4, 5); (2, 4); (1, 6); (2, 8); (4, 7); (6, 8); (8, 7); (7, 5); (8, 3);
///    (7, 1); (5, 2); (3, 1); (1, 2); (3, 3); (4, 1); (2, 2); (1, 4); (2, 6);
///    (1, 8); (3, 7); (5, 8); (7, 7); (8, 5); (7, 3); (8, 1); (6, 2); (7, 4);
///    (8, 2); (6, 1); (4, 2); (2, 1); (1, 3); (2, 5); (1, 7); (3, 8); (5, 7);
///    (7, 8); (8, 6); (6, 7); (8, 8); (7, 6); (8, 4); (7, 2); (5, 1); (3, 2);
///    (1, 1)]
///
/// > endKnightsTour 8 (4,2);;
/// val it : (int * int) list =
///   [(4, 2); (2, 1); (1, 3); (3, 2); (1, 1); (2, 3); (1, 5); (2, 7); (4, 8);
///    (6, 7); (8, 8); (7, 6); (6, 8); (8, 7); (7, 5); (8, 3); (7, 1); (5, 2);
///    (3, 1); (1, 2); (2, 4); (1, 6); (2, 8); (4, 7); (2, 6); (1, 8); (3, 7);
///    (5, 8); (7, 7); (8, 5); (7, 3); (8, 1); (6, 2); (4, 1); (2, 2); (1, 4);
///    (3, 5); (5, 6); (4, 4); (2, 5); (1, 7); (3, 8); (5, 7); (7, 8); (8, 6);
///    (7, 4); (6, 6); (4, 5); (3, 3); (5, 4); (4, 6); (6, 5); (8, 4); (7, 2);
///    (6, 4); (4, 3); (5, 1); (6, 3); (8, 2); (6, 1); (5, 3); (3, 4); (5, 5);
///    (3, 6)]
///
/// > closedKnightsTour 8;;
/// val it : (int * int) list =
///   [(2, 3); (4, 4); (6, 3); (5, 5); (4, 3); (6, 4); (5, 6); (4, 8); (3, 6);
///    (1, 5); (3, 4); (5, 3); (6, 5); (4, 6); (2, 7); (3, 5); (5, 4); (6, 6);
///    (4, 5); (2, 4); (1, 6); (2, 8); (4, 7); (6, 8); (8, 7); (7, 5); (8, 3);
///    (7, 1); (5, 2); (3, 1); (1, 2); (3, 3); (4, 1); (2, 2); (1, 4); (2, 6);
///    (1, 8); (3, 7); (5, 8); (7, 7); (8, 5); (7, 3); (8, 1); (6, 2); (7, 4);
///    (8, 2); (6, 1); (4, 2); (2, 1); (1, 3); (2, 5); (1, 7); (3, 8); (5, 7);
///    (7, 8); (8, 6); (6, 7); (8, 8); (7, 6); (8, 4); (7, 2); (5, 1); (3, 2);
///    (1, 1)]

(*[omit:(Solution)]*)
// Wikipedia has a nice article about this problem http://en.wikipedia.org/wiki/Knights_tour
//
// The way this algorithm works is like this. We create a set (board) with all the positions
// in the board that have not being used. Also we have a function (moves) that returns a 
// list of posible moves from the current position. The variable 'validMoves' is the result of
// removing all the positions returned by 'moves' that are not in the set 'board' (positions 
// that are still available). If validMoves is empty, that means that we can not move 
// anymore. If at that time the board is empty, we have a solution! Otherwise we remove the 
// current position from the board add the curent position to the tour and continue to one 
// of the valid moves.
// Now, the trick to make the algorithm converge is to move first to the valid position 
// that has the less options once we move (Warnsdorff's rule).
// 

let moves n (x,y) =
    [(x + 2, y + 1); (x + 2, y - 1); (x - 2, y + 1); (x - 2, y - 1); (x - 1, y + 2); (x - 1, y - 2); (x + 1, y + 2); (x + 1, y - 2) ] 
    |> List.filter(fun (x,y) -> x > 0 && x <= n && y > 0 && y <= n)
        
let knightsTours n start =
    let board = [1 .. n] |> List.collect(fun x -> [1 .. n] |> List.map(fun y -> (x,y))) |> Set.ofList
    let rec loop tour board = seq {
        let validMoves = tour 
                         |> List.head // the head of the tour is our current position
                         |> moves n
                         |> List.filter(fun p -> board |> Set.contains p) 
        match validMoves with
            | [] -> if board |> Set.isEmpty then yield tour // we found a solution!
            | _ -> 
                // the call to sortBy is what makes this algorithm converge fast. 
                // We want to go first to the position with the less options
                // once we move (Warnsdorff's rule).
                for p in validMoves |> List.sortBy(moves n >> List.length) do 
                    yield! loop (p::tour) <| Set.remove p board
    }
    loop [start] <| Set.remove start board

let closedKnightsTour n =
    let start = (1,1)
    let finish = moves n start |> Set.ofList
    let flip f a b = f b a
    // lets find the first solution that ends in a position next to the start
    knightsTours n start |> Seq.find(List.head >> flip Set.contains finish)

let endKnightsTour n finish =
    // just find a tour that starts with finish and reverse it!
    knightsTours n finish |> Seq.head |> List.rev
(*[/omit]*)
// [/snippet]

// [snippet: (***) Problem 92 : Von Koch's conjecture]
/// Several years ago I met a mathematician who was intrigued by a problem for which he didn't
/// know a solution. His name was Von Koch, and I don't know whether the problem has been 
/// solved since.
///  
///                                         6
///        (d)   (e)---(f)        (4)   (1)---(7)
///         |     |              1 |     | 5
///        (a)---(b)---(c)        (3)---(6)---(2)
///         |                    2 |  3     4
///        (g)                    (5)
///
/// Anyway the puzzle goes like this: Given a tree with N nodes (and hence N-1 edges). Find a 
/// way to enumerate the nodes from 1 to N and, accordingly, the edges from 1 to N-1 in such 
/// a way, that for each edge K the difference of its node numbers equals to K. The conjecture 
/// is that this is always possible.
///  
/// For small trees the problem is easy to solve by hand. However, for larger trees, and 14 is 
/// already very large, it is extremely difficult to find a solution. And remember, we don't 
/// know for sure whether there is always a solution!
///  
/// Write a predicate that calculates a numbering scheme for a given tree. What is the solution
/// for the larger tree pictured below?
///
///     (i) (g)   (d)---(k)         (p)
///        \ |     |                 |
///         (a)---(c)---(e)---(q)---(n)
///        / |     |           |
///     (h) (b)   (f)         (m)
///
/// Example in F#:  
/// > vonKoch (['d';'a';'g';'b';'c';'e';'f'],[('d', 'a');('a', 'g');('a', 'b');('b', 'e');
///                ('b', 'c');('e', 'f')]) |> Seq.head;;
///
/// val it : int list * (int * int * int) list =
///   ([4; 3; 5; 6; 2; 1; 7],
///    [(4, 3, 1); (3, 5, 2); (3, 6, 3); (6, 1, 5); (6, 2, 4); (1, 7, 6)])
///

(*[omit:(Solution)]*)
// After some searching on the internet I couldn't find an algorithm for Graceful labeling.
// So I decided to go the brute force route. I knew this would work with the first the example
// but I wasn't sure if it would work for the second tree (a tree with 14 Nodes means that we have
// 14! (87,178,291,200) posible ways to tag the tree).
// Luckly, it did!!

// To represent the trees, I decided to use a tuple with a list of nodes and a list of tuples with the edges
type 'a Graph = 'a list * ('a * 'a) list

// Here are the two examples above using that representation.
let g = (['d';'a';'g';'b';'c';'e';'f'],[('d', 'a');('a', 'g');('a', 'b');('b', 'e');('b', 'c');('e', 'f')])

let g' = (['i';'h';'g';'a';'b';'d';'c';'f';'k';'e';'q';'m';'p';'n'],[('i', 'a');('h', 'a');('a', 'b');('a', 'g');('a', 'c');('c', 'f');('c','d');('d','k');('c','e');('e','q');('q','m');('q','n');('n','p')])

// Now I knew how to generate permutations in F# from this snippet: http://fssnip.net/48
// But the problem was, that implementation was using lists and it would not work to generate the 
// 87 billion permutations for the 14 node tree. Then I remember the LazyList type in the F#
// Power Pack. Now I can generate the permutations in a lazy way and pray that a solution 
// can be found fast.
// Here is the implemetation of using LazyList.

#if INTERACTIVE 
#r "FSharp.PowerPack.dll"
#endif

open Microsoft.FSharp.Collections

// the term interleave x ys returns a  list of all possible ways of inserting 
// the element x into the list ys.
let rec interleave x = function
    | LazyList.Nil -> LazyList.ofList [ LazyList.ofList [x]]
    | LazyList.Cons(y,ys) -> LazyList.ofSeq (seq { yield LazyList.cons x (LazyList.cons y ys)
                                                   for zs in interleave x ys do
                                                       yield LazyList.cons y zs })
        
// the function perms returns a lazy list of all permutations of a list.
let rec perms = function
    | LazyList.Nil -> LazyList.ofList [LazyList.empty]
    | LazyList.Cons(x,xs) -> LazyList.concat ( LazyList.map (interleave x) (perms xs))

// Now with the problem of generating all the permutations solved. 
// It's time to tackle the real problem.
let vonKoch (nodes, edges) =
    // diff is used to compute the edge difference acording the the map m
    let diff (m : Map<_, _>) (a,b) = abs <| m.[a] - m.[b]
    let size = nodes |> List.length
    let edgSize = edges |> List.length
    match nodes with
        | [] -> failwith "Empty graph!!"
        | _  when size <> (edgSize + 1) -> // make sure that we have a valid tree
            failwith "The tree doesn't have N - 1 egdes. Where N is the number of nodes"
        | _  -> 
            seq {
            for p in perms <| LazyList.ofList [1 .. size] do
                let sol = LazyList.toList p
                let m = sol |> List.zip nodes  |> Map.ofList
                // I'm using Set here to filter out any duplicates. 
                // It's faster than Seq.distinct
                let count = edges |> List.map (diff m) |> Set.ofList |> Set.count 
                // if the number of distint differences is equal to the number 
                // of edges, we found a solution!
                if count = edgSize then 
                    yield (sol, edges |> List.map (fun ((a,b) as e) -> m.[a], m.[b], diff m e))
            }
(*[/omit]*)
// [/snippet]


// [snippet: (***) Problem 93 : An arithmetic puzzle]
/// Given a list of integer numbers, find a correct way of inserting arithmetic signs (operators)
/// such that the result is a correct equation. Example: With the list of numbers [2,3,5,7,11] we 
/// can form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).
///  
/// Division should be interpreted as operating on rationals, and division by zero should be 
/// avoided.
///  
/// Example in F#: 
/// 
/// > solutions [2;3;5;7;11] |> List.iter (printfn "%s");;
/// 2 = 3 - (5 + (7 - 11))
/// 2 = 3 - ((5 + 7) - 11)
/// 2 = (3 - 5) - (7 - 11)
/// 2 = (3 - (5 + 7)) + 11
/// 2 = ((3 - 5) - 7) + 11
/// 2 = ((3 * 5) + 7) / 11
/// 2 * (3 - 5) = 7 - 11
/// 2 - (3 - (5 + 7)) = 11
/// 2 - ((3 - 5) - 7) = 11
/// (2 - 3) + (5 + 7) = 11
/// (2 - (3 - 5)) + 7 = 11
/// ((2 - 3) + 5) + 7 = 11
/// val it : unit = ()
///

(*[omit:(Solution)]*)
// This is similar to "The countdow problem" on chapter 11 in the book
// Programming in Haskell by Graham Hutton

// First let's define our operations. The ToString override is there to help
// on printing the solutions later on.
type Op = Add | Sub | Mult | Div
    with override op.ToString() =
            match op with
            | Add -> "+"
            | Sub -> "-"
            | Mult -> "*"
            | Div -> "/"

// Here we define wich opertaions are valid.
// For Add or Sub there is no problem
// For Mult we dont want trivial mutiplications by 1. Although the 
// problem statement is not clear if that is an issue.
// For Div we don't want division by 0, by 1 or fractions
let valid op x y =
    match op with
        | Add -> true
        | Sub -> true
        | Mult -> x <> 1 && y <> 1 
        | Div  -> y <> 0 && y <> 1 && x % y = 0

// this is function applies the operation to the x and y arguments
let app op x y =
    match op with
        | Add -> x + y
        | Sub -> x - y
        | Mult -> x * y
        | Div -> x / y

// Now, we define our expresions. This is how are we going to build the
// solutions
type Expr = Val of int | App of Op * Expr * Expr

// Just for fun, I implemented the fold function for our expresions.
// There was no need since we only use it once on the toString function.
let foldExpr fval fapp expr =
    let rec loop expr cont =
        match expr with
            | Val n -> cont <| fval n
            | App(op, l, r) -> loop l <| fun ls -> loop r <| fun rs -> cont <| fapp op ls rs
    loop expr id

// Once we have fold over expresions impelmenting toString is a one-liner.
// The code after the fold is just to remove the outher parentesis.
let toString exp = 
    let str = exp |> foldExpr string (fun op l r -> "(" + l + " " + string op + " " + r + ")")
    if str.StartsWith("(") then
        str.Substring(1,str.Length - 2)
    else
        str

// The 'eval' function returns a sigleton list with the result of the evaluation.
// If the expresion is not valid, returns the empty list ([])
let rec eval = function
    | Val n -> [n]
    | App(op, l, r) -> 
        [for x in eval l do
            for y in eval r do
                if valid op x y then
                    yield app op x y]

// The function 'init', 'inits', 'tails' are here to help implement the 
// function splits and came from haskell

// the function inits accepts a list and returns the list without its last item
let rec init = function
    | [] -> failwith "empty list!"
    | [_] -> []
    | x::xs -> x :: init xs
    
// The function inits returns the list of all initial segments
// of a list , in order of increasing length.
// Example:
// > inits [1..4];;
// val it : int list list = [[]; [1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]]
let rec inits = function
    | [] -> [[]]
    | x::xs -> [ yield []
                 for ys in inits xs do
                     yield x::ys]

// the function tails returns the list of initial segments 
// of its argument list, shortest last
// Example:
// > tails [1..4];;
// val it : int list list = [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []]
let rec tails = function
    | [] -> [[]]
    | x::xs as ls -> 
        [ yield ls
          for ys in tails xs do
              yield ys ]

// this is what drives the solution to this problem and 
// came from the haskell solution.
// Here is an example of its use:
// > splits [1..4];;
// val it : (int list * int list) list =
//  [([1], [2; 3; 4]); ([1; 2], [3; 4]); ([1; 2; 3], [4])]
// As you can see, it returs all the ways we can split a list.
let splits xs = List.tail (init (List.zip (inits xs) (tails xs)))

// Now that we're armed with all these functions, we're ready to tackle the real problem.

// The goal of the function expressions is to build all valid expressions and its value given a 
// list  of numbers. First we split the list in all posible ways (1). Then we take
// the left side of the split and build all the valid expresions (2). We do the same for the
// right side (3). Now we combine the two expresions with all the operators (4). If the operation
// is valid, we add it to the list of expressions (5,6).
let rec expressions = function
    | [x] -> [(Val x, x)]
    | xs  -> [ for xsl, xsr in splits xs do (* 1 *)
                for (expl, vall) in expressions xsl do (* 2 *)
                    for (expr, valr) in expressions xsr do (* 3 *)
                        for op in [Add; Sub; Mult; Div] do (* 4 *)
                            if valid op vall valr then (* 5 *)
                                yield (App (op, expl, expr) ,app op vall valr) (* 6 *)]


// Now that we have a way of generating valid expressions, it's time to
// generate the equaions. Again, we split the list of numbers (1). Then we generate the 
// list of expressions from the left side of the split (2). Same with the right side (3).
// If both expressions have the same value, add it to our soutions (4,5).
let equations = function
    | []  -> failwith "error: empty list"
    | [_] -> failwith "error: singleton list"
    | xs  -> [for xsl, xsr in splits xs do (* 1 *)
                for el, vl in expressions xsl do (* 2 *)
                    for er, vr in expressions xsr do (* 3 *)
                        if vl = vr then (* 4 *)
                            yield (el, er) (* 5 *)]

// Go thought the list of equations a pretty-print them.
let solutions = equations >> List.map(fun (exp1, exp2) -> toString exp1 + " = " + toString exp2)
(*[/omit]*)
// [/snippet]

// [snippet: (***) Problem 94 : Generate K-regular simple graphs with N nodes]
/// In a K-regular graph all nodes have a degree of K; i.e. the number of edges incident in each 
/// node is K. How many (non-isomorphic!) 3-regular graphs with 6 nodes are there?

(*[omit:(Solution needed)]*)
let solution94 = "your solution here!!"
(*[/omit]*)
// [/snippet]