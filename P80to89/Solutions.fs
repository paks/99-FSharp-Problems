// [snippet: Ninety-Nine F# Problems - Problems 80 - 89 - Graphs]
/// Ninety-Nine F# Problems - Problems 80 - 89
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

// The solutions to the problems below use there definitions for Grahps
type 'a Edge = 'a * 'a

type 'a Graph = 'a list * 'a Edge list

let g = (['b';'c';'d';'f';'g';'h';'k'],[('b','c');('b','f');('c','f');('f','k');('g','h')])

type 'a Node = 'a * 'a list

type 'a AdjacencyGraph = 'a Node list

let ga = [('b',['c'; 'f']); ('c',['b'; 'f']); ('d',[]); ('f',['b'; 'c'; 'k']); ('g',['h']); ('h',['g']); ('k',['f'])]

// [/snippet]

// [snippet: (***) Problem 80 : Conversions]
/// Write predicates to convert between the different graph representations. With these predicates, 
/// all representations are equivalent; i.e. for the following problems you can always pick freely the most 
/// convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because 
/// it's a lot of work to deal with all the special cases. 
/// 
/// Example in F#:
/// 
/// > let g = (['b';'c';'d';'f';'g';'h';'k'],[('b','c');('b','f');('c','f');('f','k');('g','h')]);;
/// 
/// > graph2AdjacencyGraph g;;
/// val it : char AdjacencyGraph =
///   [('b', ['f'; 'c']); ('c', ['f'; 'b']); ('d', []); ('f', ['k'; 'c'; 'b']);
///    ('g', ['h']); ('h', ['g']); ('k', ['f'])]
///
/// > let ga = [('b',['c'; 'f']); ('c',['b'; 'f']); ('d',[]); ('f',['b'; 'c'; 'k']); ('g',['h']); ('h',['g']); ('k',['f'])];;
/// 
/// > adjacencyGraph2Graph ga;;
/// val it : char Graph =
///   (['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'],
///    [('b', 'c'); ('b', 'f'); ('c', 'f'); ('f', 'k'); ('g', 'h')])

(*[omit:(Solution)]*)

let graph2AdjacencyGraph ((ns, es) : 'a Graph) : 'a AdjacencyGraph = 
    let nodeMap = ns |> List.map(fun n -> n, []) |> Map.ofList
    (nodeMap,es) 
    ||> List.fold(fun map (a,b) -> map |> Map.add a (b::map.[a]) |> Map.add b (a::map.[b]))
    |> Map.toList
    
let adjacencyGraph2Graph (ns : 'a AdjacencyGraph) : 'a Graph= 
    let sort ((a,b) as e) = if a > b then (b, a) else e
    let nodes = ns |> List.map fst
    let edges = (Set.empty, ns) 
                ||> List.fold(fun set (a,ns) -> (set, ns) ||> List.fold(fun s b -> s |> Set.add (sort (a,b))) ) 
                |> Set.toSeq 
                |> Seq.sort 
                |> Seq.toList
    (nodes, edges)

(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 81: Path from one node to another one]
/// Write a function that, given two nodes a and b in a graph, returns all 
/// the acyclic paths from a to b.
/// 
/// Example:
/// 
/// Example in F#:
/// 
/// > paths 1 4 [(1,[2;3]);(2,[3]);(3,[4]);(4,[2]);(5,[6]);(6,[5])];;
/// val it : int list list = [[1; 2; 3; 4]; [1; 3; 4]]
///
/// > paths 2 6 [(1,[2;3]);(2,[3]);(3,[4]);(4,[2]);(5,[6]);(6,[5])];;
/// val it : int list list = []

(*[omit(Solution)]*)

let paths start finish (g : 'a AdjacencyGraph) = 
    let map = g |> Map.ofList
    let rec loop route visited = [
        let current = List.head route
        if current = finish then
            yield List.rev route
        else
            for next in map.[current] do
                if visited |> Set.contains next |> not then
                    yield! loop (next::route) (Set.add next visited) 
    ]
    loop [start] <| Set.singleton start
(*[/omit]*)
// [/snippet]


// [snippet: (*) Problem 82: Cycle from a given node]
/// Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a given node A in the graph G. The 
/// predicate should return all cycles via backtracking.
/// 
/// Example:
/// 
/// <example in lisp>
/// Example in F#:
/// 
/// > cycle 2 [(1,[2;3]);(2,[3]);(3,[4]);(4,[2]);(5,[6]);(6,[5])];;
/// val it : int list list = [[2; 3; 4; 2]]
///
/// > cycle 1 [(1,[2;3]);(2,[3]);(3,[4]);(4,[2]);(5,[6]);(6,[5])];;
/// val it : int list list = []

(*[omit(Solution)]*)
let cycle start (g: 'a AdjacencyGraph) = 
    let map = g |> Map.ofList
    let rec loop route visited = [
        let current = List.head route
        for next in map.[current] do
            if next = start then
                yield List.rev <| next::route
            if visited |> Set.contains next |> not then
                yield! loop (next::route) (Set.add next visited) 
    ]
    loop [start] <| Set.singleton start
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 83: Construct all spanning trees]
/// Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees of a given graph. With 
/// this predicate, find out how many spanning trees there are for the graph depicted to the left. The data of this 
/// example graph can be found in the file p83.dat. When you have a correct solution for the s_tree/2 predicate, use 
/// it to define two other useful predicates: is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!
/// 
/// Example:
/// 
/// <example in lisp>
/// Example in F#:

(*[omit(Solution needed)]*)
let solution83 = "your solution here!!"
// this is not a solution. This is still a work in progress.
type Color = White = 0 | Gray = 1 | Black = 2

let s_tree (g : 'a AdjacencyGraph) = 
    // The algorithm comes from the book Introduction to Algorithms by Cormen, Leiserson, Rivest and Stein.
    let depthFirstSearch (g : 'a AdjacencyGraph) start : 'a AdjacencyGraph = 
        let nodes = g |> Map.ofList
        let color = g |> List.map(fun (v,_) -> v, Color.White) |> Map.ofList |> ref
        let pi = g |> List.map(fun (v,_) -> v, []) |> Map.ofList |> ref

        let rec dfs u = 
            color := Map.add u Color.Gray !color
            for v in nodes.[u] do
                if (!color).[v] = Color.White then
                    let ns = (!pi).[u]
                    pi := Map.add u (v::ns) !pi
                    dfs v
            color := Map.add u Color.Black !color

        dfs start
        !pi |> Map.toList
            
    g |> List.map fst |> List.map(depthFirstSearch g)
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 84: Construct the minimal spanning tree]
/// Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal spanning tree of a given labelled
/// graph. Hint: Use the algorithm of Prim. A small modification of the solution of P83 does the trick. The data of 
/// the example graph to the right can be found in the file p84.dat.
/// 
/// Example:
/// 
/// <example in lisp>
/// 
/// Example in F#: 
/// > let graphW = [('a',['b'; 'd';]); ('b',['a';'c';'d';'e';]); ('c',['b';'e';]); ('d',['a';'b';'e';'f';]); 
///               ('e',['b';'c';'d';'f';'g';]); ('f',['d';'e';'g';]); ('g',['e';'f';]); ];;
/// > let gwF = 
///     let weigthMap = Map [(('a','b'), 7);(('a','d'), 5);(('b','a'), 7);(('b','c'), 8);(('b','d'), 9);
///                          (('b','e'), 7);(('c','b'), 8);(('c','e'), 5);(('d','a'), 5);(('d','b'), 9);
///                          (('d','e'), 15);(('d','f'), 6);(('e','b'), 7);(('e','c'), 5);(('e','d'), 15);
///                          (('e','f'), 8);(('e','g'), 9);(('f','d'), 6);(('f','e'), 8);(('f','g'), 11);
///                          (('g','e'), 9);(('g','f'), 11);]
///     fun (a,b) -> weigthMap.[(a,b)];;
/// 
/// val graphW : (char * char list) list =
///   [('a', ['b'; 'd']); ('b', ['a'; 'c'; 'd'; 'e']); ('c', ['b'; 'e']);
///    ('d', ['a'; 'b'; 'e'; 'f']); ('e', ['b'; 'c'; 'd'; 'f'; 'g']);
///    ('f', ['d'; 'e'; 'g']); ('g', ['e'; 'f'])]
/// val gwF : (char * char -> int)
/// 
/// > prim gw gwF;;
/// val it : char Graph =
///   (['a'; 'd'; 'f'; 'b'; 'e'; 'c'; 'g'],
///    [('a', 'd'); ('d', 'f'); ('a', 'b'); ('b', 'e'); ('e', 'c'); ('e', 'g')])
/// 

(*[omit(Solution)]*)
let prim (s : 'a AdjacencyGraph) (weightFunction: ('a Edge -> int)) : 'a Graph = 
    let map = s |> List.map (fun (n,ln) -> n, ln |> List.map(fun m -> ((n,m),weightFunction (n,m)))) |> Map.ofList
    let nodes = s |> List.map fst
    let emptyGraph = ([],[])

    let rec dfs nodes (ns,es) current visited = 
        if nodes |> Set.isEmpty then
            (List.rev ns, List.rev es)
        else
                let (a,b) as edge = ns 
                                    |> List.collect(fun n -> map.[n] 
                                                             |> List.filter(fun ((n,m),w) -> Set.contains m visited |> not) ) 
                                    |> List.minBy snd |> fst
                let nodes' = nodes |> Set.remove b
                dfs nodes' (b::ns,edge::es) b (Set.add b visited)
    match nodes with
        | [] -> emptyGraph
        | n::ns -> dfs (Set ns) ([n],[]) n (Set.singleton n) 
    
(*[/omit]*)
// [/snippet]


// [snippet: (**) Problem 85: Graph isomorphism]
/// Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection f: N1 -> N2 such that for any 
/// nodes X,Y of N1, X and Y are adjacent if and only if f(X) and f(Y) are adjacent.
/// 
/// Write a predicate that determines whether two graphs are isomorphic. Hint: Use an open-ended list to 
/// represent the function f.
/// 
/// Example:
/// 
/// <example in lisp>
/// 
/// Example in F#: 

(*[omit(Solution needed)]*)
let solution85 = "your solution here!!"
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 86: Node degree and graph coloration]
/// a) Write a predicate degree(Graph,Node,Deg) that determines the degree of a given node.
/// 
/// b) Write a predicate that generates a list of all nodes of a graph sorted according to decreasing degree.
/// 
/// c) Use Welch-Powell's algorithm to paint the nodes of a graph in such a way that adjacent nodes have 
/// different colors.
/// 
/// 
/// Example:
/// 
/// <example in lisp>
///
/// Example in F#: 
/// > let graph = [('a',[]);('b',['c']);('c',['b';'d';'g']);('d',['c';'e']);('e',['d';'e';'f';'g']);('f',['e';'g']);('g',['c';'e';'f'])];;
/// > degree graph 'e';;
/// val it : int = 5
/// > sortByDegree graph;;
/// val it : char Node list =
///   [ ('e',['d'; 'e'; 'f'; 'g']);  ('g',['c'; 'e'; 'f']);
///     ('c',['b'; 'd'; 'g']);  ('f',['e'; 'g']);  ('d',['c'; 'e']);
///     ('b',['c']);  ('a',[])]
/// val it : int = 5
/// > colorGraph graph;;
/// val it : (char * int) list =
///   [('a', 0); ('b', 1); ('c', 0); ('d', 1); ('e', 0); ('f', 2); ('g', 1)]

(*[omit(Solution)]*)

let degree (g: 'a AdjacencyGraph) node = 
    let es = g |> List.find(fst >> (=) node) |> snd
    // The degree of a node is the number of edges that go to the node. 
    // Loops get counted twice.
    es |> List.sumBy(fun n -> if n = node then 2 else 1)

let sortByDegreeDesc (g : 'a AdjacencyGraph) = 
    // let use this degree function instead of the one above
    // since we alredy have all the info we need right here.
    let degree (u,adj) = adj |> List.sumBy(fun v -> if v = u then 2 else 1)
    g |> List.sortBy(degree) |> List.rev

let colorGraph g = 
    let nodes = sortByDegreeDesc g
    let findColor usedColors = 
        let colors = Seq.initInfinite id
        colors |> Seq.find(fun c -> Set.contains c usedColors |> not)
    let rec greedy colorMap nodes =
        match nodes with
            | [] -> colorMap |> Map.toList
            | (n,ns)::nodes -> 
                let usedColors = ns |> List.filter(fun n -> Map.containsKey n colorMap) |> List.map(fun n -> Map.find n colorMap ) |> Set.ofList
                let color = findColor usedColors
                greedy (Map.add n color colorMap) nodes
                
    greedy Map.empty nodes

(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 87: Depth-first order graph traversal (alternative solution)]
/// Write a predicate that generates a depth-first order graph traversal sequence. The starting point should be 
/// specified, and the output should be a list of nodes that are reachable from this starting point (in depth-first
/// order).
/// 
/// Example:
/// 
/// <example in lisp>
/// 
/// Example in F#: 
///
/// > let gdfo = (['a';'b';'c';'d';'e';'f';'g';], [('a','b');('a','c');('a','e');('b','d');('b','f');('c','g');('e','f');]) |> Graph2AdjacencyGraph;;
/// 
/// val gdfo : char AdjacencyGraph =
///   [('a', ['e'; 'c'; 'b']); ('b', ['f'; 'd'; 'a']); ('c', ['g'; 'a']);
///    ('d', ['b']); ('e', ['f'; 'a']); ('f', ['e'; 'b']); ('g', ['c'])]
/// 
/// > depthFirstOrder gdfo 'a';;
/// val it : char list = ['a'; 'e'; 'f'; 'b'; 'd'; 'c'; 'g']

(*[omit(Solution)]*)

// The enum Color is defined on problem 83
// The algorithm comes from the book Introduction to Algorithms by Cormen, Leiserson, Rivest and Stein.
let depthFirstOrder (g : 'a AdjacencyGraph) start = 
    let nodes = g |> Map.ofList
    let color = g |> List.map(fun (v,_) -> v, Color.White) |> Map.ofList |> ref
    let pi = ref [start]

    let rec dfs u = 
        color := Map.add u Color.Gray !color
        for v in nodes.[u] do
            if (!color).[v] = Color.White then
                pi := (v::!pi)
                dfs v
        color := Map.add u Color.Black !color

    dfs start
    !pi |> List.rev

(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 88: Connected components (alternative solution)]
/// Write a predicate that splits a graph into its connected components.
/// 
/// Example:
/// 
/// <example in lisp>
/// 
/// Example in F#: 
/// > let graph = [(1,[2;3]);(2,[1;3]);(3,[1;2]);(4,[5;6]);(5,[4]);(6,[4])];;
/// > connectedComponents graph;;
/// val it : int AdjacencyGraph list =
///   [[(6, [4]); (5, [4]); (4, [5; 6])];
///    [(3, [1; 2]); (2, [1; 3]); (1, [2; 3])]]
/// > 

(*[omit(Solution)]*)
// using problem 87 depthFirstOrder function
let connectedComponents (g : 'a AdjacencyGraph) =
    let nodes = g |> List.map fst |> Set.ofList
    let start = g |> List.head |> fst
    let rec loop acc g start nodes = 
        let dfst = depthFirstOrder g start |> Set.ofList
        let nodes' = Set.difference nodes dfst 
        if Set.isEmpty nodes' then
            g::acc
        else
            // once we have the dfst set we can remove those nodes from the graph and
            // add them to the solution and continue with the remaining nodes
            let (cg,g') = g |> List.fold(fun (xs,ys) v -> if Set.contains (fst v) dfst then (v::xs,ys) else (xs,v::ys)) ([],[])
            let start' = List.head g' |> fst
            loop (cg::acc) g' start' nodes'
    loop [] g start nodes
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 89: Bipartite graphs]
/// Write a predicate that finds out whether a given graph is bipartite.
/// 
/// Example:
/// 
/// <example in lisp>
/// 
/// Example in F#: 
///
/// > let gdfo = [('a', ['b'; 'c'; 'e']); ('b', ['a'; 'd'; 'f']); ('c', ['a'; 'g']);('d', ['b']); 
///               ('e', ['a'; 'f']); ('f', ['b'; 'e']); ('g', ['c'])];;
/// 
/// val gdfo : (char * char list) list =
///   [('a', ['b'; 'c'; 'e']); ('b', ['a'; 'd'; 'f']); ('c', ['a'; 'g']);
///    ('d', ['b']); ('e', ['a'; 'f']); ('f', ['b'; 'e']); ('g', ['c'])]
/// 
/// > isBipartite gdfo;;
/// val it : bool = true

(*[omit(Solution)]*)
open System.Collections.Generic; // this is where Queue<'T> is defined

let isBipartite (g : 'a AdjacencyGraph) = 
    // using the breath-first search algorithm, we can compute the distances
    // from the first node to the other the nodes. If all the even distance nodes
    // point to odd nodes and viceversa, then the graph is bipartite. This works
    // for connected graphs.
    // The algorithm comes from the book Introduction to Algorithms by Cormen, Leiserson, Rivest and Stein.
    let isBipartite' (g : 'a AdjacencyGraph) = 
        let adj = g |> Map.ofList
        // The Color enum is defined on problem 83
        let mutable color = g |> List.map(fun (v,_) -> v, Color.White) |> Map.ofList
        let mutable distances = g |> List.map(fun (v,_) -> v,-1) |> Map.ofList
        let queue = new Queue<_>()
        let start = List.head g |> fst
        color <- Map.add start Color.Gray color
        distances <- Map.add start 0 distances
        queue.Enqueue(start)
        while queue.Count <> 0 do
            let u = queue.Peek()
            for v in adj.[u] do
                if color.[v] = Color.White then
                    color <- Map.add v Color.Gray color
                    distances <- Map.add v (distances.[u] + 1) distances
                    queue.Enqueue(v)
            queue.Dequeue() |> ignore
            color <- Map.add u Color.Black color
        let isEven n = n % 2 = 0
        let isOdd = isEven >> not
        let d = distances // this is just so distances can be captured in the closure below.
        g |> List.forall(fun (v,edges) -> 
                            let isOpposite = if d.[v] |> isEven then isOdd else isEven
                            edges |> List.forall(fun e -> d.[e] |> isOpposite))

    // split the graph in it's connected components (problem 88) and test each piece for bipartiteness.
    // if all the pieces are bipartite, the graph is bipartite.
    g |> connectedComponents |> List.forall isBipartite'
(*[/omit]*)
// [/snippet]
