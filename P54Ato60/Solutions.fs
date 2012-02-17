// [snippet: Ninety-Nine F# Problems - Problems 54 - 60 - Binary trees]
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
///  
/// 
/// A binary tree is either empty or it is composed of a root element and two successors, 
/// which are binary trees themselves.
/// 
///                              (a)
///                             /   \
///                            (b)  (c)
///                           /  \    \
///                         (d)  (e)  (f)
///
/// In F#, we can characterize binary trees with a type definition: 
/// 

type 'a Tree = Empty | Branch of 'a * 'a Tree * 'a Tree

///
/// This says that a Tree of type a consists of either an Empty node, or a Branch containing one 
/// value of type a with exactly two subtrees of type a.
///  
/// Given this definition, the tree in the diagram above would be represented as: 
/// 

let tree1 = Branch ('a', Branch ('b', Branch ('d', Empty, Empty),
                               Branch ('e', Empty, Empty)),
                         Branch ('c', Empty,
                               Branch ('f', Branch ('g', Empty, Empty),
                                           Empty))) 

/// Since a "leaf" node is a branch with two empty subtrees, it can be useful to define a 
/// shorthand function:

let leaf x = Branch (x, Empty, Empty) 

/// Then the tree diagram above could be expressed more simply as: 

let tree1' = Branch ('a', Branch ('b', leaf 'd',
                               leaf 'e'),
                          Branch ('c', Empty,
                               Branch ('f', leaf 'g',
                                           Empty)))
/// Other examples of binary trees: 
/// 
/// -- A binary tree consisting of a root node only
let tree2 = Branch ('a', Empty, Empty)
///  
/// -- An empty binary tree
let tree3 = Empty
///  
/// -- A tree of integers
let tree4 = Branch (1, Branch (2, Empty, Branch (4, Empty, Empty)),
                       Branch (2, Empty, Empty))
// [/snippet]

// [snippet: (*) Problem 54A : Check whether a given term represents a binary tree.]
/// In Prolog or Lisp, one writes a predicate to do this. 
/// 
/// Example in Lisp: 
/// * (istree (a (b nil nil) nil))
/// T
/// * (istree (a (b nil nil)))
/// NIL
///  
/// Non-solution: 
/// F#'s type system ensures that all terms of type 'a Tree are binary trees: it is just not 
//  possible to construct an invalid tree with this type. Hence, it is redundant to introduce 
/// a predicate to check this property: it would always return True
// [/snippet]

// [snippet: (**) Problem 55 : Construct completely balanced binary trees]
/// In a completely balanced binary tree, the following property holds for every node: 
/// The number of nodes in its left subtree and the number of nodes in its right subtree 
/// are almost equal, which means their difference is not greater than one.
///  
/// Write a function cbal-tree to construct completely balanced binary trees for a given 
/// number of nodes. The predicate should generate all solutions via backtracking. Put 
/// the letter 'x' as information into all nodes of the tree.
///  
/// Example: 
/// * cbal-tree(4,T).
/// T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
/// T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
/// etc......No
///  
/// Example in F#, whitespace and "comment diagrams" added for clarity and exposition:
///  
/// > cbalTree 4;;
/// val trees : char Tree list =
/// [
///    permutation 1
///        x
///       / \
///      x   x
///           \
///            x
/// Branch ('x', Branch ('x', Empty, Empty),
///              Branch ('x', Empty,
///                        Branch ('x', Empty, Empty)));
///  
///    permutation 2
///        x
///       / \
///      x   x
///         /
///        x
/// Branch ('x', Branch ('x', Empty, Empty),
///              Branch ('x', Branch ('x', Empty, Empty),
///                        Empty));
///  
///    permutation 3
///        x
///       / \
///      x   x
///       \
///        x
/// Branch ('x', Branch ('x', Empty, 
///                           Branch ('x', Empty, Empty)),
///              Branch ('x', Empty, Empty));
///  
///    permutation 4
///        x
///       / \
///      x   x
///     /
///    x
/// Branch ('x', Branch ('x', Branch ('x', Empty, Empty),
///                        Empty), 
///              Branch ('x', Empty, Empty))
/// ]

(*[omit:(Solution 1)]*)
let rec cbalTree n =
    match n with
        | 0 -> [Empty]
        | n -> let q,r = let x = n - 1 in x / 2, x % 2 
               [ for i=q to q + r do
                    for lt in cbalTree i do
                       for rt in cbalTree (n - 1 - i) do
                          yield Branch('x', lt, rt) ]
(*[/omit]*)

(*[omit:(Solution 2)]*)
let nodes t = 
    let rec nodes' t cont = 
        match t with
            | Empty -> cont 0
            | Branch(_, lt, rt) -> 
                nodes' lt (fun nlt -> nodes' rt (fun nrt -> cont (1 + nlt + nrt)))
    nodes' t id

let rec allTrees n =
    match n with
        | 0 -> [Empty]
        | n ->
            [ for i=0 to n - 1 do
                 for lt in cbalTree i do
                    for rt in cbalTree (n - 1 - i) do
                       yield Branch('x', lt, rt) ]

let cbalTree' n = allTrees n |> List.filter(fun t -> 
                                                match t with
                                                    | Empty -> true
                                                    | Branch(_, lt, rt) -> abs (nodes lt - nodes rt) <= 1 )
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 56 : Symmetric binary trees]
/// Let us call a binary tree symmetric if you can draw a vertical line through the root 
/// node and then the right subtree is the mirror image of the left subtree. Write a 
/// predicate symmetric/1 to check whether a given binary tree is symmetric. Hint: Write 
/// a predicate mirror/2 first to check whether one tree is the mirror image of another. 
/// We are only interested in the structure, not in the contents of the nodes.
///  
/// Example in F#: 
/// 
/// > symmetric <| Branch ('x', Branch ('x', Empty, Empty), Empty);;
/// val it : bool = false
/// > symmetric <| Branch ('x', Branch ('x', Empty, Empty), Branch ('x', Empty, Empty))
/// val it : bool = true

(*[omit:(Solution)]*)
let symmetric tree =
    let rec mirror t1 t2 cont =
        match t1,t2 with
            | Empty,Empty -> cont true
            | Empty, Branch _ -> cont false
            | Branch _, Empty -> cont false
            | Branch (_, lt1, rt1), Branch (_, lt2, rt2) -> 
                mirror lt1 rt2 (fun isMirrorLeft -> mirror rt1 lt2 (fun isMirrorRight -> cont (isMirrorLeft && isMirrorRight)))
    match tree with
        | Empty -> true
        | Branch (_,lt, rt) -> mirror lt rt id
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 57 : Binary search trees (dictionaries)]
/// Use the predicate add/3, developed in chapter 4 of the course, to write a predicate 
/// to construct a binary search tree from a list of integer numbers.
///  
/// Example: 
/// * construct([3,2,5,7,1],T).
/// T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
///  
/// Then use this predicate to test the solution of the problem P56. 
/// 
/// Example: 
/// * test-symmetric([5,3,18,1,4,12,21]).
/// Yes
/// * test-symmetric([3,2,5,7,4]).
/// No
///  
/// Example in F#: 
/// 
/// > construct [3; 2; 5; 7; 1]
/// val it : int Tree =
///   Branch (3,Branch (2,Branch (1,Empty,Empty),Empty),
///             Branch (5,Empty,Branch (7,Empty,Empty)))
/// > [5; 3; 18; 1; 4; 12; 21] |> construct |> symmetric;;
/// val it : bool = true
/// > [3; 2; 5; 7; 1] |> construct |> symmetric;;
/// val it : bool = true

(*[omit:(Solution)]*)
let insert x tree = 
    let rec insert' t cont =
        match t with
            | Empty -> cont <| Branch(x, Empty, Empty)
            | Branch(y, lt, rt) as t ->
                if x < y then
                    insert' lt <| fun lt' -> cont <| Branch(y, lt', rt)
                elif x > y then
                    insert' rt <| fun rt' -> cont <| Branch(y, lt, rt')
                else
                    t
    insert' tree id

let construct xs = xs |> List.fold(fun tree x -> insert x tree) Empty
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 58 : Generate-and-test paradigm]
/// Apply the generate-and-test paradigm to construct all symmetric, completely balanced 
/// binary trees with a given number of nodes.
///  
/// Example: 
/// * sym-cbal-trees(5,Ts).
/// Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), 
///       t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
///  
/// Example in F#: 
/// 
/// > symCbalTrees 5;;
/// val it : char Tree list =
///   [Branch
///      ('x',Branch ('x',Empty,Branch ('x',Empty,Empty)),
///       Branch ('x',Branch ('x',Empty,Empty),Empty));
///    Branch
///      ('x',Branch ('x',Branch ('x',Empty,Empty),Empty),
///       Branch ('x',Empty,Branch ('x',Empty,Empty)))]

(*[omit:(Solution)]*)
let symCbalTrees = cbalTree >> List.filter symmetric
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 59 : Construct height-balanced binary trees]
/// In a height-balanced binary tree, the following property holds for every node: The 
/// height of its left subtree and the height of its right subtree are almost equal, 
/// which means their difference is not greater than one.
///  
/// Example: 
/// ?- hbal_tree(3,T).
/// T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
/// T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
/// etc......No
///  
/// Example in F#: 
/// 
/// > hbalTree 'x' 3 |> Seq.take 4;;
/// val it : seq<char Tree> =
///   seq
///     [Branch
///        ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
///         Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)));
///      Branch
///        ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
///         Branch ('x',Branch ('x',Empty,Empty),Empty));
///      Branch
///        ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
///         Branch ('x',Empty,Branch ('x',Empty,Empty)));
///      Branch
///        ('x',Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty)),
///         Branch ('x',Empty,Empty))]

(*[omit:(Solution)]*)
let hbalTree a height =
    let rec loop h cont = 
        match h with
            | 0 -> cont [Empty, 0]
            | 1 -> cont [Branch (a, Empty, Empty), 1]
            | _ -> loop (h-1) (fun lts ->
                       loop (h-2) (fun rts -> 
                       cont <| [let t = lts @ rts 
                                for (t1,h1) in t do
                                    for (t2,h2) in t do
                                        let ht = 1 + max h1 h2 
                                        if ht = h then
                                            yield Branch (a, t1, t2), ht] ))
    loop height id |> List.map fst
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 60 : Construct height-balanced binary trees with a given number of nodes]
/// Consider a height-balanced binary tree of height H. What is the maximum number of nodes 
/// it can contain?
/// Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question is more 
/// difficult. Try to find a recursive statement and turn it into a function minNodes that 
/// returns the minimum number of nodes in a height-balanced binary tree of height H. On the 
/// other hand, we might ask: what is the maximum height H a height-balanced binary tree with 
/// N nodes can have? Write a function maxHeight that computes this. 
///
/// Now, we can attack the main problem: construct all the height-balanced binary trees with a 
/// given nuber of nodes. Find out how many height-balanced trees exist for N = 15.
///  
/// Example in Prolog: 
/// ?- count_hbal_trees(15,C).
/// C = 1553
///  
/// Example in F#: 
/// 
/// > hbalTreeNodes 'x' 15 |> List.length;;
/// val it : int = 1553
/// > [0 .. 3] |> List.map (hbalTreeNodes 'x');;
/// val it : char Tree list list =
///   [[Empty]; [Branch ('x',Empty,Empty)];
///    [Branch ('x',Branch ('x',Empty,Empty),Empty);
///     Branch ('x',Empty,Branch ('x',Empty,Empty))];
///    [Branch ('x',Branch ('x',Empty,Empty),Branch ('x',Empty,Empty))]]

(*[omit:(Solution)]*)
let minNodes height = 
    let rec minNodes' h cont =
        match h with
        | 0 -> cont 0
        | 1 -> cont 1
        | _ -> minNodes' (h - 1) <| fun h1 -> minNodes' (h - 2) <| fun h2 -> cont <| 1 + h1 + h2
    minNodes' height id

let maxHeight nodes = 
    let rec loop n acc =
        match n with
            | 0 -> acc
            | _ -> loop (n >>> 1) (acc + 1)
    let fullHeight = loop nodes 0 // this is the height of a tree with full nodes
    let minNodesH1 = minNodes (fullHeight + 1)
    if nodes < minNodesH1 then
        fullHeight
    else
        fullHeight + 1

let numNodes tree = 
    let rec numNodes' tree cont =
        match tree with
            | Empty -> cont 0
            | Branch(_, lt , rt) ->
                numNodes' lt <| fun ln -> numNodes' rt <| fun rn -> cont <| 1 + ln + rn
    numNodes' tree id

let hbalTreeNodes x nodes = 
    let maxH = maxHeight nodes
    let minH = if maxH = 0 then 0 else maxH - 1
    [minH .. maxH] |> List.collect(fun n -> hbalTree x n) |> List.filter(fun t -> nodes = numNodes t)
(*[/omit]*)
// [/snippet]