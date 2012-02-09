// [snippet: Ninety-Nine F# Problems - Problems 70 - 73 - Multiway Trees]
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
/// A multiway tree is composed of a root element and a (possibly empty) set of successors which are 
/// multiway trees themselves. A multiway tree is never empty. The set of successor trees is sometimes 
/// called a forest.
///  
///                              (a)
///                            /  |  \
///                          (f) (c) (b)
///                           |      /  \
///                          (g)   (d)  (e)
/// 
/// Problem 70B 
/// 
/// (*) Check whether a given term represents a multiway tree. 
/// 
/// In Prolog or Lisp, one writes a predicate to check this. 
/// 
/// Example in Prolog: 
/// ?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
/// Yes
///  
/// In F#, we define multiway trees as a type.
/// 
type 'a Tree = Node of 'a  * 'a Tree list
///         
/// Some example trees: 
/// 
let tree1 = Node ('a', [])

let tree2 = Node ('a', [Node ('b', [])])

let tree3 = Node ('a', [Node ('b', [Node ('c', [])])])

let tree4 = Node ('b', [Node ('d', []); Node ('e', [])])

let tree5 = Node ('a', [
                        Node ('f', [Node ('g', [])]);
                        Node ('c', []);
                        Node ('b', [Node ('d', []); Node ('e', [])])
                       ] )

/// The last is the tree illustrated above. 
/// 
/// 
/// (*) Problem 70B : Check whether a given term represents a multiway tree
/// As in problem 54A, all members of this type are multiway trees; there is no use for a predicate to 
/// test them.
/// 
// [/snippet]

// [snippet: (*) Problem 70C : Count the nodes of a multiway tree.]
/// Example in F#: 
/// 
/// > nnodes tree2;;
/// val it : int = 2 

(*[omit:(Solution)]*)
let rec nnodes = function
    | Node (_, []) -> 1
    | Node (_, xs) -> 
        let t = xs |> List.sumBy (nnodes)
        1 + t
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 70 : Tree construction from a node string.]
/// We suppose that the nodes of a multiway tree contain single characters. In the depth-first 
/// order sequence of its nodes, a special character ^ has been inserted whenever, during the 
/// tree traversal, the move is a backtrack to the previous level.
///  
/// By this rule, the tree below (tree5) is represented as: afg^^c^bd^e^^^ 
/// 
/// 
/// 
/// Define the syntax of the string and write a predicate tree(String,Tree) to construct the 
/// Tree when the String is given. Make your predicate work in both directions.
///
/// Example in F#: 
/// 
/// > string2Tree "afg^^c^bd^e^^^";;
/// val it : char Tree =
///   Node
///     ('a',
///      [Node ('f',[Node ('g',[])]); Node ('c',[]);
///       Node ('b',[Node ('d',[]); Node ('e',[])])])
/// > string2Tree "afg^^c^bd^e^^^" = tree5;;
/// val it : bool = true

(*[omit:(Solution)]*)
let string2Tree str =
    let chars = str |> List.ofSeq
    let rec loop chars stack =
        match chars with
            | '^'::xs ->
                match stack with
                    | [x] -> x
                    | tx::Node(y, ty)::stack' -> loop xs (Node(y, ty @ [tx])::stack')
                    | [] -> failwith "malformed text"
            | x::xs -> loop xs (Node(x,[])::stack)
            | [] -> failwith "malformed text"
    loop chars [] 

let tree2String tree =
    let rec loop tree =
        match tree with
            | Node(a, []) -> a.ToString() +  "^"
            | Node(a, xs) -> a.ToString() + (xs |> List.fold(fun acc x -> acc + loop x) "")  + "^"
    loop tree
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 71 : Determine the internal path length of a tree.]
/// We define the internal path length of a multiway tree as the total sum of the path lengths from the 
/// root to all nodes of the tree. By this definition, tree5 has an internal path length of 9.
///  
/// Example in F#: 
/// 
/// > ipl tree5;;
/// val it : int = 9
/// > ipl tree4;;
/// val it : int = 2

(*[omit:(Solution)]*)
let rec ipl tree = 
    let rec loop depth = function
        | Node(a, []) -> depth
        | Node(a, xs) -> depth + (xs |> List.sumBy( fun x -> loop (depth+1) x))
    loop 0 tree 
(*[/omit]*)
// [/snippet]
    
// [snippet: (*) Problem 72 : Construct the bottom-up order sequence of the tree nodes.]
/// Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the nodes of the 
/// multiway tree Tree.
///  
/// Example in F#: 
/// 
/// > bottom_up tree5;;
/// val it : string = "gfcdeba"
/// > bottom_up tree4;;
/// val it : string = "deb"

(*[omit:(Solution)]*)
let bottom_up tree = 
    let rec loop = function
        | Node(a, []) -> a.ToString()
        | Node(a, xs) -> (xs |> List.fold( fun acc x -> acc + (loop x) ) "") + a.ToString()
    loop tree 
(*[/omit]*)
// [/snippet]
 
// [snippet: (**) Problem 73 : Lisp-like tree representation.]
/// There is a particular notation for multiway trees in Lisp. Lisp is a prominent 
/// functional programming language, which is used primarily for artificial intelligence 
/// problems. As such it is one of the main competitors of Prolog. In Lisp almost everything
/// is a list, just as in Prolog everything is a term.
///  
/// The following pictures show how multiway tree structures are represented in Lisp.
///  
///    (a)        (a)        (a)        (b)            (a)
///                |          |        /   \         /  |  \
///               (b)        (b)     (d)   (e)     (f)  (c)  (b)
///                           |                     |       /   \
///                          (c)                   (g)    (d)   (e)
///
///     a        (a b)    (a (b c))   (b d e)    (a (f g) c (b d e))
///
/// Note that in the "lispy" notation a node with successors (children) in the tree is always the first
/// element in a list, followed by its children. The "lispy" representation of a multiway tree is a sequence
/// of atoms and parentheses '(' and ')', which we shall collectively call "tokens". We can represent this
/// sequence of tokens as a Prolog list; e.g. the lispy expression (a (b c)) could be represented as the 
/// Prolog list ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the
/// "lispy token list"
/// LTL if the tree is given as term T in the usual Prolog notation.
///  
/// (The Prolog example given is incorrect.) 
/// 
/// Example in F#: 
/// 
/// > treeltl "(x (a (f g) c (b d e)))";;
/// val it : char list =
///   ['('; 'x'; '('; 'a'; '('; 'f'; 'g'; ')'; 'c'; '('; 'b'; 'd'; 'e'; ')'; ')';
///    ')']
///
/// > displayList tree1;;
/// val it : string = "a"
/// > displayLisp tree2;;
/// val it : string = "(a b)"
/// > displayLisp tree3;;
/// val it : string = "(a (b c))"
/// > displayLisp tree4;;
/// val it : string = "(b d e)"
/// > displayLisp tree5;;
/// val it : string = "(a (f g) c (b d e))"
///
/// > lisp2Tree "(a (f g) c (b d e))" = tree5;;
/// val it : bool = true
///
/// As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that the inverse
/// conversion is also possible.

(*[omit:(Solution)]*)
let treeltl str = str |> List.ofSeq |> List.filter((<>) ' ')

let displayLisp tree = 
    let rec loop = function
        | Node(a, []) -> a.ToString()
        | Node(a, xs) -> "(" + a.ToString() + (xs |> List.fold( fun acc x -> acc + " " + (loop x) ) "") + ")"
    loop tree 

let lisp2Tree str = 
    let tokens = treeltl str
    let rec loop tokens stack =
        match tokens with
            | ')'::xs ->
                match stack with
                    | [x] -> x
                    | tx::Node(y, ty)::stack' -> loop xs (Node(y, ty @ [tx])::stack')
                    | [] -> failwith "malformed text"
            | '('::x::xs -> loop xs (Node(x,[])::stack)
            | x::xs -> 
                match stack with
                    | [] -> loop xs [Node(x,[])]
                    | Node(y,t)::stack -> loop xs (Node(y,t @  [Node(x,[])])::stack)
            | [] -> stack |> List.head
    loop tokens []
(*[/omit]*)
// [/snippet]