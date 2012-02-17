// [snippet: Ninety-Nine F# Problems - Problems 46 - 50 - Logic and Codes]
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

// [snippet: (**) Problem 46 : Define Logical predicates]
/// Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical 
/// equivalence) which succeed or fail according to the result of their respective 
/// operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
///  
/// A logical expression in two variables can then be written as in the following example: 
/// and(or(A,B),nand(A,B)).
///  
/// Now, write a predicate table/3 which prints the truth table of a given logical 
/// expression in two variables.
///  
/// Example: 
/// (table A B (and A (or A B)))
/// true true true
/// true fail true
/// fail true fail
/// fail fail fail
///  
/// Example in F#: 
/// 
/// > table (fun a b -> (and' a (or' a b)));;
/// true true true
/// true false true
/// false true false
/// false false false
/// val it : unit = ()

(*[omit:(Solution)]*)
let and' = (&&)

let or'  = (||)

let nand a b = not <| and' a b

let nor a b = not <| or' a b

let xor a b = if a <> b then true else false

let impl a b = compare a b |> (<>) 1

let eq = (=)

let table expr = 
    let inputs = [ (true, true); (true, false); (false, true); (false, false) ]
    inputs |> Seq.iter (fun (a,b) -> printfn "%b %b %b" a b (expr a b))
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 47 : Truth tables for logical expressions (2).]
/// Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write 
/// the logical expression in the more natural way, as in the example: A and (A or not B). 
/// Define operator precedence as usual; i.e. as in Java.
///  
/// Example: 
/// * (table A B (A and (A or not B)))
/// true true true
/// true fail true
/// fail true fail
/// fail fail fail
///  
/// Example in F#: 
/// 
/// > table2 (fun a b -> a && (a || not b));;
/// true true true
/// true false true
/// false true false
/// false false false
/// val it : unit = ()

(*[omit:(Solution)]*)
// let's use the F# built-in operateros plus:

// xor
let (&|) a b = if a <> b then true else false

// nand
let (^&&) a b = not <| a && b

// nor
let (^||) a b = not <| a || b

// impl
let (|->) a b = compare a b |> (<>) 1

// same as problem 46
let table2 expr = 
    let inputs = [ (true, true); (true, false); (false, true); (false, false) ]
    inputs |> Seq.iter (fun (a,b) -> printfn "%b %b %b" a b (expr a b))
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 48 : Truth tables for logical expressions (3).]
/// Generalize problem P47 in such a way that the logical expression may contain any 
/// number of logical variables. Define table/2 in a way that table(List,Expr) prints the 
/// truth table for the expression Expr, which contains the logical variables enumerated 
/// in List.
///  
/// Example: 
/// * (table (A,B,C) (A and (B or C) equ A and B or A and C))
/// true true true true
/// true true fail true
/// true fail true true
/// true fail fail true
/// fail true true true
/// fail true fail true
/// fail fail true true
/// fail fail fail true
///  
/// Example in F#: 
/// 
/// > tablen 3 (fun [a;b;c] -> a && (b || c) = a && b || a && c)
/// warning FS0025: Incomplete pattern matches on this expression. ...
/// True True True true
/// False True True false
/// True False True true
/// False False True false
/// True True False true
/// False True False false
/// True False False false
/// False False False false
/// val it : unit = ()

(*[omit:(Solution)]*)
let tablen n expr =
    let replicate n xs = 
        let rec repl acc n =
            match n with
                | 0 -> acc
                | n -> 
                    let acc' = acc |> List.collect(fun ys -> xs |> List.map(fun x -> x::ys))
                    repl acc' (n-1)
        repl [[]] n 
 
    let values = replicate n [true; false]
    let toString bs = System.String.Join(" ", Array.ofList (bs |> List.map string))
    values |> Seq.iter(fun bs -> printfn "%s %b" (bs |> toString) (expr bs))
(*[/omit]*)
// [/snippet]


// [snippet: (**) Problem 49 : Gray codes.]
/// An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.
/// For example,
///
/// n = 1: C(1) = ['0','1'].
/// n = 2: C(2) = ['00','01','11','10'].
/// n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
///  
/// Find out the construction rules and write a predicate with the following specification:
///  % gray(N,C) :- C is the N-bit Gray code
///  
/// Can you apply the method of "result caching" in order to make the predicate more efficient, 
/// when it is to be used repeatedly?
///  
/// Example in F#: 
/// 
/// P49> gray 3
/// ["000","001","011","010","110","111","101","100"]

(*[omit:(Solution)]*)
// The rules to contruct gray codes can be found here : http://en.wikipedia.org/wiki/Gray_code
let rec gray = function
    | 0 -> [""]
    | n -> 
        let prev = gray (n - 1)
        (prev |> List.map ((+) "0")) @ (prev |> List.rev |> List.map((+) "1"))
(*[/omit]*)
// [/snippet]
   
// [snippet: (***) Problem 50 : Huffman codes.]
/// We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. 
/// Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is to 
/// construct /// a list hc(S,C) terms, where C is the Huffman code word for the symbol 
/// S. In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), 
/// hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.]. The task shall be 
/// performed by the predicate huffman/2 defined as follows:
/// 
///  % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
///  
/// Example in F#: 
/// 
/// > huffman [('a',45);('b',13);('c',12);('d',16);('e',9);('f',5)];;
/// val it : (char * string) list =
///   [('a', "0"); ('b', "101"); ('c', "100"); ('d', "111"); ('e', "1101");
///    ('f', "1100")]

(*[omit:(Solution)]*)
// First we create a representation of the Huffman tree
type 'a HuffmanTree = Node of int (*frecuency*) * 'a (* left *) HuffmanTree * 'a (* right *) HuffmanTree | Leaf of int * 'a (* term *)

// Auxiliary function to get the frecuency
let frecuency = function
    | Leaf (frec, _) -> frec
    | Node(frec, _, _) -> frec

// Once we have build the Huffman tree, we can use this function to assing the codes
// nodes to the left get a '0'. Nodes to the right get a '1'.
let encode tree =
    let rec enc code tree cont =
        match tree with
            | Leaf (_, a) -> cont [(a, code)]
            | Node(_, lt, rt) ->
                enc (code + "0") lt <| fun ltacc -> enc (code + "1") rt <| fun rtacc -> cont (ltacc @ rtacc)
    enc "" tree id

// The algorithm is explained here: http://en.wikipedia.org/wiki/Huffman_coding
// The implementation below uses lists. For better performance use a priority queue.
// This is how it works. First we transform the list of terms and frecuencies into a list of Leafs (6).
// Then, before anything happpens, we sort the list to place the terms with the lowest frecuency
// at the head of the List (1) (this is where a priority queue would shine). 
// Otherwise, we combine the first two elements into a Node with the combined frecuency of the two nodes (4). 
// We add the node to the list and try again (5). Eventualy the list is reduced to 
// one term and we're done constructing the tree (2). Once we have the tree, we just need to encode it (7).
let huffman symbols =
    let rec createTree tree = 
        let xs = tree |> List.sortBy frecuency (* 1 *)
        match xs with
            | [] -> failwith "Empty list"
            | [x] -> x (* 2 *)
            | x::y::xs -> (* 3 *)
                let ht = Node(frecuency x + frecuency y, x , y) (* 4 *)
                createTree (ht::xs) (* 5 *)
    let ht = symbols 
             |> List.map(fun (a,f) -> Leaf (f,a)) (* 6 *)
             |> createTree
    encode ht |> List.sortBy(fst) (* 7 *)
(*[/omit]*)
// [/snippet]