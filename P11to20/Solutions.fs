// [snippet: Ninety-Nine F# Problems - Problems 11 - 20 - List, continued]
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

// [snippet: (*) Problem 11 :  Modified run-length encoding.]
/// Modify the result of problem 10 in such a way that if an element has no duplicates it 
/// is simply copied into the result list. Only elements with duplicates are transferred as
/// (N E) lists.
///  
/// Example: 
/// * (encode-modified '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeModified <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]

type 'a Encoding = Multiple of int * 'a | Single of 'a

(*[omit:(Solution)]*)
/// From problem 9 
let pack xs = 
    let collect x = function
        | (y::xs)::xss when x = y -> (x::y::xs)::xss
        | xss -> [x]::xss
    List.foldBack collect xs []

let encodeModified xs = xs |> pack |> List.map (Seq.countBy id >> Seq.head >> fun(x,n)-> if n = 1 then Single x else Multiple (n,x))
(*[/omit]*)
// [/snippet]


// [snippet: (**) Problem 12 : Decode a run-length encoded list.]
/// Given a run-length code list generated as specified in problem 11. Construct its 
/// uncompressed version.
///  
/// Example in F#: 
/// 
/// > decodeModified 
///     [Multiple (4,'a');Single 'b';Multiple (2,'c');
///      Multiple (2,'a');Single 'd';Multiple (4,'e')];;
/// val it : char list =
///   ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 'a'; 'd'; 'e'; 'e'; 'e'; 'e']

(*[omit:(Solution)]*)
let decodeModified xs = 
    let expand = function
        | Single x -> [x]
        | Multiple (n,x) -> List.replicate n x
    xs |> List.collect expand
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 13 : Run-length encoding of a list (direct solution).]
/// Implement the so-called run-length encoding data compression method directly. I.e. 
/// don't explicitly create the sublists containing the duplicates, as in problem 9, 
/// but only count them. As in problem P11, simplify the result list by replacing the 
/// singleton lists (1 X) by X.
///  
/// Example: 
/// * (encode-direct '(a a a a b c c a a d e e e e))
/// ((4 A) B (2 C) (2 A) D (4 E))
///  
/// Example in F#: 
/// 
/// > encodeDirect <| List.ofSeq "aaaabccaadeeee"
/// val it : char Encoding list =
///   [Multiple (4,'a'); Single 'b'; Multiple (2,'c'); Multiple (2,'a');
///    Single 'd'; Multiple (4,'e')]

(*[omit:(Solution)]*)
let encodeDirect xs = 
    let collect x = function
        | [] -> [Single x]
        | Single y::xs when x = y -> Multiple(2, x)::xs
        | Single _::_ as xs -> Single x::xs
        | Multiple(n,y)::xs when y = x -> Multiple(n + 1, x)::xs
        | xs -> Single x::xs
    List.foldBack collect xs []
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 14 : Duplicate the elements of a list.]
/// Example: 
/// * (dupli '(a b c c d))
/// (A A B B C C C C D D)
///  
/// Example in F#: 
/// 
/// > dupli [1; 2; 3]
/// [1;1;2;2;3;3]

(*[omit:(Solution 1)]*)
let dupli xs = xs |> List.map (fun x -> [x; x]) |> List.concat
(*[/omit]*)

(*[omit:(Solution 2)]*)
let rec dupli' = function
    | [] -> []
    | x::xs -> x::x::dupli' xs
(*[/omit]*)

(*[omit:(Solution 3)]*)
let dupli'' xs = [ for x in xs do yield x; yield x ]
(*[/omit]*)

(*[omit:(Solution 4)]*)
let dupli''' xs = xs |> List.collect (fun x -> [x; x])
(*[/omit]*)

(*[omit:(Solution 5)]*)
let dupli'''' xs = (xs,[]) ||> List.foldBack(fun x xs -> x::x::xs) 
(*[/omit]*)

(*[omit:(Solution 6)]*)
let dupli''''' xs = ([], xs) ||> List.fold(fun xs x -> xs @ [x; x])
(*[/omit]*)

(*[omit:(Solution 7)]*)
let dupli'''''' xs = xs |> List.collect (List.replicate 2)
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 15 : Replicate the elements of a list a given number of times.]
/// Example: 
/// * (repli '(a b c) 3)
/// (A A A B B B C C C)
///  
/// Example in F#: 
/// 
/// > repli (List.ofSeq "abc") 3
/// val it : char list = ['a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'c']

(*[omit:(Solution 1)]*)
let repli xs n = xs |> List.collect (List.replicate n)
(*[/omit]*)

(*[omit:(Solution 2)]*)
let repli' xs n= 
    [ for x in xs do 
        for i=1 to n do
            yield x ]
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 16 : Drop every N'th element from a list.]
/// Example: 
/// * (drop '(a b c d e f g h i k) 3)
/// (A B D E G H K)
///  
/// Example in F#: 
/// 
/// > dropEvery (List.ofSeq "abcdefghik") 3;;
/// val it : char list = ['a'; 'b'; 'd'; 'e'; 'g'; 'h'; 'k']

(*[omit:(Solution 1)]*)
let dropEvery xs n = xs |> List.mapi (fun i x -> (i + 1,x)) |> List.filter(fun (i,_) -> i % n <> 0) |> List.map snd
(*[/omit]*)

(*[omit:(Solution 2)]*)
let dropEvery' xs n =
    let rec drop xs count =
        match xs,count with
            | [], _ -> []
            | _::xs, 1 -> drop xs n
            | x::xs, _ -> x::drop xs (count - 1) 
    drop xs n
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 17 : Split a list into two parts; the length of the first part is given.]
/// Do not use any predefined predicates. 
/// 
/// Example: 
/// * (split '(a b c d e f g h i k) 3)
/// ( (A B C) (D E F G H I K))
///  
/// Example in F#: 
/// 
/// > split (List.ofSeq "abcdefghik") 3
/// val it : char list * char list =
///   (['a'; 'b'; 'c'], ['d'; 'e'; 'f'; 'g'; 'h'; 'i'; 'k'])

(*[omit:(Solution)]*)
let split xs n = 
    let rec take n xs =
        match xs,n with
            | _,0 -> []
            | [],_ -> []
            | x::xs,n -> x::take (n-1) xs
    let rec drop n xs =
        match xs,n with
            | xs,0 -> xs
            | [],_ -> []
            | _::xs,n -> drop (n-1) xs
    take n xs, drop n xs
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 18 : Extract a slice from a list.]
/// Given two indices, i and k, the slice is the list containing the elements between the 
/// i'th and k'th element of the original list (both limits included). Start counting the 
/// elements with 1.
///  
/// Example: 
/// * (slice '(a b c d e f g h i k) 3 7)
/// (C D E F G)
///  
/// Example in F#: 
/// 
/// > slice ['a';'b';'c';'d';'e';'f';'g';'h';'i';'k'] 3 7;;
/// val it : char list = ['c'; 'd'; 'e'; 'f'; 'g']

(*[omit:(Solution 1)]*)
let slice xs s e =
    let rec take n xs =
        match xs,n with
            | _,0 -> []
            | [],_ -> []
            | x::xs,n -> x::take (n-1) xs
    let rec drop n xs =
        match xs,n with
            | xs,0 -> xs
            | [],_ -> []
            | _::xs,n -> drop (n-1) xs
    let diff = e - s
    xs |> drop (s - 1) |> take (diff + 1)
(*[/omit]*)

(*[omit:(Solution 2)]*)
let slice' xs s e = [ for (x,j) in Seq.zip xs [1..e] do
                            if s <= j then
                                yield x ]
(*[/omit]*)

(*[omit:(Solution 3)]*)
let slice'' xs s e = xs |> Seq.zip (seq {1 .. e}) |> Seq.filter(fst >> (<=) s) |> Seq.map snd
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 19 : Rotate a list N places to the left.]
/// Hint: Use the predefined functions length and (@) 
/// 
/// Examples: 
/// * (rotate '(a b c d e f g h) 3)
/// (D E F G H A B C)
/// 
/// * (rotate '(a b c d e f g h) -2)
/// (G H A B C D E F)
///  
/// Examples in F#: 
/// 
/// > rotate ['a';'b';'c';'d';'e';'f';'g';'h'] 3;;
/// val it : char list = ['d'; 'e'; 'f'; 'g'; 'h'; 'a'; 'b'; 'c']
///  
/// > rotate ['a';'b';'c';'d';'e';'f';'g';'h'] (-2);;
/// val it : char list = ['g'; 'h'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f']

(*[omit:(Solution 1)]*)
// using problem 17
let rotate xs n =
    let at = let ln = List.length xs in abs <| (ln + n) % ln
    let st,nd = split xs at
    nd @ st
(*[/omit]*)

(*[omit:(Solution 2)]*)
let rec rotate' xs n =
    match xs, n with
        | [], _ -> []
        | xs, 0 -> xs
        | x::xs, n when n > 0 -> rotate' (xs @ [x]) (n - 1)
        | xs, n -> rotate' xs (List.length xs + n)
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 20 : Remove the K'th element from a list.]
/// Example in Prolog: 
/// ?- remove_at(X,[a,b,c,d],2,R).
/// X = b
/// R = [a,c,d]
///  
/// Example in Lisp: 
/// * (remove-at '(a b c d) 2)
/// (A C D)
///  
/// (Note that this only returns the residue list, while the Prolog version also returns 
/// the deleted element.)
///  
/// Example in F#: 
/// 
/// > removeAt 1 <| List.ofSeq "abcd";;
/// val it : char * char list = ('b', ['a'; 'c'; 'd'])

(*[omit:(Solution 1)]*)
let removeAt n xs = 
    let rec rmAt acc xs n =
        match xs, n with
            | [], _ -> failwith "empty list you fool!"
            | x::xs, 0 -> (x, (List.rev acc) @ xs)
            | x::xs, n -> rmAt (x::acc) xs (n - 1)
    rmAt [] xs n
(*[/omit]*)

(*[omit:(Solution 2)]*)
// using problem 17
let removeAt' n xs = 
    let front,back = split xs n
    List.head back, front @ List.tail back
(*[/omit]*)
// [/snippet]