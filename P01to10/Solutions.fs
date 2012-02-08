// [snippet: Ninety-Nine F# Problems - Problems 1 - 10 - Lists]
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

// [snippet: (*) Problem 1 : Find the last element of a list.]
/// Example in F#: 
/// > myLast [1; 2; 3; 4];;
/// val it : int = 4
/// > myLast ['x';'y';'z'];;
/// val it : char = 'z'

(*[omit:(Solution 1)]*)
// Solution using recursion
let rec myLast xs = 
    match xs with
        | [] -> failwith "empty list you fool!"
        | [x] -> x
        | _::xs -> myLast xs
(*[/omit]*)

(*[omit:(Solution 2)]*)
// Solution using higher-order functions
let myLast' xs = xs |> List.rev |> List.head
(*[/omit]*)

(*[omit:(Solution 3)]*)
// ignore the acumulator using reduce
let myLast'' xs = List.reduce(fun _ x -> x) xs
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 2 : Find the last but one element of a list.]
/// (Note that the Lisp transcription of this problem is incorrect.) 
///
/// Example in F#: 
/// myButLast [1; 2; 3; 4];;
/// val it : int = 3
/// > myButLast ['a'..'z'];;
/// val it : char = 'y'

(*[omit:(Solution 1)]*)
// Solution with pattern matching
let rec myButLast = function
    | [] -> failwith "empty list you fool!"
    | [x] -> failwith "singleton list you fool!"
    | [x;_] -> x
    | _::xs -> myButLast xs
(*[/omit]*)

(*[omit:(Solution 2)]*)
let myButLast' xs = xs |> List.rev |> List.tail |> List.head
(*[/omit]*)

(*[omit:(Solution 3)]*)
let myButLast'' xs = 
    let flip f a b = f b a
    xs |> List.rev |> flip List.nth 1
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 3 : Find the K'th element of a list. The first element in the list is number 1.]
/// Example: 
/// * (element-at '(a b c d e) 3)
/// c
/// 
/// Example in F#: 
/// > elementAt [1; 2; 3] 2;;
/// val it : int = 2
/// > elementAt (List.ofSeq "fsharp") 5;;
/// val it : char = 'r'

(*[omit:(Solution 1)]*)
// List.nth is zero based
let elementAt xs n = List.nth xs (n - 1)
(*[/omit]*)

(*[omit:(Solution 2)]*)
// Recursive solution with pattern matching
let rec elementAt' xs n = 
    match xs,n with
        | [],_   -> failwith "empty list you fool!"
        | x::_,1 -> x
        | _::xs,n -> elementAt xs (n - 1)
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 4 : Find the number of elements of a list.]
/// Example in F#: 
/// 
/// > myLength [123; 456; 789];;
/// val it : int = 3
/// > myLength <| List.ofSeq "Hello, world!"
/// val it : int = 13 

(*[omit:(Solution 1)]*)
// Solution using the library method
let myLength = List.length
(*[/omit]*)

(*[omit:(Solution 2)]*)
// replace the elemt with 1 and sum all the ones
let myLength' xs = xs |> List.sumBy(fun _ -> 1) 
(*[/omit]*)

(*[omit:(Solution 3)]*)
// Solution using tail-recursion
let myLength'' xs =
    let rec length acc = function
        | [] -> acc
        | _::xs  -> length (acc+1) xs
    length 0 xs
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 5 : Reverse a list.]
/// Example in F#: 
///
/// > reverse <| List.ofSeq ("A man, a plan, a canal, panama!")
/// val it : char list =
///  ['!'; 'a'; 'm'; 'a'; 'n'; 'a'; 'p'; ' '; ','; 'l'; 'a'; 'n'; 'a'; 'c'; ' ';
///   'a'; ' '; ','; 'n'; 'a'; 'l'; 'p'; ' '; 'a'; ' '; ','; 'n'; 'a'; 'm'; ' ';
///   'A']
/// > reverse [1,2,3,4];;
/// val it : int list = [4; 3; 2; 1]

(*[omit:(Solution 1)]*)
// Using tail-recursion
let reverse xs = 
    let rec rev acc = function
        | [] -> acc
        | x::xs -> rev (x::acc) xs
    rev [] xs
(*[/omit]*)

(*[omit:(Solution 2)]*)
let reverse' xs = List.fold(fun acc x -> x::acc) [] xs
(*[/omit]*)

(*[omit:(Solution 3)]*)
let reverse'' = List.rev
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 6 : Find out whether a list is a palindrome.]
/// A palindrome can be read forward or backward; e.g. (x a m a x).
/// 
/// Example in F#: 
/// > isPalindrome [1;2;3];;
/// val it : bool = false
/// > isPalindrome <| List.ofSeq "madamimadam";;
/// val it : bool = true
/// > isPalindrome [1;2;4;8;16;8;4;2;1];;
/// val it : bool = true

(*[omit:(Solution)]*)
// A list is a palindrome is the list is equal to its reverse
let isPalindrome xs = xs = List.rev xs
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 7 : Flatten a nested list structure.]
/// Transform a list, possibly holding lists as elements into a `flat' list by replacing each 
/// list with its elements (recursively).
///  
/// Example: 
/// * (my-flatten '(a (b (c d) e)))
/// (A B C D E)
///  
/// Example in F#: 
/// 
type 'a NestedList = List of 'a NestedList list | Elem of 'a
///
/// > flatten (Elem 5);;
/// val it : int list = [5]
/// > flatten (List [Elem 1; List [Elem 2; List [Elem 3; Elem 4]; Elem 5]]);;
/// val it : int list = [1;2;3;4;5]
/// > flatten (List [] : int List);;
/// val it : int list = []

(*[omit:(Solution 1)]*)
let flatten ls = 
    let rec loop acc = function 
        | Elem x -> x::acc
        | List xs -> List.foldBack(fun x acc -> loop acc x) xs acc
    loop [] ls
(*[/omit]*)

(*[omit:(Solution 2)]*)
#nowarn "40"
let flatten' x =
    let rec loop = List.collect(function
        | Elem x -> [x]
        | List xs -> loop xs)
    loop [x]
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 8 : Eliminate consecutive duplicates of list elements.] 
/// If a list contains repeated elements they should be replaced with a single copy of the 
/// element. The order of the elements should not be changed.
///  
/// Example: 
/// * (compress '(a a a a b c c a a d e e e e))
/// (A B C A D E)
///  
/// Example in F#: 
/// 
/// > compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
/// val it : string list = ["a";"b";"c";"a";"d";"e"]

(*[omit:(Solution 1)]*)
let compress xs = List.foldBack(fun x acc -> if List.isEmpty acc then [x] elif x = List.head acc then acc else x::acc) xs []
(*[/omit]*)

(*[omit:(Solution 2)]*)
let compress' = function
    | [] -> []
    | x::xs -> List.fold(fun acc x -> if x = List.head acc then acc else x::acc) [x] xs |> List.rev
(*[/omit]*)
// [/snippet]

// [snippet: (**) Problem 9 : Pack consecutive duplicates of list elements into sublists.] 
/// If a list contains repeated elements they should be placed 
/// in separate sublists.
///  
/// Example: 
/// * (pack '(a a a a b c c a a d e e e e))
/// ((A A A A) (B) (C C) (A A) (D) (E E E E))
///  
/// Example in F#: 
/// 
/// > pack ['a'; 'a'; 'a'; 'a'; 'b'; 'c'; 'c'; 'a'; 
///         'a'; 'd'; 'e'; 'e'; 'e'; 'e']
/// val it : char list list =
///  [['a'; 'a'; 'a'; 'a']; ['b']; ['c'; 'c']; ['a'; 'a']; ['d'];
///   ['e'; 'e'; 'e'; 'e']]

(*[omit:(Solution)]*)
let pack xs = 
    let collect x = function
        | [] -> failwith "empty list you fool!"
        | []::xss -> [x]::xss
        | (y::xs)::xss as acc -> 
            if x = y then
                (x::y::xs)::xss
            else
                [x]::acc
    List.foldBack collect xs [[]]
(*[/omit]*)
// [/snippet]

// [snippet: (*) Problem 10 : Run-length encoding of a list.]
/// Use the result of problem P09 to implement the so-called run-length 
/// encoding data compression method. Consecutive duplicates of elements 
/// are encoded as lists (N E) where N is the number of duplicates of the element E.
///  
/// Example: 
/// * (encode '(a a a a b c c a a d e e e e))
/// ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
///  
/// Example in Haskell: 
/// 
/// encode <| List.ofSeq "aaaabccaadeeee"
/// val it : (int * char) list =
///   [(4,'a');(1,'b');(2,'c');(2,'a');(1,'d');(4,'e')]

(*[omit:(Solutions 1)]*)
let encode xs = xs |> pack |> List.map (Seq.countBy id >> Seq.head >> fun(a,b)-> b,a)
(*[/omit]*)

(*[omit:(Solutions 2)]*)
let encode' xs = xs |> pack |> List.map(fun xs -> List.length xs, List.head xs)
(*[/omit]*)

(*[omit:(Solutions 3)]*)
let encode'' xs = 
    let collect x = function
        | [] -> [(1, x)]
        | (n,y)::xs as acc-> 
            if x = y then
                (n+1, y)::xs
            else
                (1,x)::acc
    List.foldBack collect xs []
(*[/omit]*)
// [/snippet]