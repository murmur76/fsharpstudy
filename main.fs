
module Homework1

type Tree<'a> =
  | Node of Tree<'a> * 'a * Tree<'a>
  | Leaf of 'a

let sum num =
  [1..num] |> List.sum

let fac num =
  [1..num] |> List.fold (fun acc elem -> acc * elem) 1

let rec fib num =
  if num = 0 || num = 1 then 1
  else (fib (num - 1)) + (fib (num - 2))

let rec gcd leftNum rightNum =
  if leftNum = rightNum then leftNum
  elif leftNum > rightNum then gcd (leftNum - rightNum) rightNum
  else gcd leftNum (rightNum - leftNum)

let rec max listOfNum =
  match listOfNum with
    | [] -> 0
    | head::tail ->
        let maxOfTail = max tail
        if head > maxOfTail then head else maxOfTail


let rec sumTree tree =
  match tree with
    | Leaf value -> value
    | Node (leftTree, value, rightTree) -> (sumTree leftTree) + value + (sumTree rightTree)

let rec depth tree =
  match tree with
    | Leaf _ -> 0
    | Node (leftTree, _, rightTree) -> (max [(depth leftTree); (depth rightTree)]) + 1

let rec binSearch tree toSearch =
  match tree with
    | Leaf leafValue -> toSearch = leafValue
    | Node (leftTree, nodeValue, rightTree) ->
        (binSearch leftTree toSearch) || nodeValue = toSearch || (binSearch rightTree toSearch)

let rec preorder tree =
  match tree with
    | Leaf value -> [ value ]
    | Node (leftTree, nodeValue, rightTree) -> nodeValue :: (preorder leftTree) @ (preorder rightTree)


let rec listAdd leftList rightList =
  match (leftList, rightList) with
    | (x::xs, y::ys) -> (x+y)::(listAdd xs ys)
    | _ -> []

let rec insert element listOfNum =
  match listOfNum with
    | x::xs when x < element -> x::(insert element xs)
    | _::_ -> element :: listOfNum
    | [] -> [ element ]

let rec insortHelper sorted unsorted =
  match unsorted with
    | [] -> sorted
    | x::xs -> insortHelper (insert x sorted) xs

let insort listOfNum = insortHelper [] listOfNum

// Is this right?
let compose f g = f >> g
let composeByTaehoon f g = fun x -> g f x

let curry uncurried arg1 arg2 = uncurried (arg1, arg2)

let uncurry curried (arg1, arg2) = curried arg1 arg2

let rec multifun f count =
  match count with
    | 1 -> f
    | num when num > 1 -> f >> (multifun f (count - 1))
    | _ ->
        printfn "Not Reached in multifun"
        f

let rec ltake listInput count =
  match (listInput, count) with
    | (_, 0) -> []
    | ([], _) -> []
    | (x::xs, num) when num > 0 -> x::(ltake xs (count - 1))
    | _ ->
        printfn "Not Reached in ltake"
        []

let rec lall predicate listInput =
  match listInput with
    | [] -> true
    | x::xs -> (predicate x) && (lall predicate xs)

let lmap transformer listInput = [ for a in listInput do yield transformer a ]

let rec lrev = function
  | [] -> []
  | x::xs -> (lrev xs) @ [x]

let rec lzip = function
  | ([], _) -> []
  | (_, []) -> []
  | (x::xs, y::ys) -> (x,y)::(lzip (xs, ys))

let rec split = function
  | x::y::xs ->
    let (odds, evens) = split xs
    (x::odds, y::evens)
  | x::[] -> ([x], [])
  | [] -> ([], [])

let rec cartprod leftList rightList =
  match (leftList, rightList) with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, ys) -> (lmap (fun y -> (x, y)) ys) @ (cartprod xs ys)

printfn "Fsharp homework"


module HWassert =
  let should cond =
    if cond then printfn "." else printfn "Fail test"

  let shouldEqual left right =
    if left = right then printfn "."
    else printfn "%A should equal %A" left right

HWassert.shouldEqual (sum 3) 6
HWassert.shouldEqual (fac 4) 24
// 1 1 2 3 5 8
HWassert.should ((fib 5) = 8)
HWassert.should ((gcd 24 36) = 12)
HWassert.should ((max []) = 0)
HWassert.should ((max [1;9;10]) = 10)
HWassert.shouldEqual (max [11;9]) 11

HWassert.shouldEqual (sumTree (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4))) 17
HWassert.shouldEqual (depth (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4))) 2
HWassert.shouldEqual (binSearch (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) 2) true
HWassert.shouldEqual (binSearch (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4)) 11) false
HWassert.shouldEqual (preorder (Node (Node (Leaf 1, 3, Leaf 2), 7, Leaf 4))) [7; 3; 1; 2; 4]
HWassert.shouldEqual (listAdd [1;2;3] [4;5]) [5;7]
HWassert.shouldEqual (listAdd [1;2;3] []) []
HWassert.shouldEqual (listAdd [] [4;5]) []
HWassert.shouldEqual (insert 3 [1;2;3]) [1;2;3;3]
HWassert.shouldEqual (insert 3 []) [3]
HWassert.shouldEqual (insert 3 [2;3;4]) [2;3;3;4]
HWassert.shouldEqual (insort [1;9;3;2]) [1;2;3;9]
HWassert.shouldEqual (insort [1]) [1]
HWassert.shouldEqual (insort []) []
HWassert.shouldEqual ((compose (fun x -> x + 1) (fun y -> y * 10)) 10) 110
HWassert.shouldEqual ((curry (fun (x, y) -> x * y)) 10 100) 1000
let multiplyUC (x, y) = x * y
let curriedMultiple10 = curry multiplyUC 10
HWassert.shouldEqual (curriedMultiple10 3) 30
HWassert.shouldEqual (uncurry (fun x y -> x * y) (3, 4)) 12
let add1 x = x + 1
HWassert.shouldEqual ((multifun add1 1) 1) 2
HWassert.shouldEqual ((multifun add1 3) 1) 4
HWassert.shouldEqual ((multifun (fun x -> x + 1) 3) 1) 4
HWassert.shouldEqual (ltake [1; 2; 3] 2) [1; 2]
HWassert.shouldEqual (ltake [1; 2; 3] 5) [1; 2; 3]
HWassert.shouldEqual (ltake ["s";"t";"r";"i";"k";"e";"r";"z" ] 5) ["s";"t";"r";"i";"k"]
HWassert.shouldEqual (lall (fun x -> x > 0) [1; 2; 3]) true
HWassert.shouldEqual (lmap (fun x -> x + 1) [1; 2; 3]) [2; 3; 4]
HWassert.shouldEqual (lrev [1; 2; 3; 4]) [4; 3; 2; 1]
HWassert.shouldEqual (lzip (["Rooney";"Park";"Scholes";"C.Ronaldo"], [8;13;18;7;10;12])) [("Rooney",8);("Park",13);("Scholes",18);("C.Ronaldo",7)]
HWassert.shouldEqual (split [1; 3; 5; 7; 9; 11]) ([1; 5; 9], [3; 7; 11])
HWassert.shouldEqual (cartprod [1;2] [3;4;5]) [(1,3); (1,4); (1,5); (2,3); (2,4); (2,5)]
