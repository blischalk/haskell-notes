# Haskell notes

:l to load file on command line

## Lists

++ Put two lists together

:  Put something at the beginning of a list

!! Get element out of list by index

	"Steve Buscemi" !! 6
    [9.4,33.2,96.2,11.2,23.25] !! 1

* head
* tail
* last
* init - takes a list and returns everything except its last element
* length
* null - checks if a list is empty
* reverse
* take
* drop
* maximum
* minimum
* sum
* product
* elem - 4 \`elem\` [3,4,5,6]

## Ranges
	[..20]
	[‘a’..’z’]
	[‘K’..’Z’]
	[2,4..20]
	[3,6..20]
	[10000, 9998..0]
	
* cycle
* repeat
* replicate

## List Comprehension
	[x*2 | x <- [1..10]]

list comprehension with condition

	[x*2 | x <- [1..10], x*2 >= 12]

list comprehension in function

	boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

multiple predicates

	[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

multiple lists

	[ x*y | x <- [2,5,10], y <- [8,10,11]]

nested list comprehension

	[ [ x | x <- xs, even x ] | xs <- xxs]

## Tuples
	(“a”, “tuple”, 1)
	
	let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]

	zip [1,2,3] [4,5,6] results in [(1,4),(2,5),(3,6)]
	
* fst
* snd
* zip


##Types and Typeclasses

Explicit types are always denoted with the first letter in capital case.

:t used to get type

If you want to give your function a type declaration but are unsure as to what it should be, you can always just write the function without it and then check it with :t

	removeNonUppercase :: [Char] -> [Char]  
	removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']] 

	addThree :: Int -> Int -> Int -> Int  
	addThree x y z = x + y + z 


### Common Types

* Int
* Integer
* Float
* Double
* Bool
* Char

A *type variable* (like a generic in java) is denoted in lowercase.

## Typeclass (like a java interface but better)

* Eq
* Ord
* Show
* Read
* Enum
* Bounded
* Num
* Integral
* Floating

## Pattern Matching
	lucky :: (Integral a) => a -> String  
	lucky 7 = "LUCKY NUMBER SEVEN!"  
	lucky x = "Sorry, you're out of luck, pal!"
	
	factorial :: (Integral a) => a -> a  
	factorial 0 = 1  
	factorial n = n * factorial (n - 1)
	
	addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
	addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
	
	first :: (a, b, c) -> a  
	first (x, _, _) = x  
  
	second :: (a, b, c) -> b  
	second (_, y, _) = y  
  
	third :: (a, b, c) -> c  
	third (_, _, z) = z
	
	let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)] 
	[a+b | (a,b) <- xs]
	
	head' :: [a] -> a  
	head' [] = error "Can't call head on an empty list, dummy!"  
	head' (x:_) = x
	
	tell :: (Show a) => [a] -> String  
	tell [] = "The list is empty"  
	tell (x:[]) = "The list has one element: " ++ show x  
	tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
	tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y
	
	capital :: String -> String  
	capital "" = "Empty string, whoops!"  
	capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
	
## Guards

	bmiTell :: (RealFloat a) => a -> String  
	bmiTell bmi  
    	| bmi <= 18.5 = "You're underweight, you emo, you!"  
    	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    	| otherwise   = "You're a whale, congratulations!" 
    	
    bmiTell :: (RealFloat a) => a -> a -> String  
	bmiTell weight height  
    	| weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    	| weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    	| weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    	| otherwise                 = "You're a whale, congratulations!"  
    	
    max' :: (Ord a) => a -> a -> a  
	max' a b | a > b = a | otherwise = b 
	
	bmiTell :: (RealFloat a) => a -> a -> String  
	bmiTell weight height  
    	| bmi <= 18.5 = "You're underweight, you emo, you!"  
    	| bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    	| bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    	| otherwise   = "You're a whale, congratulations!"  
    	where bmi = weight / height ^ 2 
    
## Let
	cylinder :: (RealFloat a) => a -> a -> a  
	cylinder r h = 
		let sideArea = 2 * pi * r * h  
        	topArea = pi * r ^2  
    	in  sideArea + 2 * topArea 
    	
    4 * (let a = 9 in a + 1) + 2
    
    [let square x = x * x in (square 5, square 3, square 2)] 
    
    (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
    
    calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
	calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
	
	calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
	calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
	
	let boot x y z = x * y + z in boot 3 4 2
	
## Case
	describeList :: [a] -> String  
	describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                                   [x] -> "a singleton list."   
                                                   xs -> "a longer list."
                                                   
    describeList :: [a] -> String  
	describeList xs = "The list is " ++ what xs  
    	where what [] = "empty."  
              what [x] = "a singleton list."  
              what xs = "a longer list."
              
## Recursion

	maximum' :: (Ord a) => [a] -> a  
	maximum' [] = error "maximum of empty list"  
	maximum' [x] = x  
	maximum' (x:xs) = max x (maximum' xs)

	replicate' :: (Num i, Ord i) => i -> a -> [a]  
	replicate' n x
		| n <= 0    = []  
		| otherwise = x:replicate' (n-1) x

	take' :: (Num i, Ord i) => i -> [a] -> [a]  
	take' n _ 
		| n <= 0   = []  
	take' _ []     = []  
	take' n (x:xs) = x : take' (n-1) xs 

	reverse' :: [a] -> [a]  
	reverse' [] = []  
	reverse' (x:xs) = reverse' xs ++ [x] 
	
	repeat' :: a -> [a]  
	repeat' x = x:repeat' x 

	zip' :: [a] -> [b] -> [(a,b)]  
	zip' _ [] = []  
	zip' [] _ = []  
	zip' (x:xs) (y:ys) = (x,y):zip' xs ys 

	elem' :: (Eq a) => a -> [a] -> Bool  
	elem' a [] = False  
	elem' a (x:xs)  
    	| a == x    = True  
    	| otherwise = a `elem'` xs

	quicksort :: (Ord a) => [a] -> [a]  
	quicksort [] = []  
	quicksort (x:xs) =
		let smallerSorted = quicksort [a | a <- xs, a <= x]  
        	biggerSorted = quicksort [a | a <- xs, a > x]  
    	in  smallerSorted ++ [x] ++ biggerSorted
    	
    	
## Higher Order Functions

Note to self:  There are two ways of thinking about functions.  The easy way is to just look at the type definition and see all the elements as arguments except the last which would be the return value.  The more accurate way to think about functions is that each -> that you see means return thus, each step is essentially returning a function.  Ex:

	myFunc :: Int -> Int -> Int
	
This can be thought of as
	
	myFunc :: Int -> (Int -> Int)
	
Which can be read as myFunc takes an Int and returns a functio that takes an Int and returns an Int.

### Curried Functions
* All functions in Haskell only take one argument
* Putting a space between two things is **function application**

This:

	max :: (Ord a) => a -> a -> a

Is the same as

	max :: (Ord a) => a -> (a -> a)
	
That could be read as: max takes an a and returns (that's the ->) a function that takes an a and returns an a

If we call a function with too few parameters, we get back a partially applied function

	multThree :: (Num a) => a -> a -> a -> a  
	multThree x y z = x * y * z 
	
	let multTwoWithNine = multThree 9
	multTwoWithNine 2 3
	output: 54
	
	let multWithEighteen = multTwoWithNine 2
	multWithEighteen 10
	output: 180
	
multThree's type could be written as this:

	multThree :: (Num a) => a -> (a -> (a -> a))
	
Use sections to partially apply infix functions:

	divideByTen :: (Floating a) => a -> a  
	divideByTen = (/10)
	
Functions can take functions:

	applyTwice :: (a -> a) -> a -> a  
	applyTwice f x = f (f x)
	
* map
* filter
* takeWhile

## Lambdas

Indicated with a \
Followed by -> and function body

ex:

	(\a b -> (a * 30 + 3) / b)
	
	addThree :: (Num a) => a -> a -> a -> a  
	addThree x y z = x + y + z  
	
	addThree :: (Num a) => a -> a -> a -> a  
	addThree = \x -> \y -> \z -> x + y + z
	
	flip' :: (a -> b -> c) -> b -> a -> c  
	flip' f = \x y -> f y x
	
You can pattern match in lambdas but can only define one pattern.  If the pattern doesn't match an error will occur.

	map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 
	
foldl is like inject in Ruby.  Starts at the head of a collection.

	sum' :: (Num a) => [a] -> a  
	sum' xs = foldl (\acc x -> acc + x) 0 xs
	
	sum' :: (Num a) => [a] -> a  
	sum' = foldl (+) 0
	
Foldr starts from the opposite end of the collection.

	map' :: (a -> b) -> [a] -> [b]  
	map' f xs = foldr (\x acc -> f x : acc) [] xs 
	
Folds can be used to implement any function where you traverse a list once, element by element, and then return something based on that. Whenever you want to traverse a list to return something, chances are you want a fold.

`foldl1` and `foldr1` work just like the functions with out 1 but assume the first element as the starting value.

* scanl
* scanr
* scanl1
* scanr1

These functions report there intermediate accumulator states if the form of a list.

	scanl (+) 0 [3,5,2,1]
	output: [0,3,8,10,11]
	
## Function Application $
When a $ is encountered, the expression on its right is applied as the parameter to the function on its left

But apart from getting rid of parentheses, $ means that function application can be treated just like another function. That way, we can, for instance, map function application over a list of functions.

## Function composition
We do function composition with the . function
	
*The following acheive the same result*:
	
	map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
	map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

Functions that take multiple arguments need partial application

If you want to rewrite an expression with a lot of parentheses by using function composition, you can start by putting the last parameter of the innermost function after a $ and then just composing all the other function calls, writing them without their last parameter and putting dots between them.

	replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
	replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
	
### Point Free Style (Also Called the Pointless Style)

Normal style:

	sum' :: (Num a) => [a] -> a     
	sum' xs = foldl (+) 0 xs
	
Because of currying, we can omit the xs on both sides, because calling foldl (+) 0 creates a function that takes a list.

The function can be rewritten as:

	sum' = foldl (+) 0
	
# Modules

	import <module name>
This must be done before defining any functions, so imports are usually done at the top of the file.

You can also put the functions of modules into the global namespace when using GHCI. If you're in GHCI and you want to be able to call the functions exported by Data.List, do this:

	ghci> :m + Data.List
	ghci> :m + Data.List Data.Map Data.Set 
	
You can selectively import functions

	import Data.List (nub, sort)
	
Exclusive

	import Data.List hiding (nub)
	
Import as qualified:

	import qualified Data.Map
	
Filtering function would then be called like:

	Data.Map.filter
	
## Data.List

The Data.List module is all about lists, obviously. It provides some very useful functions for dealing with them. We've already met some of its functions (like map and filter) because the Prelude module exports some functions from Data.List for convenience. You don't have to import Data.List via a qualified import because it doesn't clash with any Prelude names except for those that Prelude already steals from Data.List. Let's take a look at some of the functions that we haven't met before.

	intersperse '.' "MONKEY"
	"M.O.N.K.E.Y"
	
	intercalate " " ["hey","there","guys"]
	"hey there guys"
	
	transpose [[1,2,3],[4,5,6],[7,8,9]]
	[[1,4,7],[2,5,8],[3,6,9]]
	
	transpose ["hey","there","guys"]
	["htg","ehu","yey","rs","e"]
	
`foldl'` and `foldl1'` are stricter versions of their respective lazy incarnations. When using lazy folds on really big lists, you might often get a stack overflow error. The culprit for that is that due to the lazy nature of the folds, the accumulator value isn't actually updated as the folding happens. What actually happens is that the accumulator kind of makes a promise that it will compute its value when asked to actually produce the result (also called a thunk). That happens for every intermediate accumulator and all those thunks overflow your stack. The strict folds aren't lazy buggers and actually compute the intermediate values as they go along instead of filling up your stack with thunks. So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.

	concat ["foo","bar","car"]
	"foobarcar"
	
	concat [[3,4,5],[2,3,4],[2,1,1]]
	[3,4,5,2,3,4,2,1,1]
	
	concatMap (replicate 4) [1..3]
	[1,1,1,1,2,2,2,2,3,3,3,3]
	
	and $ map (>4) [5,6,7,8]
	True
	
	and $ map (==4) [4,4,4,3,4]
	False
	
	or $ map (==4) [2,3,4,5,6,1]
	True
	
	or $ map (>4) [1,2,3]
	False
	
	any (==4) [2,3,5,6,1,4]
	True
	
	all (>4) [6,9,10]
	True
	
	take 10 $ iterate (*2) 1
	[1,2,4,8,16,32,64,128,256,512]
	
	splitAt 3 "heyman"  
	("hey","man")
	
	splitAt 100 "heyman"  
	("heyman","")
	
	splitAt (-3) "heyman"  
	("","heyman")
	
	let (a,b) = splitAt 3 "foobar" in b ++ a  
	"barfoo"
	
	takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
	[6,5,4]
	
	sum $ takeWhile (<10000) $ map (^3) [1..]
	53361
	
	dropWhile (/=' ') "This is a sentence"
	" is a sentence"
	
	let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
	head (dropWhile (\(val,y,m,d) -> val < 1000) stock)
	(1001.4,2008,9,4)
	
	let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
	"First word: This, the rest: is a sentence"
	
	break (==4) [1,2,3,4,5,6,7]
	([1,2,3],[4,5,6,7])
	
	sort [8,5,3,2,1,6,4,2]
	[1,2,2,3,4,5,6,8]
	
	group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
	[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]
	
	map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
	[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]
	
	inits "w00t"
	["","w","w0","w00","w00t"]
	
	tails "w00t"
	["w00t","00t","0t","t",""]
	
	let w = "w00t" in zip (inits w) (tails w)
	[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]