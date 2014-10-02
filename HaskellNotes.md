# Haskell notes

## Types

Every expression and function in Haskell has a `type`. The type of a
value indicates that it shares certain properties with other values of
the same type.  A type system gives us an abstraction so we can tell
the computer these bytes are text, those bytes are a stock trade.  A
type system prevents us from accidentally mixing types up.

Haskell types are strong, static, and inferred.

Explicitly writing the type of an expression occurs in the form:

	expression :: type
	'a' :: Char

`:: theType` is referred to as a type signature.

## Polymorphic Types

To make a type polymorphic e.g., a list can of ints or a list of
strings, you must use a `type variable`.

e.x.,

	myFunk :: [a] -> a

## Basic Types

- "foo" has type `String`
- True has type `Bool`

**Int**: guaranteed by Haskell to be at least up to ±2^29 but exact
  size depends on system

Min and Max bounds can be found with:

    (minBound :: Int, maxBound :: Int)
    
**Integer**: is limited by the amount of memory on machine.

**Double**: for floating-point numbers
**Float**: single precision but not used much
**Bool**: True or False
**Char**: Unicode characters
**String**: String.  Lists of characters


## GHCI

:l to load file (can take an absolute or relative path to source file)
:r to reload a file
:t for the type of something
:k for the kind of something
:m + to set the context for expression evaluations
:? for a list of commands
:info get some useful information
:set +t (Print more type information)
:unset + t to turn off
:show bindings shows bound variables
:cd change directories


Prelude is a standard library of useful functions.  When you set the context, you will append modules to prelude.  Because the prompt will grow as modules are used, set prompt can be used:

    :set prompt "ghci> "
    
To load another module into ghci:

    :m + Control.Applicative
	:module + Data.Ratio

## Operators

	==
	<
	>=
	/=  -- Not equal
	not True

## Operator Fixity Rules

Operators have precedence.
use `:info operator` e.g, `:info (+)` to see precedence 1 - 9.
Higher precedence is applied before lower.

Associativity: Whether an operator is left (infixl) or right (infixr) associative. (Which direction evaluation occurs)

Don't try and remember the fixity rules.  Just add parens.

## Useful Info

- negative numbers should mostly be surrounded with parens (-3) so that it is not evaluated as subtraction.
- \`backticks\` make a function name into an infix operator.
- / is for floating point division \`div\` is used for integer division.
- fromIntegral converts any integral type (Int or Integer) to any other numeric type.
- round, floor, and ceiling convert floating-point numbers to Int or Integer.
- If statement is also an expression.  It will always return a value.  The else portion is mandatory.
- `putStrLn` to print to stdout.

## If statements

Both branches (then, else) must have the same return type.

## Data.Ratio

Adds the operator % so we can have rational numbers.

1 % 2  one half
2 % 3  two thirds
etc.


## The Offside Rule (Indention / Whitespace Counts)
When you have questions about indention, look this up.  Basically indentent how you normally would and everything should be fine...

## Lists

All elements must be of the same type.
All elements must be separated by commas without a trailing comma.

++ Concatenate two lists together

:  Add something to the beginning of a list

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

## Ranges (Enumeration)
	[1..20]
	[‘a’..’z’]
	[‘K’..’Z’]
	[2,4..20]
	[3,6..20]
	[10000, 9998..0]
	
* cycle
* repeat
* replicate

Beware enumerating floating point numbers

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

Useful for returning multiple values from a function.

	("a", "tuple", 1)
	
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

x :: y is read as "the expression x has type y"


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
Function dispatch based on parameter shape.

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

    bookID      (Book id title authors) = id
	bookTitle   (Book id title authors) = title
	bookAuthors (Book id title authors) = authors
	
## Guards

Dispatch on more than just a patterns shape.  After a successful pattern match, all guards are checked and a successful guard results in that value becoming the functions return value.

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

Define variables (or functions) to be used within an `in` block of code.  Similar to `where` but variables are defined before they are used as opposed to after.

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
	
## Where

Similar to `let` but allows you to define the code before the variables (or local functions) are defined.

	myFunk someVal = someVal - anotherVal
		where anotherVal = 10
		      localFunk a = a + anotherVal
	
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
	
	"cat" `isInfixOf` "im a cat burglar"
	True
	
	"hey" `isPrefixOf` "oh hey there!"
	False
	
`elem` and `notElem` check if an element is or isn't inside a list.

Partition
	
	partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
	("BOBMORGAN","sidneyeddy")
	
	partition (>3) [1,3,5,6,3,2,1,0,3,7]
	([5,6,7],[1,3,3,2,1,0,3])
	
Is different than
	
	span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
	("BOB","sidneyMORGANeddy")
	
Find

	find (>4) [1,2,3,4,5,6]
	Just 5
	
	find (>9) [1,2,3,4,5,6] 
	Nothing
	
	:t find
	find :: (a -> Bool) -> [a] -> Maybe a
	
elemIndex

	:t elemIndex
	elemIndex :: (Eq a) => a -> [a] -> Maybe Int
	4 `elemIndex` [1,2,3,4,5,6]
	Just 3
	10 `elemIndex` [1,2,3,4,5,6]
	Nothing
	
elemIndicies

	' ' `elemIndices` "Where are the spaces?"
	[5,9,13]
	
findIndex and findIndicies

	findIndex (==4) [5,3,2,1,6,4]
	Just 5
	
	findIndex (==7) [5,3,2,1,6,4]
	Nothing

zipWith3 and zip4 (up to 7)

	zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]
	[7,9,8]
	
	zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]
	[(2,2,5,2),(3,2,5,2),(3,2,3,2)]
	
lines
	
	lines "first line\nsecond line\nthird line"
	["first line","second line","third line"]
	
unlines

	unlines ["first line", "second line", "third line"]
	"first line\nsecond line\nthird line\n"
	
words and unwords

	words "hey these are the words in this sentence"
	["hey","these","are","the","words","in","this","sentence"]
	
	unwords ["hey","there","mate"]
	"hey there mate"
	
nub

	nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
	[1,2,3,4]
	
	nub "Lots of words and stuff"
	"Lots fwrdanu"
	
delete

	delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
	"ey tere gang!"
	
\\\ (set difference)

	[1..10] \\ [2,5,9]
	[1,3,4,6,7,8,10]
	
union

	"hey man" `union` "man what's up"
	"hey manwt'sup"
	
	[1..7] `union` [5..10]
	[1,2,3,4,5,6,7,8,9,10]
	
intersect

	[1..7] `intersect` [5..10]
	[5,6,7]
	
insert

	insert 4 [3,5,1,2,8,2]
	[3,4,5,1,2,8,2]
	
## 'Generic' and 'By' Forms of Functions

There are functions that for historical reasons only deal with very specific types such as length.  It only deals with Int but would be more useful if it could take Num.  That is when you would use `genericLength`.  For the by forms of functions; the difference between them is that the first set of functions use == to test for equality, whereas the By ones also take an equality function and then compare them by using that equality function. group is the same as groupBy (==)

## Compare \`on\` \<something\>

An even clearer way to write equality functions for the By functions is if you import the on function from Data.Function

	let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
	sortBy (compare `on` length) xs
	[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
	
# Data.Char

Has a lot of functions that you would normally use regex for. Ex. `isLower`.

	filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"
	["hey","guys","its","me"]
	
# Data.Map

# Creating Types and Typeclasses

Bool is a `type constructor`

    data Bool = False | True
    
The part after the equals are the value constructors.  They specify the different values that this type can have.  The Boolean type can have a value of either False or True.

    data Shape = Circle Float Float Float | Rectangle Float Float Float Float

Shape is a `type constructor`
    
The Circle and Rectangle value constructors accept floats as arguments as values they will contain.

#Type parameters
A value constructor can take some values and produce a new value.  **Type Constructors** can take types to produce new types.

Ex.

	Data Maybe a = Nothing | Just a
	
"a" is a type parameter.  Since a type parameter is involved, "Maybe" is a type constructor.

The type parameter is similar to generics in Java and templates in C++.

If our type acts as some kind of box, its good to use type parameters.

	Data Car = Car { company :: String
	               , model :: String
	               , year :: Int
	               } deriving (Show)
	               
	Can be re-written as:
	
	data Car a b c = Car { company :: a
	                     , model :: b
	                     , year :: c
	                     } deriving (Show)
	                     
	                     
This would not really benefit us though as we'd end up defining functions that only work on Car String String Int type.

A type class constraint:

	data (Ord k) => Map k v = .....
	
**Never add typeclass constraints in data declarations!**

# Derived Instances

A Type Class is like an interface.  We don't make instances from them.  We create a data type and determine what it can act like.

If the data type can act like something that can be equated, we make it an instance of the **Eq** typeclass.  If it can act like something that can be ordered, it should be an instance of the **Ord** typeclass.

We make our data type an instance of a typeclass by
using the **deriving** keyword.

Ex.

	data Person = Person { firstName :: String
	                     , lastName :: String
	                     , age :: Int
	                     } deriving (Eq, Show, Read)
	                     
Read requires a explicit type annotation to tell Haskell which type we want to get as a result.

	read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
	
If we use the result of our read later on in a way that Haskell can infer that it should read it as a person, we don't need the type annotation.

	read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" == mikeD
	
We can also read parameterized types, but we have to fill in the type parameters. So we can't do

	read "Just 't'" :: Maybe a
	
	but we can do
	
	read "Just 't'" :: Maybe Char.
	
# Type Synonyms

Are only for making code more readable.

	type String = [Char]
	
# Defining a TypeClass
Use class keyword followed by typeclass name and a type variable.  Where is then used to define some function type declarations and function bodies.

	class Eq a where
		(==) :: a -> a -> Bool
		(/=) :: a -> a -> Bool
		x == y = not (x /= y)
		x /= y = not (x == y)
		
# Defining a type

With a single constructor, a `type` is similar to a C struct.

They can also act lake a C enum like:

	data roygbiv = Red
		         | Orange
				 | Yellow
				 | Green
				 | Blue
				 | Indigo
				 | Violet
				   deriving (Eq, Show)

The part before the = denotes the type, the part(s) after are the value constructors.

	data TrafficLight = Red | Yellow | Green
	
	data Shape = Circle Float Float Float | Rectangle Float Float Float Float
	
Value constructors can take some values as parameters and produce a new value.
Type constructors can take types as parameters to product new types.

	data Maybe a = Nothing | Just a
	
The a is a type parameter and because there is a type parameter, Maybe is a *type constructor*
	
Exporting the functions and types we defined in a module would look like:

	module Shapes
	( Point(..)
	, Shape(..)
	, surface
	, nudge
	, baseCircle
	, baseRect
	) where
	
# Deriving TypeClass instances by hand
By doing this you can then override methods defined in the typeclass.

	instance Eq TrafficLight where
		Red == Red = True
		Green == Green = True
		Yellow == Yellow = True
		_ == _ = False
		
# Monoids
A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function. When something acts as an identity with respect to a function, it means that when called with that function and some other value, the result is always equal to that other value. 1 is the identity with respect to * and [] is the identity with respect to ++.

	class Monoid m where  
    	mempty :: m  
    	mappend :: m -> m -> m  
    	mconcat :: [m] -> m  
    	mconcat = foldr mappend mempty
    	
### Monoid Laws

	mempty `mappend` x = x
	x `mappend` mempty = x
	(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
	
## Rember fancy values, values with contexts

Functor:

	fmap (++"!") (Just "wisdom")
	
The Just "wisdom" is a fancy value and can be fmapped over so it is a functor.

Applicative Functor:

	 Just (+3) <*> Just 3
	 
Applicative Functor deals with taking a fancy value (possibly a function that is itself a fancy value) and applying it to another fancy value.
