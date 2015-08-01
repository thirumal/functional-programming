# Structure and Interpretation of Computer Programs

Exercises and textbook examples

## Chapter 1: Building Abstractions with Procedures

* Approximating ![sqrt(x)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%7Bx%7D) by Newton's method
	* [Simple method](../../../blob/24d54bcd15e891d3746b673858d2a0fb636403b0/sicp/sqrt.rkt#L39)
	* [Abstracted](../../../blob/9ae2218aa78370b016162b89e271aef9a2922561/sicp/sqrt-abstracted.rkt#L12-L23)
* Approximating ![cube-root(x)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%5B3%5D%7Bx%7D) by Newton's method
* Finding ![n!](http://latex.codecogs.com/png.latex?%5Cinline%20n%21)
* Ackerman function
* Fibonacci number
* Find sum of squares of two largest of three numbers
* Coin change problem (Combinatorics)
* Pascal triangle elements
* Approximating the ![sin](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csin) function
* Exponentiation and fast exponentiation algorithms
* Karatsuba multiplication
* Greatest common divisor by Euler's method
* Primality testing algorithms
	* Brute force algorithm (Search till ![sqrt(n)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%7Bn%7D))
	* Fermat's little theorem
	* Miller Rabin primality test (probabilistic)
* Higher Order procedures
	* Interval sum ![\[a,b\]](http://latex.codecogs.com/png.latex?%5Cinline%20%5Ba%2Cb%5D): ![Interval sum from a to b](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csum_%7Ba%7D%5E%7Bb%7D)
	* Simple sum approximation of ![integration](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cint_%7Ba%7D%5E%7Bb%7Df%28x%29)
* Simpson's approximation for  ![integration](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cint_%7Ba%7D%5E%7Bb%7Df%28x%29)
* Higher Order procedures
	* Interval product ![\[a,b\]](http://latex.codecogs.com/png.latex?%5Cinline%20%5Ba%2Cb%5D) : ![Interval product from a to b](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cprod_%7Ba%7D%5E%7Bb%7D)
* Accumulate and filtered Accumulate design patterns
* ![Lambda](http://latex.codecogs.com/png.latex?%5Cinline%20%5Clambda) functions
* Find a root of ![f(x)](http://latex.codecogs.com/png.latex?%5Cinline%20f%28x%29) using half interval method
* Fixed point function and infinite continuous fractions
	* Approximating ![sqrt(x)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%7Bx%7D)
	* Approximating ![cuberoot(x)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%5B3%5D%7Bx%7D)
	* Approximating ![phi](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cphi) (Fibonacci golden ratio)
	* Approximating ![e](http://latex.codecogs.com/png.latex?%5Cinline%20%5Ctextup%7Be%7D)
	* Approximating ![tan](http://latex.codecogs.com/png.latex?%5Cinline%20%5Ctan)
* Procedures as return values
	* Average damping, ![(x + f(x))/2](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cfrac%7Bx%20&plus;%20f%28x%29%7D%7B2%7D)
	* Finding the derivative, ![d/dx(f(x))](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cfrac%7B%5Cmathrm%7Bd%7D%7D%7B%5Cmathrm%7Bd%7D%20x%7Df%28x%29)
	* Newton's transform, ![Newton's transform equation](http://latex.codecogs.com/png.latex?%5Cinline%20x_%7Bn+1%7D%20%3D%20x_n%20-%20%5Cfrac%7Bg%28x%29%7D%7B%5Cfrac%7B%5Cmathrm%7Bd%7D%20g%28x%29%7D%7B%5Cmathrm%7Bd%7D%20x%7D%7D)
	* Approximating ![nroot(x)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%5Bn%5D%7Bx%7D) by Newton's method (Newton's transform)
	* Fixed point of a transform
	* Solve cubic equations of the form ![x^3 + ax^2 + bx + c](http://latex.codecogs.com/png.latex?%5Cinline%20x%5E3%20&plus;%20ax%5E2%20&plus;%20bx%20&plus;%20c) by Newton's method
	* Composition of functions: ![f o g(x)](http://latex.codecogs.com/png.latex?%5Cinline%20f%20%5Ccirc%20g%20%28x%29)
	* Repeated application of a function: ![f(f(...(f(x))...))](http://latex.codecogs.com/png.latex?%5Cinline%20f%28f%28...%28f%28x%29%29...%29%29) or in other words ![f o f o ... f(x)](http://latex.codecogs.com/png.latex?%5Cinline%20f%20%5Ccirc%20f%20%5Ccirc%20...%20%5Ccirc%20f%28x%29)
	* Approximating ![nroot(x)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Csqrt%5Bn%5D%7Bx%7D) by repeated average damping
	* `iterative-improvement` design pattern

## Chapter 2: Building Abstractions with Data

* Pairs
	* Representing rational numbers as a pair of numbers
	* Rational number arithmetic
	* Representing point as pairs of numbers
	* Representing a line segment as pair of points
	* Midpoint of a line segment
	* Representing a rectangle as a pair of points
	* Area and the perimeter of a rectangle
* Various Representation of pairs
	* Procedural representations of pairs
	* Exponentiation representation of pairs
* Church Numerals
* Interval arithmetic ![\[a,b\]](http://latex.codecogs.com/png.latex?%5Cinline%20%5Ba%2Cb%5D)
	* Addition, subtraction, multiplication and division of intervals
	* Fast interval multiplication algorithm
	* Center, width (tolerance) and the percentage width of an interval
	* Approximating the product of tolerances
* Sequences and Lists
	* List as a combination of pairs
	* Nth element of a list
	* Length of a list
	* Appending a list behind another
	* Reversing a list
	* Coin change problem revisited
	* Dotted tail notation in Scheme
	* Finding numbers of the same parity
* `map` design pattern
	* Scaling every element in a list
	* Implementation of `map` function
	* Implementation of the `for-each` design pattern
* Hierarchical structures (trees)
	* Representing a tree as a composite list
	* Counting leaf nodes in a tree
	* Deep-reversing a list/tree
	* Flattening a composite list/tree
	* Binary mobiles and computations
		* Torque of a branch
		* Balancedness of a binary mobile
* `map` design pattern over trees
	* Scaling a tree
	* Squaring every element of tree
	* Using `map` to abstract and in turn create `tree-map`
	* Power set of a given set, ![P(S)](http://latex.codecogs.com/png.latex?%5Cinline%20%5Cmathcal%20P%20%5Cleft%28%7BS%7D%5Cright%29)
* Sequences as conventional interfaces
	* The need for conventional interfaces
	* `accumulate`, `map`, `filter` and `enumerate` interfaces
	* Defining map, append and length of a sequence using accumulate
	* Horner's rule for evaluating polynomials
	* `accumulate-n` design pattern
	* Matrix and vector multiplications
	* `fold-left` and `fold-right` (a.k.a accumulate) operations
	* Expressing reversal of a list in terms of fold-left and fold-right
* Nested mapping
	* Generate all pairs (i j), such that ![1 <= j < i <= n](http://latex.codecogs.com/png.latex?1%20%5Cleq%20j%20%3C%20i%20%5Cleq%20n)
	* `flatmap` design pattern
	* Get all pairs which sum up to a prime number less than a given number
	* Fetch all permutations of a set ![S](http://latex.codecogs.com/png.latex?S)
	* Generate triplets (i j k) such that ![1 <= k < j < i <= n](http://latex.codecogs.com/png.latex?1%20%5Cleq%20k%20%3C%20j%20%3C%20i%20%5Cleq%20n)
	* N-Queens problem of placing `N` queens on a `N * N` chess board
