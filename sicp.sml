(* https://emunix.emich.edu/~shaynes/341/wi10/Lectures/ML/operators.html *)


(* some helper functions *)
fun commute f a b    = f b a 
val com' = commute      : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

fun curry f a b      = f (a, b)
val c' = curry       : ('a * 'b -> 'c) -> 'a -> 'b -> 'c 

fun uncurry f (x, y) = f x y
val uc' = uncurry    : ('a -> 'b -> 'c) -> 'a * 'b -> 'c

fun apply f a        = f a
val a' = apply       : ('a -> 'b) -> 'a -> 'b

fun pass a f         = f a
val p' = pass        : 'a -> ('a -> 'b) -> 'b

fun reflex f a         = f a a
val r' = reflex        : ('a -> 'a -> 'b) -> 'a -> 'b

(* p is monadic pure? *)
fun initfoldr d _ _ []      = d   (* default *)
  | initfoldr _ p f (x::xs) = let 
  fun iter acc [] = acc
    | iter acc (x'::xs') = iter (f acc x') xs'
  in iter (p x) xs end
val _ = initfoldr : 'a -> ('b -> 'a) -> ('a -> 'b -> 'a) -> 'b list -> 'a

fun initfoldl d p f = initfoldr d p f  o  rev
val _ = initfoldl : 'a -> ('b -> 'a) -> ( 'a -> 'b -> 'a) -> 'b list -> 'a

val id : 'a -> 'a = fn x => x 

(* 
(* recursive process *)
fun zipWith f a b = 
case (a , b )
  of ([], _ )       => []
   | (_ , [])       => []
   | (x::xs, y::ys) => f x y :: zipWith f xs ys 
*)

(* iterative process *)
fun zipWith f a b = let 
  fun iter a b acc = 
  case (a , b )
    of ([], _ )       => acc
     | (_ , [])       => acc
     | (x::xs, y::ys) => iter xs ys (f x y::acc)
  in rev ((iter : 'a list -> 'b list -> 'c list -> 'c list) a b []) end 
val _ = zipWith : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list







(* 1.1   The Elements of Programming *)
(* 1.1.1 Expressions *)

val printintNL = print o (curry (op ^) "\n") o Int.toString  : int -> unit 

(* ; map printintNL [486, 137  + 249, 1000 - 334, 5 * 99, 10 div 5] *)
(* ; print (Real.toString (2.7 + 10.0) ^ "\n") *)
;
( zipWith apply (map (initfoldr 0 id o curry) [op +, op -]) [[21, 35, 12, 7], [25, 4, 12]]
; 3 * 5 + 10 - 6
; ((3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6))
)
;
(* 1.1.2 Naming and the Environment *)
let
  val size          = 2                 : int
  val pi            = 3.14159           : real
  val radius        = 10.0              : real
  val circumference = 2.0 * pi * radius : real
in size ; circumference end 

(* 1.1.3 Evaluating Combinations *)
; (2 + (4 * 6)) * foldr (op +) 0 [3, 5, 7]
;

(* Compound Procedures *)
val squareInt  : int  -> int  = fn x => x * x
val squareReal : real -> real = fn x => x * x

fun sumOfSquaresInt  a b = (foldr (op +) 0 o map squareInt)  [a, b]
val _ = sumOfSquaresInt : int -> int -> int

fun sumOfSquaresReal a b = (foldr (op +) 0.0 o map squareReal) [a, b]
val _ = sumOfSquaresReal : real -> real -> real

(* 
 * ; sumOfSquaresInt 3 4
 * ;
 * val f__ = fn a => sumOfSquaresInt (a + 1) (a * 2)
 * ; f__ 5 
 * ; 
 *)

(* 1.1.6 Conditional Expression and Predicates *)
val absInt  : int -> int   = fn x =>
case (op Int.compare) (x, 0) of GREATER => x | EQUAL => 0 | LESS => ~x

val absInt : int -> int = fn x => if x < 0 then ~x else x

(* infix 3 ge *)
(* fun op ge (a, b) = a > b orelse a = b ;; val _ : int * int -> bool = op ge *)
(* fun op ge (a, b) = not (a < b) *)
(* ; 5 ge 4 *)
; 
(* Exercise 1.3 *)
fun ex_1_3 a b c = let 
    val c1 = a < b andalso a < c
    val c2 = b < c
  in (uncurry sumOfSquaresInt) 
    ( case (c1, c2) 
    of (true, _) => (b, c)
     | (_, true) => (a, c)
     | _         => (a, b)
    )
  end
val _ = ex_1_3 : int -> int -> int -> int
; ex_1_3 3 2 1
;

(* 1.1.7 Example: Square Roots by Newtons's Method *)



(* fun goodEnough g x = Real.abs (g*g - x) < 0.001 *)
fun sqrt x = let
    (* g : guess *)
    fun goodEnough g x = Real.abs ((g*g - x) / g ) < 0.001
    val _ = goodEnough : real -> real -> bool

    fun average x' y   = (x' + y) / 2.0
    val _ = average : real -> real -> real

    fun improve g x'   = average g (x' / g)
    val _ = improve : real -> real -> real

    fun sqrtIter g x'  = if goodEnough g x' then g else sqrtIter (improve g x') x'
    val _ = sqrtIter : real -> real -> real
  in
    sqrtIter 1.0 x
  end
val _ = sqrt : real -> real

; map (squareReal o sqrt) [9.0, (100.0 + 37.0), 1000.0] ;

fun sqrt2 x = let
    (* condensed version *)
    (* g : guess *)
    fun goodEnough g = Real.abs ((g*g - x) / g) < 0.001
    fun improve g    = (g + (x / g)) / 2.0
    fun sqrtIter g   = if goodEnough g then g else sqrtIter (improve g)
  in
    sqrtIter 1.0
  end
val _ = sqrt2 : real -> real

; map (squareReal o sqrt2) [9.0, (100.0 + 37.0), 1000.0] ;



(* from this point onwards a more stratified operator precedence *)
infix 6 ^   val sCat  = curry op ^
infix 6 +   val iPlus = curry op Int.+   val rPlus = curry op Real.+ 
infix 6 -   val iSub  = curry op Int.-   val rSub = curry op Real.- 
infix 6 *   val iMul  = curry op Int.*   val rMul = curry op Real.* 
infix 6 /   val rDiv  = curry op /
infix 6 div val iDiv  = curry op div
infix 6 mod val iMod  = curry op mod
infix 6 ::  fun cons a b = a :: b
infix 6 @   fun lCat a b = a @ b
infix 6 >   val gt    = curry op >
infix 6 <   val lt    = curry op <
infix 6 <=  val le    = curry op <=
infix 6 >=  val ge    = curry op >=
infix 6 =   fun eq a b = a = b
infix 6 <>  fun ne a b = a <> b

(* http://mlton.org/InfixingOperators *)
(* adjusted for my tastes *)
infix  8 >&     fun x >& f = fn y => f (x, y)     (* Left section      *)
infix  8 @<     fun f @< y = f y                  (* Left application  *)
infix  8 &<     fun f &< y = fn x => f (x, y)     (* Right section     *)
infix  8 >@     fun x >@ f = f x                  (* Right application *)

infix  8 o  (* See motivation below *)
infix  0 :=
infix  1 >|     val op>| = op>@      (* Left pipe *)
infix  1 |<     val op|< = op@<      (* Right pipe *)
;
(* J *)
infix 8 <>: fun u <>: v   = fn x => fn y => u (v x) (v y) (* over *)
            fun over a b  = c' op<>: a b
infix 8 @:  fun u @: v    = fn x => fn y => u (v x y)     (* atop *)
            fun atop a b  = c' op@: a b   (* atop *)
infix 8 >:  fun u >: v    = fn x => fn y => u x (v y)     (* right over / hook *)
            fun rHook a b = c' op>: a b
infix 8 <:  fun u <: v    = fn x => fn y => u (v x) y     (* left over  /rev hook *)
            fun lHook a b = c' op<: a b

fun W (f : 'a -> 'b) (g : 'b -> 'c -> 'd) (h : 'a -> 'c) (a : 'a) 
= g (f a) (h a)
fun W' (f : 'a -> 'b -> 'c) (g : 'c -> 'd -> 'e) (h : 'a -> 'b -> 'd) a b 
= g (f a b) (h a b)

fun F (g : 'b -> 'c -> 'd) (f : 'a -> 'b)  (h : 'a -> 'c) (a : 'a)
 = g (f a) (h a)
fun F' (g : 'c -> 'd -> 'e)(f : 'a -> 'b -> 'c)  (h : 'a -> 'b -> 'd) a b 
= g (f a b) (h a b)

fun const a b = a

(* Exercise 1.7 *)
(* implemented above *)

(* Exercise 1.8 *)
fun cube (x : real) = x*x*x
val _ = cube : real -> real 

val cbrt_ex_1_8 : real -> real = fn x => let
  (* g : guess *)
  val goodEnough = (op< &< 0.001) o Real.abs o r' (rDiv <: (op- &< x o cube))
  val improve    = (op/ &<3.0) o W (rDiv x o squareReal) rPlus (rMul 2.0)
  fun iter g     = if goodEnough g then g else (iter o improve) g 
in
  iter 1.0
end

; map (squareReal o sqrt2) [27.0, (100.0 + 37.0), 1000.0] ;


(* 1.2 Procedures and the Process They Generate *)

(* 1.2.1 Linear Recursion and Iteration *)

fun factorial n = if n = 1 then 1 else n * factorial (n - 1)
; factorial 6 ;

fun factorial_2 n = let
  fun iter p c (* product counter*) = if c>n then p else iter (c * p) (c + 1)
in
  iter 1 1
end
; factorial_2 6 ;

(* Exercise 1.9 *)
fun plus_ex_1_9a a b = if a=0 then b else 1 + plus_ex_1_9a (a-1) b (* rec *)
fun plus_ex_1_9b a b = if a=0 then b else plus_ex_1_9b (a-1) (b+1) (* iter *)
; plus_ex_1_9a 5 8
; plus_ex_1_9b 5 8
;

(* Exercise 1.10 *)
fun ack _ 0 = 0
  | ack 0 y = 2*y
  | ack _ 1 = 2
  | ack x y = ack (x-1) (ack x (y-1))


; map (initfoldr 0 id ack) [[1, 10], [2, 4], [3, 3]] ;

(* 1.2.2 Tree Recursion *)

fun fib_1 n = 
case n 
of 0 => 0 
 | 1 => 1 
 | _ => W (op- &<1) (iPlus <>: fib_1) (op- &<2) n
val _ = fib_1 : int -> int

fun fib_2 n = let 
  fun iter _ b 0 = b
    | iter a b c = iter (a+b) a (c-1)
in
  iter 1 0 n
end         
val _ = fib_2 : int -> int

; fib_1 4 ; fib_2 4;


fun countChange amount = let
  (* k:kindsOfCoins *)
  fun firstDenomination k = 
    case k of 1=>1 | 2=>5 | 3=>10 | 4=>25 | 5=>50 | _=>0
  fun cc a k= 
    if a=0            then 1 else 
    if a<0 orelse k=0 then 0 else 
    cc a (k-1) + cc (a - firstDenomination k) k
in
  cc amount 5
end
val _ = countChange : int -> int

; countChange 100 ;

(* Exercise 1.11 *)
fun f_ex_1_11a num = 
  if num<3 then num else 
let fun p f' = W' (com' const) iMul (f' @: iSub)
in  map (p f_ex_1_11a num) [1, 2, 3] >| foldr op+ 0
end 
val _ = f_ex_1_11a : int -> int

val f_ex_1_11b = 
let 
  fun iter f3 f2 f1 n =
    if n<2 then n  else
    if n<3 then f1 else
    iter f2 f1 (f1 + (2*f2) + (3*f3)) (n-1)
in  
  iter 0 1 2
end  
val _ = f_ex_1_11b : int -> int

; f_ex_1_11a 5 ;
; f_ex_1_11b 5 ;


(* Exercise 1.12 *)

val dec : int -> int = op Int.- &< 1 
val inc : int -> int = op Int.+ &< 1 
fun pascal_ex_1_12 (row : int) (col : int) =
  (* (row column) 1 based indexing, 0 on out of bounds
   * 1
   * 1 1
   * 1 2 1
   * 1 3 3 1
   * 1 4 6 4 1 
   *)
  if row<0 orelse col<0 orelse row<col then 0 else
  if col=1 orelse row=col              then 1 else
  W' over (F' iPlus) lHook pascal_ex_1_12 dec row col

; pascal_ex_1_12 5 3 ;

(* Exercise 1.13 *)

fun proveFib_ex_1_13 n = let
  val phi = 1.0 + sqrt 5.0 / 2.0
  fun p pow acc = if pow=n then acc / sqrt 5.0 else p (pow+1) (acc*phi)
in
  (c' op> 1.0  o  abs  o  com' rSub (p 0 1.0)  o  Real.fromInt  o  fib_2) n
end

; proveFib_ex_1_13 20 ;

(* Exercise 1.15 *)

val ex_1_15 = let
  val p = W (rMul 3.0) rSub (rMul 4.0 o cube)
  fun sine a(* angle *) = if not (Real.abs a > 0.1) then a else
    p o sine  |<  a / 3.0
in
  sine 12.15
end

(* 1.2.4 Exponentiation *)
fun expt_1 b n = if n=0 then 1.0 else b * expt_1 b (n-1)

fun expt_2 b n = let
  (* c:counter p:product*)
  fun iter c p = if c<0 then p else iter (c-1) (b*p)
in 
  iter n 1.0
end

; expt_1 2.0 3 ; expt_2 2.0 3 ;

fun even n = n mod 2 = 0

fun fast_expt _ 0 = 1.0
  | fast_expt (b : real) (n : int) = 
    if even n then (n mod 2 :int) >@ fast_expt b >@ squareReal else 
    b * fast_expt b (n-1)

(* Exercise 1.16 *)

val fast_expt_ex_1_16 = let
  fun iter a _ 0 = a
    | iter a b n = if even n then iter a (b*b) (n mod 2) else iter (a*b) b (n-1)
in
  iter 1
end

(* Exercise 1.17 *)
val iDouble = 2 >& op Int.*
val rDouble = 2.0 >& op Real.*
val iHalve  = Int.quot &< 2
val rHalve  = op Real./ &< 2.0

fun mul_ex_1_17 0 _ = 0
  | mul_ex_1_17 _ 0 = 0
  | mul_ex_1_17 a 1 = a
  | mul_ex_1_17 a b = if even b then mul_ex_1_17 (iDouble a) (iHalve b) else a + mul_ex_1_17 a (b-1)

; mul_ex_1_17 4 5 ;

(* Exercise 1.18 *)

val mul_ex_1_18 = let
  fun iter acc _ 0 = acc
    | iter acc a b = if even b then iter acc (iDouble a) (iHalve b) else iter (acc+a) a (b-1)
in
  iter 0
end
; mul_ex_1_18 2 4 ;

(* Exercise 1.19 *)

val fib_3 = let 
  fun iter _ b _ _ 0 = b 
    | iter a b q p count = if even count 
        then iter a 
                  b 
                  ((p*p) + (q*q)) 
                  ((2*p*q) + (q*q)) 
                  (count div 2)
        else iter ((b*q)+(a*q)+(a*p))  
                  ((b*p)+(a*q))  
                  p  
                  q  
                  (count-1)
in
  iter 1 0 0 1
end

; map fib_3 [1, 2, 3, 4, 5, 6, 7, 8, 9] ;

(* 1.2.5 Greatest Common Divisors *)

fun gcd a b = if b=0 then a else gcd b (a mod b)
; gcd 206 40 ;

(* Exercise 1.20 *)
(* 18 *)

(* 1.2.6 Example Testing for Primality *)
val divides = W' (com' iMod) eq (fn _ => fn _ => 0)
fun findDivisor n t =
  if squareInt t>n then n else 
  if divides t n   then t else
  findDivisor n (t+1)
val smallestDivisor = com' findDivisor 2
fun prime v = if v=1 then false else r' (eq >: smallestDivisor) v


; map prime [1,2,3,4,5,6,7,8,9] ;

(* The Fermat Test *)

fun expmod _ 0 _      = 1
  | expmod base exp m = let fun aux i j = com' iMod m o i @< expmod base (j exp ) m
in
  if even exp 
  then aux squareInt   (com' iDiv 2) 
  else aux (iMul base) dec
end

; (expmod 10 10 7) ;

val now = LargeInt.toInt o Time.toNanoseconds o Time.now
fun random 0 = 0
  | random m = (Random.randInt o Random.rand)(now(),now()) mod m

; map random [1,2,3,4,5,6,7,8,9] ;

fun fermatTest n = let 
  fun tryIt a = a = expmod a n n
in
  (tryIt o inc o random o dec) n
end

; map (fn b => if b then 1 else 0) |< map fermatTest [1,2,3,4,5,6,7,8,9];

fun fastPrime _ 0 = true
  | fastPrime n times = if fermatTest n then fastPrime n (times-1) else false


; map (fn b => if b then 1 else 0) |< map (com' fastPrime 20) [1,2,3,4,5,6,7,8,9] ;

(* Exercise 1.21 *)
fun range from till = let 
  fun iter acc curr = if curr=from then acc else iter (curr::acc) (curr-1)
in
  iter [] till
end
(* ; range 1 10 ; *)

; map smallestDivisor [199, 1999, 19999] ;

(* Exercise 1.22 *)

fun reportPrime (elapsedTime : int) = print (" *** " ^ Int.toString elapsedTime)
; reportPrime 5 ;
fun startPrimeTest n startTime = if prime n then reportPrime (now () - startTime) else ()
fun timedPrimeTest n = (print ("\n" ^ Int.toString n) ; startPrimeTest n (now ()))

fun searchForPrimes from till = let 
  fun done (_ : unit) = print "\nDONE\n"
  fun iter curr = let
    val next = curr + 2
  in
    timedPrimeTest curr
  ; if next>till then done() else iter next
  end
  fun beginWith2 (_ : unit) = (timedPrimeTest 2 ; iter 3 ; print "\n")
in
  if from>=till              then done() else
  if from<=2 andalso till>=2 then beginWith2 () else
  iter (if even from then from+1 else from)
end

; searchForPrimes 500 1000 ;

(* skip exercises 1.23 1.24 1.25 1.26 1.27 1.28 *)

(* 1.3 Formulation Abstractractions with Higher-Order Procedures *)

(* 1.3.1 Procedures as Arguments *)

fun sumIntegers a b =
  if a>b then 0 else a + sumIntegers (a + 1) b
fun sumIntegersIter a b = let 
  fun iter acc a' = 
    if a' >b 
    then acc 
    else iter (acc+a') (a'+1)
in
  iter 0 a
end

; sumIntegers 1 10 ;
; sumIntegersIter 1 10 ;

fun sumCubes a b = if a>b then 0.0 else cube a + sumCubes (a+1.0) b

fun sumCubesIter a b = let
  fun iter acc a' = 
    if a' >b 
    then acc 
    else iter (acc+ cube a') (a' +1.0)
in
  iter 0.0 a
end


fun piSum a b = if a>b then 0.0 else ( 1.0 / (a+2.0 * a)) + piSum (a+4.0) b

fun piSumIter a b = let
  fun iter acc a' = 
    if a' >b 
    then acc 
    else iter (1.0 / (a' + 2.0 * a') + acc) (a' +4.0)
in
  iter 0.0 a
end

(* ; 8.0 * piSum 1.0 1000.0 ; *)

fun iSum term next (a : int) (b : int) = if a>b then 0   else term a + iSum term next (next a) b
fun rSum term next (a : real) (b : real) = if a>b then 0.0 else term a + rSum term next (next a) b

fun iSumIter term next (a : int) (b : int) = let
  fun iter acc a' = 
    if a' >b 
    then acc 
    else iter (acc+ term a') (next a')
in
  iter 0 a
end

fun rSumIter term next (a : real) (b : real) = let
  fun iter acc a' = if a' >b then acc else iter (acc+ term a') (next a')
in
  iter 0.0 a
end

fun iInc a = a+1
fun rInc a = a+1.0

val rSumCubes2 = rSum (fn a => a*a*a ) rInc
val iSumCubes2 = iSum (fn a => a*a*a)  iInc

; rSumCubes2 1.0 10.0 ;
; iSumCubes2 1 10 ;

val identity = id
val sumIntegers2 = iSum id iInc

; sumIntegers2 1 10 ;

val piSum2 = let 
  val piTerm = rDiv 1.0 o r' (rMul >: rPlus 2.0)
  val piNext = rPlus 4.0
in
  rSumIter piTerm piNext
end

; 8.0 * piSum2 1.0 1000.0 ;

fun integral f a b dx = let
  val addDx = rPlus dx
in
  dx * rSumIter f addDx (dx/2.0 + a) b
end

; integral (fn x => x*x*x) 0.0 1.0 0.01  ;
; integral (fn x => x*x*x) 0.0 1.0 0.001 ;

(* Exercise 1.29 *)

fun simpson f n a b = let
  val h        = b-a / n
  val yk       = f o (rPlus a) o (rMul h)
  val oddTerm  = (rMul 4.0) o yk
  val evenTerm = (rMul 2.0) o yk
in
  rDiv h 3.0 * ( yk 0.0 
               + yk n
               + rSumIter oddTerm (rPlus 2.0) 1.0 (n-1.0)
               + rSumIter evenTerm (rPlus 2.0) 2.0 (n-1.0)
               )
end

; simpson cube 100.0  0.0 1.0 ;
; simpson cube 1000.0 0.0 1.0 ;

(* Exercise 1.30 *)
(* written above *)

fun rProductRec term next (a : real) (b : real) = 
  if a > b 
  then 1.0 
  else term a * rProductRec term next (next a) b

fun rProductIter term next (a : real) (b : real) = let 
  fun iter acc a =
    if a > b
    then acc
    else iter (acc * term a) (next a)
in
  iter 1.0 a
end

val factorial_3 = rProductIter id (rPlus 1.0) 1.0
; factorial_3 6.0 ;

val tauDiv4 = let 
  fun term k = if (even o Real.floor o rPlus 0.5) k then k / (k+1.0) else k+1.0 / k 
in 
  rProductIter term (rPlus 1.0) 1.0 
end

; tauDiv4 1000.0 * 4.0 ;
val pi = 2.0 * tauDiv4 1000000.0

(* Exercise 1.32 *)

fun rAccumulateRec combiner nullValue term next a b =
  if a > b 
  then nullValue
  else combiner (term a) (rAccumulateRec combiner nullValue term next (next a) b)

fun rAccumulateIter combiner nullValue term next (a : real) (b : real) = let
  fun iter acc a =
    if a>b 
    then acc
    else iter (combiner acc o term |< a) (next a)
in
  iter nullValue a
end

(* sum *)
val rInc = rPlus 1.0

val rSum3 = rAccumulateIter rPlus 0.0
; rAccumulateIter rPlus 0.0 id rInc 1.0 10.0 ;
; rSum3 id rInc 1.0 10.0;

(* product *)
val rProduct3 = rAccumulateIter rMul 1.0
; rAccumulateIter rMul 1.0 id rInc 1.0 6.0 ;
; rProduct3 id rInc 1.0 6.0 ;