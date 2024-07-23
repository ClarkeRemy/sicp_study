NB. cocurrent 'SICP'

NB. 1.1 The Elements of Programming
NB. 1.1.1 Expressions

u_LAcc =. {{ acc =. {. y for_i. }. y do. acc =. acc u i end. acc }}
float_fmt =: 0j20&":
uv =. {{u v}}

NB. 486 ,(137 + 249),(1000 - 334),(5 * 99),(10%5),(2.7 + 10)
NB. (+/ 21 35 12 7),(*/ 25 4 12)
NB. (3*5)+10-6
NB. (3 * (2*4) + 3+5) + (10-7)+6

NB. size =.2 [ pi =. 3.14159 [ radius =. 10
NB. pi * radius * radius
NB. circumference =. */ 2, pi, radius
NB. circumference

NB. 1.1.3 Evaluating Combinations

NB. (2 + 4 * 6) * +/ 3 5 7

NB. 1.1.4 Compound Procedures

square =: *:
NB. (square 21), (square 2 + 5), square square 3

sum_of_squares =: $:~ : (+&:square)
3 sum_of_squares 4

NB. f =: >: sum_of_squares +:
NB. f 5

NB. 1.1.6 Conditionak Expressions andPredicates
NB. abs =: {{
NB. if. y>0 do.  y return. end.
NB. if. y=0 do.  0 return. end.
NB. if. y<0 do. -y return. end.
NB. }}
NB. abs =: ]`- @.(<&0)
abs =: | : [:

NB. ge =: > +. =
ge =: -.@:<

NB. Exercise 1.2
%/(+/5 4,-/2,-/3,+/6,%/4 5x), */3, (-/6 2),-/2 7

NB. Exercise 1.3
ex_1_3 =: {{)m
'a b c' =. y
if. b*.&:(a&<)c do. b sum_of_squares c return. end.
if. b < c       do. a sum_of_squares c return. end.
a sum_of_squares b
}}
NB. ex_1_3 |.1 2 3

good_enough =: (|@:- *:)~ < 0.001"_
average     =: (+/ % #) : ($:@,"_1)
improve     =: [ average %~
sqrt_iter   =: (improve $: ])`[@.good_enough
sqrt        =: 1&sqrt_iter

NB. float_fmt (sqrt 9), (sqrt 100 + 37), (square sqrt 1000)

NB. Exercise 1.7
good_enough =: 0.001 > (|@:- *:)~ % [

NB. float_fmt (sqrt 9), (sqrt 100 + 37), (square sqrt 1000)

NB. Exercise 1.8
cube =: ^&3
good_enough_ex_1_8  =. 0.001 > (|@:- cube)~ % [
improve_cbrt_ex_1_8 =. (% *:)~ %&3@+ +:@[
cbrt_iter_ex_1_8    =. (improve_cbrt_ex_1_8$:])`[@.good_enough_ex_1_8
cbrt_ex_1_8         =. 1&cbrt_iter_ex_1_8

NB. float_fmt (cbrt_ex_1_8 27), (cbrt_ex_1_8 100 + 37), (cube cbrt_ex_1_8 1000)

NB. 1.1.8 Procedures as Black-Box Abstractions


NB. 1.2 Procedures and the Process They Generate

NB. 1.2.1 Linear Recursion and Iteration

factorial =. (* $:@:<:)~`1:@.(1&=)
NB. factorial 6

factorial_2 =. {{)m
  iter =. (* $: >:@])`[@.(>&y@]) NB. product iter counter
  iter/ 1 1
}}
NB. factorial_2 6

NB. Exercise 1.9
plus_ex_1_9a =. >:@:(<:@[ $: ])`]@.(0=[)
plus_ex_1_9b =. (<:@[ $: >:@:])`]@.(0=[)
NB. plus_ex_1_9a/ 5 8
NB. plus_ex_1_9b/ 5 8

NB. Exercise 1.10
ack_ex_1_10 =. 0:`(+:@])`2:`(<:@[ $: ($:<:)) @. (1 i.~  ,~&:(0&=), 1 = ]) "0
NB. ack_ex_1_10/ 1 10
NB. ack_ex_1_10/ 2 4
NB. ack_ex_1_10/ 3 3

NB. 1.2.2 Tree Recursion
fib_1 =. 0:`1:`((+&$: <:)@<:) @. (=&0 1 i. 1:) "0
fib_2 =.{{ 
  iter=:  ([:$: +/@:}:,  {.,  <:@{:) ` (1&{)  @. (0={:)
  iter 1 0 , y
}}"0
NB. fib_1 6
NB. fib_2 6

count_change =. {{)m
  first_denomination =. {&0 1 5 10 25 50
  cc =. 1:`0:`(($:<:) + ((-first_denomination) $: ])) @. (1 i.~ 0&=@[, <&0@[+. 0&=@])  NB. amound cc kind
  y cc 5
}}
NB. count_change 100

NB. Exercise 1.11
f_ex_1_11a =. +/@:((]* $:@-)&1 2 3)`]@.(<&3)
f_ex_1_11b =. {{)m
  iter =. {:`(2&{)`([:$: 1 2&{, +/@:(3 2 1*}:), <:@{:)@.(1 i.~ <&2 3@{:) NB. iter (f-3, f-2, f-1, n)
  iter 0 1 2, y
}}"0
NB. f_ex_1_11a 5
NB. f_ex_1_11b 5

NB. Exercise 1.12
NB. 1
NB. 1 1
NB. 1 2 1
NB. 1 3 3 1
NB. 1 4 6 4 1

NB. (row, col) one based indexing, 0 on out of bounds
pascal_ex_1_12 =. 0:`1:`($:@:<: + $:@:(-&1 0)) @. (1 i.~ (+./@:(0&=)+. </), (1={:)+.=/) NB."1
NB. pascal_ex_1_12 5 3

prove_fib_ex_1_13 =. {{
  phi =. 2%~1+%:5
  NB. p pow`acc
  p   =. $:@:(>:`(phi&*)"0)`({:% %:@:5:) @. (y={.) "1
  1>|(p 0 1)-fib_2 y
}}"0
NB. prove_fib_ex_1_13 i.20

NB. Orders of Drowth

NB. Exercise 1.15
ex_1_15 =. {{
  p =. *&3 - *&4@:(^&3)
  sine =. p@:$:@:(%&3)`] @. (-.@:(0.1<|))"0
  sine 12.15
}}
ex_1_15 ''

NB. 1.2.4 Exponentiation

expt_1 =. ([*($:<:))`1: @. (0=])"0
NB. 2 expt_1 3

expt_2 =. {{)d
  iter =. (<:@[ $: x&*@])`] @. (0=[)
  y iter 1
}}"0
NB. 2 expt_2 3

even =. 0=2&|

fast_expt =. 1:`(*:@:$: %&2)`([*($:<:)) @. (1 i.~ (=&0,0=2&|)@])
NB. 2 fast_expt 3

NB. Exercise 1.16
fast_expt_ex_1_16 =. {{)d
  NB. iter acc`b`n
  iter =. {.`([:$: ]`*:`(%&2)"0)`([:$: */@}:, 1&{, <:@{:) @. (1 i.~ (=&0, 0=2&|)@{:)
  iter 1`x`y
}}"0
NB. 2 fast_expt_ex_1_16 3

NB. Exercise 1.17
mul_ex_1_17a =. ([+ ($: <:))`0: @. (0=])
NB. 4 mul_ex_1_17a 5
mul_ex_1_17b =. 0:`[`($:/@:(+:`(%&2)"0)@:,)`([+ (*<:)) @. (1 i.~ +./&:(0&=), (=&1, 0=2&|)@])
NB. 4 mul_ex_1_17b 5

NB. Exercise 1.18

mul_ex_1_18b =. {{
  iter =. {.`([:$: ]`+:`(%&2)"0)`([:$: +/@}:, 1&{, <:@{:) @. (1 i.~ (=&0, 0=2&|)@{:)
  iter 0`x`y
}}"0
NB. 4 mul_ex_1_18b 5

NB. Exercise 1.19

fib_3 =. {{)m
  NB. iter a`b`p`q`count
  cond =. 1 i.~ (0= ], 2%|)@{:
  b1 =. 1&{ 
  b2 =. 2&{., (+/@:*:, (+:@(*/) + *:@{:))@:(2 3&{), %&2@{:
  f =. +/@:(*/)@:{
  else =. (1 0 0,:3 3 2)&f, (1 0,:2 3)&f, 2 3&{, <:@{:

  iter =. b1`($:@:b2)`($:@:else) @. cond @:([echo)
  iter 1 0 0 1,y
}}"0
NB. fib_3 i.10

NB. 1.2.5

gcd =: (]$:|~)`[ @. (0=])
NB. 206 gcd 40
divides=:0=|~
NB. find_divsor =. [`]`($:>:) @. (1 i.~ (<*:),divides) "0  
find_divsor =: {{)d 
  iter =. (0;[;a:"_)`(0;];a:"_)`(1;(;>:)) @. (1 i.~ (<*:),divides) "0  
  while. cont['cont x y'=. x iter y do.end.
  x
}}"0
smallest_divisor =: find_divsor&2
is_prime =: {{)m
  if. y=1 do. 0 return. end.
  (= smallest_divisor)y 
}}"0
is_prime }.i. 10

NB. The Fermat test
m=: {{ ]`u`]"0 }}
r=:<.@:|
cond=: 1 i.~ (0= (, 2&|))@(1&{)
expmod =: 1:  `  ({:r *:@:$:@:(%&2 m))  `  ({:r ({.* $:@:(<:m)))  @. (cond f.) "1 : [:
expmod 10 10 7

fermat_test =: {{)m
  if. y=1 do. 0 return. end.
  (try_it =:  = expmod@:(,&(y`y)) ) ?&.:<:y
}} "0

 fermat_test 2+i.15

NB. n fast_prime times
fast_prime =. 1:`($:<:)`0: @. (1 i.~ 0&=@] , fermat_test@[ ) "0

(2+i. 15) fast_prime 20

NB. Exercise 1.21
smallest_divisor 199 1999 19999

NB. report-prime elapsedTime
report_prime     =: 0 0&$@:tmoutput@:(' *** ',":) "0
NB. n start_prime_test startTime
start_prime_test =: 0 0&$`(report_prime@:(6!:1@''-)) @. (is_prime@[) "0
NB. timed_prime_test n
timed_prime_test =: (start_prime_test 6!:1@'') [ tmoutput@:(CRLF,":)
timed_prime_test 5

search_for_primes =: {{
  echo 'what'
  done =. [:echo  CRLF,'DONE'"_
  if. x>=y do. done@'' end.

  if.       (x<:2) *. y>:2  do. timed_prime_test@2 curr=. 3
  else. if. (0=2&|) x       do. curr =. >: x
        else.                   curr =. x
        end.
  end.
  iter =. 0:@:done@''`]@.(y>])@:+&2 [ timed_prime_test
  while. curr =. iter curr do. end.
  i.0 0
}} "0

NB. 1 search_for_primes 60 NB. limit error?

NB.  skip exercises 1.23 1.24 1.25 1.26 1.27 1.28

NB. 1.3 Formulation Abstractractions with Higher-Order Procedures

NB. 1.3.1 Procedures as Arguments

sum_integers =: (          [+   >:@[$:  ])`0: @. >
sum_cubes    =: (      ^&3@[+   >:@[$:  ])`0: @. >
pi_sum       =: ((%@* +&2)@[+  +&4@[$:  ])`0: @. >

NB. a (term sum next) b 
uv_sum =: {{ (u@[+ v@[$: ])`0: @. > "0}}
sum_cubes_2 =: ^&3 uv_sum >: f.

1 sum_cubes_2 10

identity =: ] : [:
sum_integers_2 =: ]uv_sum>:
sum_integers_2/ 1 10

pi_sum_2 =. {{
  pi_term =. %@:* +&2
  pi_next =. +&4
  pi_term uv_sum pi_next/x`y
}}"0

8*pi_sum_2/ 1 1000

integral =. {{ 
  NB. note that adverbs and conjuctions need to define verbs separately because of $:
  i_sum=. u uv_sum (+&n)
  NB. using x and y is advantageous here, because tacitly we would need to assign globally
  n* (n%2+x) i_sum y
}}

^&3 integral 0.01/ 0 1


inside=: (,&.>~  <@:(,&'__');._1 @:(' '&,) )~

'a b c' inside <'yup'

simpson =: {{
  NB. simpson f a b n
  tmp=. cocreate''
  ('fun a b times' inside <'tmp') =: y NB. makelocal 
  
  out=. times__tmp
  coerase tmp
  out
}}
a=:+
simpson a`(1 1;2 2;6)
'a'~
erase 'a b c'
'a__ b__ c__' =.1 2 3





b
b=:4
('b')~

call =: ".@:;@:(":&.>)
call (<1 1)`+`(<1 1)