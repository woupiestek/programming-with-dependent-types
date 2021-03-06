
# 1/6/20

It has been a long break.

## Rehearsal

Dependent types are most naturally modelled using a locally
cartesian closed category. Sadly, type checking could be quite 
complex, if it relies on comparing members of nested arrows 
types. Hence this idea of approximating the more difficult types 
with partial equivalences over types of a simpler subcategory.

A locally cartesian closed category has finite limits and
polynomial functors. The latter are the functors:
`P(f)(A) = sum{y: cod f} {x: dom f | f(x) = y} -> A`
for each morphism `f`. The idea now is that because every 
morphism is an algorithm that can also be applied to input 
that doesn't satisfy the equations, every morphism
`{x: dom f | f(x) = y} -> A` is a restriction of a morphism
`dom f -> A` (I am starting to see a problem here, but let's 
continue). The `dom f` can be an injective resolution of the 
fibres of `f` and hence `cod f * (dom f -> A)` can cover the
more complicated type above.

Here is the flaw: if `A = {z:Z| E}` is a restriction, then 
`dom f -> A` would exclude morphisms that leave `A` in cases 
where `f(x) = y` doesn't hold.

So, there may be a benefit to only having weak polynomials,
because that allow comp-lex type to be usbsets of simpler types,
but we still would have to deal with complex propositions.

Just consider equations level 0, and implications `a -> b` to be
of level `max(level(a)+1, level(b)}`
To properly cover polynomials, at any level, the following is 
needed:
`{ <a,b> | forall x: dom f. f(x) = a -> E(b(x)) }`
On one hand, predicates have to be at least level 1 to take 
`f(x) = a` into consideration. But if `dom f` is defined by a 
level 1 predicate, we are forced up another level, and then 
another and so on, aren't we?

The point of covering `P(f)(A)` with a subset `Q(f)(A)`
of `|cod f| * |dom f| -> |A|` is to simplify comparisons of 
functions. So while `a,b:Q(f)(A)` depends on satisfying 
constraints of `A` relative to `dom f`, `a = b` should ignore 
these constraints.

## retrace steps

I want to let go of extensional equivalence to simplify type
checking, but were do we see the advantages? So the idea is that
when comparing `a` and `b` of `Q(f)(A)`, restrictions on the
domains of `a` and `b` no longer count.
This helps with limits: the equalizer of `g, h: B -> Q(f)(A)`
only contains those elements `b` where `g b x = h b x` for all
`x : |dom f|`, no just those in a domain. This is therefore
no source of level escalation anymore. It help with polynomials 
as well: if `f: dom f -> Q(g)(B)`, then 
`forall x: dom f.f(x) = a -> E(b(x))` doesn't necessarily exceed 
level 1, since the equation `f(x) = a` doesn't bring along 
further restrictions.

Maybe this way of letting go of extensional equivalence just
isn't that useful.

We need the equalizers, can we have those while weaking the
equivalence of morphisms? The greatest danger is losing the 
uniqueness of the equalizers themselves, but when would that 
happen? Does this actually help?

## Just play the game?

Ultimately there seems no way out of searching proofs for
at least a large subset of first order propositions.

Why are we doing this again?

I think I am just carefully considering what dependent type 
checking actually involves, while trying to keep it real...

When higher level formulas show up, as in the definition of
basically any arrow and especially any polynominal type, 
that seems cause for including evidence of equality and 
membership. So where do we go off track?

The clearest case is perhaps `P(f)(A)`. Instead of ignoring
`[a,b,c]: P(f)(A)` consist of `a: cod f`, `b: |dom f| -> |A|`
and `c: forall x: dom f. f(x) = a -> b(a): A`...
Have I solved any problem here?

The point is that the equality type of lambda terms in non
trivial. The type `{M = N}` must explain how to prove the 
equality.

What I am really saying is not that there are path types, but
that equalizers tend to be weak here. Maybe that can be excused 
by the nature of the types.

It is so singular. Non trivial equality types just show up.

## Hierarchy

Simple types at the bottom. These might as well be 
0-dimensional if inhabited. Then there are equalizers of simple 
types. Functions can be applied and the results can be 
evaluated. At this point there is 1-dimension already, which 
starts to explain the math. Then the space of functions between
equalizers is 3-dimensional already. There aren't just paths,
but deformations of paths to deal with, though the latter should
be of simple type.

These dimension relate to the number of points in the simplicial
set, but are always off by one. I don't really know what to
expect from the dimensions of functions spaces based on this.
The space of morphisms from an m-simplex to and n-simplex has 
what dimension? `mn+m+n`? Okay, that is somwhat plausible.

Something is off. Products of equalizer are equalizers, but
products of 1-dimensional spaces have higher dimensions. If 
equalizers are considered 0-dimensional however, then we don't
get off the ground.

Not a good metaphore then.

I suppose we need to say that evidence for `{M = N}` is used in
the prove that `{P = Q}`, with an certain context. Usually this
means `M=\r.sT`, and that occurences of `s` in `{P = Q}` can be 
eliminated with pattern matching. Yeah, if `s` occurs with 
at least as many arguments, then by putting constraints on 
the variables `r` the whole subexpression can be replaced by 
`Q`. Weaken using assumption `exists r. T = U`, and replace `sU`
with `sT = Nr`. That is what the proof terms could look like:
A combination of weakening and using substitution with 
equivalents.

Yeah. Rules like `{M = N} * phi[M] -> phi[N]` could be the 
morphisms that rule equivalences of lambda terms. And this
starts to look like homotopy type theory.

## Too much

Comonadic io `Promise`, `Ref`/`Var`. 

```
$.tailRec: (a -> m(a || b)) -> a -> (m b)
$.await: ((a -> (m 1)) -> (m 1)) -> (m a)
$.async: (((a -> (m 1)) -> (m 1)) -> (m b)) -> a -> (m b)
$.catch: (e -> (io $ e' a)) -> (io $' e a') -> (io ($ && $') e' (a || a'))
$.raise: e -> (io 1 e 0)
$.rebase: (io r 0 r') -> (io (r && r') e a) -> (io r e a)
$.ref<a>: (a -> (io 1 0 1)) && (io 1 0 a)
$.js: string -> (io 1 0 top)
```

Maybe go back to something more useful. 

# 15/3/20

Let's go over the rules again.

Basically I am now assuming that types can be fully implicit,
and are constructed for type varaibles using only function 
spaces `a -> b` and array spaces `a*`. Those arrays are a way 
to get recursive types. This is just a start, because I think
more types are needed. The idea now, is that more general types 
are partial equivalences over these simple type.

Yeah. The type checker should look use pattern matching to find
a counter model for a certain system of equations. The 
strategies seem rather obvious, though.

It strickes me that messing with fixpoint types can help: the 
least fixpoint is the open one that allows existential
quantification, while the greatest fixpoint is the compact one
that allows universal quantification. What I am talking about
is an assymetric strategy, where either the falsifier or the
verifier wins by default.

## All the reductions

There are layers, where the ultimate dependent type theory is
about equivalence relations in an extensional type theory,
which in turn is about predicates in a simple type theory.
`ITT > ETT > STT`
The typechecker is concerned with the extensional type theory,
so it worries about how equalities between simple functions 
imply each other.

Here is the hard part: the type `{x:A|P(x)} -> {y:B|Q(y)}`
doesn't exist on the extentional level. Instead,
`{f:A -> B| forall (x:A). Q(f(x))}` serves to cover (for) it.
Or is it more like `{f:A -> B| forall (x:A). P(x) -> Q(f(x))}`?
I think we are going to need the more powerful version.

I guess what I am mostly worried about is introducing some base
types like `String`, and then finding it impossible to deal with
equalities of `String`-values functions. What will happen in 
practice, however, is that such expressions won't always be 
considered equal if there a extensionally, at least not on the 
existential level of the type checker. Instead there'd be a type
of `String`-valued-expressions, with few expressions considered
equal. It is up to the intensional layer to fix this.

## Induction

Induction is needed to prove properties about arrays. Okay,
maybe we won't notice that, because, in a way, we are just
putting labels on lambda terms the type checker had to give
special treatment.

# 14/3/20

One further reduction of recursive types: take the second
projection `{<=} -> N`. Now `A*` is the fibred exponential,
which is covered by `N * ({<=} -> A)` which turns `A* -> B` 
into a subtype of `N -> ({<=} -> A) -> B`.

# 10/3/20

The type `A* -> B` is basically all we need. It is the fixpoint
of `W -> B × (A -> W)`, which covers the greatest fixpoint of
the polynomial functor `P_f` for every `A -> B`. Since there
already are functions spaces, the free monoid functor is 
sufficient.

Functions and arrays are all we need to get us started.

# 9/3/20

For any parser functor:

- Monoid-valued is a way to get nondeterminism, but this may not
  be expressive enough.
- Applicative works given choices based on characters, monad
  can do with a simple read next function. 

# 7/3/20

Reasoning from the tree comes eerily close to the applicative
functor ideas I had before. It is a different way to write down
similar sets of rules. Bottom up parsers shows that reasoning 
from the tree can go in other directions.

# 4/3/20

The very basic version just have a list of constructors...
but terminals have te be added. So, characters, strings with
holes. The guard against left recursion is that if the 
template starts with a hole, only constructors donwstream
will be considered to fill that hole.

The second level groups constructors by type, and works with
typed holes. This limits the choice of constructors considered
at each stages of the search. 

# 2/3/20

## Thinking from the tree

The best opportunities for parsing in parallel are searching
through the space of all abstract syntax trees. Considering what
the nodes would look like, I get something like this: 

```json
{
  "product": {
    "times": "${sum} (\\* ${sum})*"
  },
  "sum": {
    "plus": "${type2} (\\+ ${type2})*"
  },
  "number": {
    "digits": "[0-9]+",    
  }
}
```

Names for the nodes in the grammar. Some support for repeaters
so the nodes can have arrays of children instead of single
elements, though this doesn't actually add any power. The types
should be optional. On the other hand, perhaps the repeaters
should apply to the types, with an eye on producting trees 
efficiently.

```json
{
  "parenthetical": {
    "times": "\\(${list}\\)"
  },
  "list": {
    "empty": "",
    "one": "${elt}",
    "more": "${elt},${list}" 
  },
}
```
Of course, from here on the type can be even more complex.

The core of the idea is that the space of ASTs is a fibred 
W-type for a finite communative triangle. I guess special 
support would only be needed for the non final variants,
in particular to have infinite lists of rules.

That rules are sequences of characters and holes, and that holes
and rules can be connected by types to simplify searching the 
trees. 

Another twist would be to take advantage of operations on the
character set: mainly having types that represent ranges of
characters.

For the parallel search, the crucial thing is the ordering of
the rules: each thread just consider the higher priority paths
that have not yet been ruled out.

# 21/12/19
Simplified oo/js: everything is an object and in case of doubt
the empty object is returned. Well, methods are in their own 
category and need conversion. 

## compiling to jvm with asm
The hard part is generating the methods.

As an exercise, just do a functional version of
standard methods. i.e. `if` and `while` expressions,
perhaps `try` expressions as well, whatever that means.
Some other control structures.

- `if B F G` ~ pick one of two options
- `while B F X ~ if (B X) (while B F (F X)) X`
- `try F G X` ~ think of 'attempt'


# 19/12/19
The `the` solution may make sense when dealing with data,
but for the functions operating on the data it makes much less
sense to be referred to by their type. Especially if lots of 
functions operate on a limited number of data types.

Its that it doesn't make much sense to introduce aliases
for `Double -> Double` for each function in a mathematics
library, even if those aliases are free.

Perhaps it is not about types but about comple time information.
So static functions might as well

`type Square = Double -> Double`

Back to inference, somewhat:
`%Square = %Double * %Double <- Double`
That is assuming this is statement is meant to define a type 
and a value at the same time.

Maybe I should give it a shot: just naming types, never values.

# 18/12/19
Go the other way with a language where variables are eliminated 
in favor of types. It could work with a `the` operator, that
refers to the value of a type in the context. That should always
work as long as there is one element of each type.

`[ % A | M =% A; N | M N | A -> M ]` 

# 16/12/19
What enables type inference is that types are patterens that
can be matched. With the product-projection example, I run into
a situation where no neat pattern seem to be formed, but it
doesn't have to be that way.

I am thinking of row polymorphism now. A 'pattern' like
`{ a: t }` can match a pattern like `{ b: u }`, because of 
implicit row variables: `{ a: t } ~= { a: t, R }` and
`{ b: u } ~= { b: u, S }`. 

## Further analysis
Injectives are closed under products and transfinite composition
in one direction. My concern is that such types cannot be
reconstructed for expressions that have no explicit type 
annotations. Now I have several solutions:

- Come up with a language that has type annotations.
  I thought I was working on a kind of compile time script where
  type annotations would be explicit assertions, however.
- Find a solution with intersection types
  This solution simply collects a number of constraints, and
  only finds a type mistmatch if these constraints cannot be
  unified.
- Find a solution with row types. A refined version of the above
  What does that buy us?
- Baked in recursive product types? This one seems by far the 
  simplest...

The idea was that the dependent types and programs form a
category with enough injectives. The subcategory of injectives 
doesn't need to be closed under all the structure of the
dependently typed supercategory. The products and transfinite
compositions should be there, and to support local cartesian
closure, the category of injective should at least be 
cartesian closed. What more do we ask? Mostly just structure to
help to get the other types back from just the injectives.
Weak colimits could be part of that mix, just like weak limits
are for projectives.

## backing in recursives
Mimic Javascript and have a type `any` which is a sum of 
other types, like `string`, `integer`, `object = string -> any`,
`array = integer -> any` and `function = any -> any`.
Maybe it should be a product instead of a sum...
Actually `any ~ type -> any` wouldn't work, would it?

We can introduce a universal type `any` and let the others be
retracts of this type. I cannot tell what happens to the
functions whose types we derived more carefully however.

## the type ocean
Let there be a sum of all types `any`. There is an inclusion
`t -> any` from any concrete type `t` that adds a type tag to
each label. As huge sum type, the elimination rule is quite
epic, since it has to offer solutions for each type tag it 
finds. What could help here, is pattern matching on types, to 
cover large numbers of options easily. Another helpful aspect is
that every type is injective, so there is a way to extend
partial functions to the whole of every type.

It makes sense to define functions from the any type using
switches that match types:

- `[x0: p0 = M] N0 || [x1: p1 = M] N1 || ...`
- `M = x0: p0 -> N0 || x1: p1 -> N1 ||  ...`
- `{ x0: p0 -> N0, x1: p1 -> N1, ... }`

If we go here, then maybe it makes sense to add the entire
universe `typeOf: any -> type`.

## acknowlegde product differences
Records can be eliminated through destructuring. This
can provide a precise match between keys, and make products safe
to work with. This is just a matter of treating products as
adjoints of function types.

Destructering can be done in function arguments as well as
on the left had side of equations, to safe space.

## back up
My wishlist contained recursive types because they are a
challenge for the solvers, that needs to be solved for most of
the practical applications I already had in mind. Introducing
such types without actually having a language for types is
diffucult, however. The option I am considering here is 
creating a sum of all types, which seems a bit awkward...

The `any` type doesn't actually have to be absolutely any of 
course. It could be limited to a subset of all types and so on,
though that would make the parallel tothe universe of small
type more powerful. Hmm...

The purpose of `any` is that objects can be defined as functions
`string -> any` and arrays as `int -> any`. That takes care of
two kind of products, and possibly recursive products as well.
Of course, if we go for the `any` is a sum of types solution,
then in order to access a member, we need to specify its type...
it would be missing from the definition.

The destructuring solution now seems more elegant now, though it
technically has the wrong type of product.

Shifting to a more type based approach means completely
different expression to solve equations between.

This is not the nice little challenge I can fill an idle 
afternoon with.

# 15/12/19
The alternative of just using record types doesn't really work.
The problem is not getting rid of keys that aren't accepted,
but finding types for keys that aren't used. I am afraid that
there is no other solution than providing this data explicitly.

## Intersections
What could we do with intersection and type inequalities?

It looks like we can bascially give every expression a type,
without ruling anything out, unless some intersections are
empty in some circumstances. That could work though: everything
has a type, so the real work is in the intersections.

# 14/12/19

I am wondering about adding products and coinductives to
the calculus.

The destructors generally give to little information to tell
the type reconstructor what to do. It gives a type inequality,
instead of the equation we normally use.

I can think of a couple of options: stick with generic arrays
and records etc. and get the type safty back through the
equations. Or, add the required information to either methods
or inputs.

## Targets
I wanted a language that looks dynamic, but is dependently types
mostely behind the screen. Since more advance type systems
require more information, I hoped to supply this data with
mere equations. This is connected to the idea of needing finite
limits to do the coolest things.

Other ideas came in, like using this as an intermediate target
for many purposes: compile to this language, typecheck, then
compile to somthing else. 

Another idea: unusual sytax based on category theory. Could be
fun to have a language that is point free.

# 6/10/19

Big map solution: use position to identify each bit of source
code and reify the heap to get an efficient parser/typechecker.
Different design from the books, but it makes sense.

Now the map part is just an interface, to encapsulate other
data structures.

Okay how does this work?
The packrat parser already has this structure: for each position
a lazy mapping from non-terminals to matched ASTs.
the position help telling variables with the same name apart,
which in turn helpt with type checking and constraint checking.

## Basic datastructure

```scala
sealed trait Key {
  val line: Int
  val column: Int
  type ValueType 
}

//datastructure has type
Key => Key#ValueType
```

Perhaps split this into one structure for each type, to make
things easier.

# 2/9/19

Packrat is revitalizing, although I haven't quite got it right.

# 25/8/19

Not being effective.

A stream could be: `Foldable[IO[Foldable[_]]]`

# 3/8/19

Recursive type equations suggest that expressions of a recursive type have
infinitely many different types at once, rather that having an option of
being folded or unfolded explicitly. This ambiguity is undesirable.

## types explored again
Function types are strictly necessary. Distinct cartesian products are really
nice to have, and possibly not optional in a language that isn't generic. 
Then the recursive types are supposed to solve a lot of problem generically,
rather than having to come up with separate solutions every time.
```
Gamma, x: A |- M: B
-------------------
Gamma |- x -> M: A -> B

Gamma |- M: A -> B   Gamma |- N: A
----------------------------------  (application should bind more tightly I think)
Gamma |- M N: B

(x -> M)N  ==  x = N; M

Gamma |- M: A   Gamma |- N: B
-----------------------------
Gamma |- <M, N> : <A, B>

Gamma |- M: <A, B>
----------------- (here elimination binds more tightly too)
Gamma |- M_0: A

<M, N>_0  ==  M



Gamma, x: A |- M: F(A)
----------------------
Gamma |- x @ M: t @ F(t)

Gamma |- M: t @ F(t)
-------------------- (it is a pattern)
Gamma |- M': F(t @ F(t))

(x @ M)'  ==  x = (x @ M); M
```
No starting point for the fix point operator, 
since the starting point is bottom every time.

# 31/7/19

## the language
types: [T, *] | T -> T | {V:T,}
functions: \ (V:Y,)*.T, T T, V, V=T,T, [(T,)*], [(V,)*]=T;T, {(V:T,)*}, 
  {(V:V,)*}=T;T
proposition: &V:T.P, P && P, P => P, T == T
keyword: 'type', 'def', 'assert', 'fix'

identifiers
operators
parentheses, brackets and braces
colon, comma, is, backslash

~ Proposition is like a build in type
~ connected to build in functions... which don't need theis own syntax...
forall: (T -> Omega) -> Omega
&&, ->: Omega -> Omega -> Omega
==: T -> T -> Omega

# 27/7/19

## JSON TDOP

The full power of a parser comes from a `MonadPlus` structure on a generic parser
type `P`, together with a single `read:P[IA]` where `IA` is the input alphabet.
This read stand for the effect of reading a single symbol form input and moving
forward. Main concern about this parsing algebra is that it is hard to implement
efficiently. After all, the parser for the next symbol can depend on the
abstract syntax tree (AST) that has been generated up to that point--i.e. too 
much feedback is allowed.

A step weaker is `ApplicativePlus` with `readIf: (IA => Boolean) => P[IA]`. 
Basically, the next parser depends on the current input symbol. Choice of parser
is now limited to a finite list. This is still powerful enough to describe 
parsers for many context-free languages, however.

The next step break the parser into a parser and lexer, each of which give up
on a different power. The lexer breaks up de input and labels the fragments
as token, which the parser then combines into ASTs.

The parser can now be `Applicative` with `read[AST]: (IA => P[AST]) => P[AST]`,
using a top-down-operator-precendence-like strategy. I.e. if the current token 
is an operator, the parser for the next token produces a suitable function.

It makes sene for the lexer to avoid the `Applicative` part and to focus on the
`PlusEmpty` part... and that kind of works. Added `Functor` to analyse the
matched characters, `pop:P[List[IA]]` to return chucks of symbols and stop 
reading and both `read[AST]: (IA => P[AST]) => P[AST]` and `readIfEqual`.

Lexing strings and numbers of JSON was really hard. How can this be improved?

- Break these token up into smaller parts. The problem here is that lexers won't
  know that certain token can only appear in certain contexts, which becomes a 
  problem with strings, because segments of string can look like anything.
- Double pass: go through the input symbols a second time after lexing, to 
  unescape the escape sequences, and to break up numbers into signs, integer, 
  fractional and exponential parts.
- Input manipulation: especially useful to deal with escape codes inside strings. 

Actually, different strategies make sense for different problems. Numbers can
be broken up. For strings it makes more sense to do one of the others, because
strings can contain all of the other tokens, and cause confusion that way.

## Experimenting
Allowing invalid token and soritng them out later seems to work well.
With number we run into trouble: there is no 'end-of-number' operator,
that says when to stop reading more token. Fascinating--it appears an expression
cannot simply end, it must be forcefully broken off.

What we haven't considered yet is operator precedence. i.e. the higher binding 
power of `.` are supposed to safe us here. Strangely this still looks like 
a choice made in the parser, that differs from simply accept or reject. Maybe
JSON wasn't a good test case.

The lexer can be brought down to basically `PlusEmpty`, `Functor`,
`readIf: (AI => Boolean) => P[AI]` and `pop: P[List[AI]]` provided that no token
is wasted. The parser has to take care of errors.

# 26/7/19

Maybe we could do something with method declarations instead: `x.m = ...`
This only makes sense if methods are functions, though. Why not just allow 
recursive functions?

Just keep it simple. `x`, `x:A = M; N`, `M N`, `\x:A.M`, `M.m`, `{m M}`, `M[0]`,
`[M]`


# 25/7/19

The types are arrows and recursive products. Perhaps these can be nested in
mutually recursive type declarations.

The terms are more complicated. One idea is that injectives have default 
members, so the members of recursive products can be build up from nothing
by attaching members step by step. Added to this is a notation for fix points.

Something like this:

```ts
type F = {true X false X} -> X
  where X = F -> F 

function f {x A y B z C} Z = x.m y w where w = z.k x

function w {x A y B z C} Z = {field0 M field1 N field2 P} where a = x.m y w

assert forall x M = N -> P = Q
```

Missing here is anything about modules, compilation or execution.

# 23/7/19

## Top down operator precedence parser
I think I got it down to this
The parser is applicative-error plus *guards*:
`Read(f: I => P[I,O]): P[I,O]`
I keep wanting to change this, but we cannot forget that the point is to
limit the way in which parsers can depend on input symbols.

I have some proble here concerning whitespace etc., because I cannot seem to
define it in these terms. The trouble there is that rather than failing,
or providing a parser, we need to keep the current symbol unread.
We could break it up:
`Read(f: I => P[I,O]): P[I,O]`
`Advance: P[I, Unit]`
The first one simply looks at the current symbol without advancing,
the second actually moves the parser to the next symbol. 
I guess I am missing something, perhaps somthing special about whitespace,
like `Unit` being a monad. I.e. `ReadWhile(f: I => Boolean, ???)`
Something like a `*` operator for these parsers.

# 21/7/19

Replace separate predicates with functions `a0 -> ... -> an -> Proposition`.
Perhaps that causes problems as we can now introduce functions that take
propositions as arguments. The promise of extensional equivalence is avoided.
Another interesting issue is--should `Proposition` just be `Type`? I think that
in the case of modest sets and assemblies, `Proposition` is the power set of the
natural numbers and `Type` is the sets of partial equivalence relations on the
natural numbers, so there is an inclusion.

I think it is a moot point: I have target runtimes in mind, and `Type` is 
supposed to represent a set of types that means something in that runtime.
Some runtimes will give nice interpretations to propositions, others won't.
The proposition have their own purpose, however. Alongside supercombinator
declarations the core language allows assertions of propositions, for two
purposes: before compilation the assertions are checked to guarantee certain
results and when imported as modules, the asserted statements are treated as
assumptions.

Concern: there are a number of *native functions* to construct expressions
construction. I don't think we can treat these like other symbols. E.g. 
`typeOf (lambda ...)` should be equivalent to something like
`product (typeOf ...)` and this recursive pattern matching and rewriting 
should be done while checking asserted propositions. The problem is that
the tactic for making equivalences computable is ignored here. Couple of 
solutions:
- allow special cases. Just treat some functions different from others.
- make the resolver powerful enough to take these equations into account, 
  i.e. the extra equivalences are propositions, and we generally allow
  these proposition to affect how equations are solved.
- have some algebraic types at the top level. Perhaps all of `Proposition`,
  `Type` and `Value` are, despite the speical role of `Proposition`.
  Fair and scary a the same time.

## Algebraics?
The requirement of injectiveness makes this awkward, like we need to add a 
*bottom* to every type, to make this work. Let's just go with it. Also, we are
straying from the initial idea of how to compare functions. Algebraic types
allow systematic search, giving a different way to assess whether a proposition
is true.

This is a set back. `typeOf` is a recursive function. A logic processor that
avoids recursive types cannot reason about it properly.

There is an advantage, which is that less has to come from nothing.

## weak W-types
Perhaps *W-types* can reduce our pain. For any `f: A -> B` approximate `W(f)` 
with a simpler `W = B * (A -> W)`. We would combine this with finite types, 
probably, especially for `B`. Is that possible?
`cons: (A -> W(A, B)) -> B -> W(A, B)`, `dom: W(A,B) -> B`, 
`fun: W(A,B) -> A -> W(A,B)`. Then, hopefully:
`W(f) = { w: W(A, B) | \a: A. dom (fun w a) = f a }`
This could work for proper W-types. Are our types of this sort?

Consider:

- `product: (Value -> Type) -> Type -> Type`  
- `lambda: (Value -> Value) -> Type -> Value`

I assume that for `Type`, `product: B` and I guess `A = Value + 1`, which
does pose a challenge. `Value`, however, is not a proper W-type!

## review
Many constraints are formulated in terms of `typeOf` which means that 
`typeOf` cannot be an opaque function symbol. The system needs a way to 
rewrite it.

So, imagine that `Value` is actually a more complicated type `X -> Y` and
e.g. `typeOf v = v .getType`. This shifts the problem to value constructors,
but that may be okay? `lambda f d .getType = product (\y(f y .getType)) d`.
This leads to more class // interface like structures, which have reduction 
rules. 

```c#
interface Value {
  Type dom
  (Value -> Value) fun
  Type type = product dom \(v: Value)((fun v).type)
}
```

No matter how well this works, we firstly add in complicated recursive types
that can firstly, can screw up the type checker and secondly, in order to have 
different kinds of Value, we need to support values with different members, e.g.
a dependent product would look like this:

```c#
interface Value {
  (Type type = coproduct fun
  (Value -> Type) fun 
  Value first
  Value second) |
  second.type = fun(first)
}
```

This is pretty much a complete dynamic language. Are there upsides?
I suppose the nominal system does help: it gives a place to leave 
the constraints.

## Recursive products
I think I need mutually recursive products. Type declaration may not be
necessary, but they might be useful. Constructors do need special care: 
recursive elements refer to themselves. Hence there should be fixpoint
constructors that allow such constructions. These should exist as top level
functions because we want to lambda lift everything. This means there is
a constructor declaration, that takes a function with an extra argument, and
produces a product.
When comparing... *records* the checker compares the members and ultimately
constructors, deliberately failing to look further than whether the same own
members get called to avoid endless loop in the checkers.

At least a limited form of recursive product of injective is injective, 
thanks to closure under transfinite composition (actually its opposite) for
injective maps. Injectives lift the map `0 -> 1` and therefore have default
elements. Hence all type in this language have default elements and only
contraints can protect against them.

## Other side effecting declarations
So beside function and type declarations, and assert commands for the checker to 
check, there could be run commands, or compile commands for the rest of the 
interpreter.

# 20/7/19

Expose classes & methods, with signatures, but what does that mean?
Classes get replaced with predicates--but do these predicates have signatures?
```Y = {(x0:X0,...,xn:Xn)| ... }```
In that case the predicates needs equations and membership, like a topos.

What is hard here is that we define predicates for later use.
```Y = {(x0,...,xn)| ... }```

Look, the real structure is layered.

- There is a basis of *injective* types and functions that have arrow types.
  The large types are limits of these. Quantifying over all large types must be
  impossible, so it isn't possible to equate functions that have literally the
  same code, but are between different types. That kind of quantification does
  not work here.
- There are arrow types for the injectives, and here there should be a strict
  equality relation. The result is a left exact category with weak exponential,
  assuming that the space of functions `{(x0: a0, ..., xn: an)|E} -> b` can be
  *covered* by the type `a0 -> ... -> an -> b`. This is the core assumption, 
  that could turn out to be fatal, but it is related to the notion.
  Powersets, sets with defualt elements etc. are examples of injectives. Could
  be an intuition for what the basic types are.
- Do not forget equality types for: `{(x0: a0, ..., xn: an)|E} -> b`. 
  I think this works: `{(a*,g,h) | E -> g(a*)=f(a*) }`. This betrays that the
  injectives are not just regular injectives however.
- So that is basically what a file should look like:
  Declarations of functions between limits of injectives,
  and predicates (actually sets of equations) defined on products of injectives.
  no lambda's aren needed at this stage, as long as partial application is
  allowed. functions between predciates, that is actually the case.  
- Building on all this, there is a *universe of small types* which is a function
  `typeOf: Value -> Type`. First order dependent products are supposed to show
  up here, as functions `product: (Value -> Type) -> Type -> Type`, 
  `lambda: (Value -> Value) -> Type -> Value` for example. This shows the first
  examples of injectives as well.
- Note that to approximate some languages, multiple universes may be needed. 
  Note that properly evalutating terms requires special resolution rules,
  which may be complicated for any build in monad.
  Note that this core language forces the type of a function to be specified
  form the start, so type inference requires equation solving.

### Collect

- `product: (Value -> Type) -> Type -> Type`  
- `lambda: (Value -> Value) -> Type -> Value`
- `coproduct: (Value -> Type) -> Type -> Type`
- `pair: (Value -> Type) -> Value -> Value -> Value` 
  (with constraint `x0 x1 = typeOf x2`)


# 19/7/19

## more simplifying assumptions

Keep track of positions to tell variables apart. 
Only top level functions, types and all.

# 13/7/19

Thoroughly reworked parsing. Wonder how these new versions will work.

# 10/7/19

Dealing with right recursion: `X = A | X ~ B` to `X' = A ~ (X' | B)`.
Otherwise the breadth is infinite.

# 9/7/19

Its just breadth first search.
Every parser reduces to a list of `Point(_)` and `FlatMap(Read,_)`
The first one represent the matches, the second one the differentials.

# 7/7/19

## Stack safety
How likely is it that a file with code conatians such deepley nested expressions
that it overflows the stack to recur through them? I woudl have to admit that
change is small. The embedded interpreter for lambda terms is already tail 
recursive and should not be a problem.

I get really distracted by a performance detail that probably is not going to
trip me up. Let's try to avoid that from now on.

## Plan

- What do the results look like? I imagine either a conformation that the
  term checks out, or a list of failures that should be able to indicate where
  errors occured.
- Before we evaluate terms, we should first do a type check and comparison
  between types and variables. Here too we deal with equations nested in 
  clauses.
- If the simple type check succeeds, then the terms should have a normal form.
  There is not limit on the size of this normal form, but the problem is kept of
  the stack, and made smaller by pausing reduction at each weak head normal
  form.
- So now the procedure becomes the same: free variables in forms are pattern
  matched until each is assigned a value, or eliminated. The result is a 
  set of assignments of both type and value to locally free variables--a model
  for the term equations.
- The next step is to compare models generated by different parts of the
  sentence. Keeping thing local is probably a good idea, because that means
  that local conflicts will be noticed first.
- Now it becomes important who own what variable and in what context. If the
  model constrains variables that belong to the skeptic, he could argue
  that there are counterexamples that break there constraints, which can be
  countered in the antecedents. Values assigned to the believer, on the other
  hand, are welcome gifts.

Still complex this.

# 1/7/19

## weak exponentials
Suppose:
- `A = { x: a | alpha(x) }`
- `B = { x: b | beta(x) }`
- `C = { x: c | gamma(x) }`
- `f: A -> B`
Then:
` f => C = { (x: b, y: a -> c) | beta(x), forall z:a. f(z) = x, alpha(z) -> gamma(y(z)) } `
While this is already a weakening, because a clause shows up, the 
predicates can contains nested clauses. Not completely evaluating lambda 
expressions becomes more valuable. 

On one hand, it would be nice to reduce all the way down to nomral forms and 
compare those. On the other, this is only valid if reductions are equal, which 
is avoided for a simpler type checker. Lambda expression equivalence needs
working out.

## limited to free variables
Terms define behavior outside their proper domains and can be distiguished on 
that account. In other words, although there are restrictions on what values a
variable can take encoded in the type, the equivalence on lambda expressions
doesn't care about these equivalences.

This locks together now:
- Extensional equivalence means that two lambda expressions give the same 
outputs for the same inputs, given inputs *in the domain*. Extensionally 
equivalent terms may therefore differ on inputs outside of the domain. The
type checker will ignored this, however, and consider counter examples
to equivalene outside the domains to be proper evidence of inequality. I.e. 
```(\x \y x == y -> x) != (\x \y x == y -> y)``` because any pair of elements
counts as evidence against equality.

Path types are supposed to give back extensionality in a way. It is sort of 
clear what that would look like now. The type `{x:a|M==N} -> B` has a path type, 
where a path `P:B -> Q:B` is a term `R` such that `RM = P` and `RN = Q`. 
Larger sets of equations simply lead to higher arity `R` and nested clauses 
are functions of paths.

# 30/6/19

## Weak exponentials
The basic idea is that the language is the internal language of a locally 
Cartesian closed category. That requires limits and fibred exponentials. This
is a problem, because comparing functions is not necessarily decidable, and
apparently quite complex if possible. However, weakened versions of these 
structures are porbabaly good enough. In other words, rather than having
fibred exponentials, have weak fibred exponentials.

If `f: A -> B` is a function, then de fibred exponential type `f => C` consist 
of pairs `(x:B,y:A_b -> C)`, or rather there should be functions 
`dom: (f => C) -> B` and `apply: { (x: f => C, y:A) | dom(x) == f(y) } -> C` 
such that for each `g: D -> B` and `h: { (d:D,a:A) | g(d) == f(a) } -> C` there 
is a unique `h':D -> f => C` such that `dom ¤ h' = g` and `apply ¤ h' = h`.
The weakening is precisely that the uniqueness above is dropped. There may be
multiple `h'` such that the composition is `h`.

Obviously, `f => C` is not a proper expression, because it contains a function.
Instead the types would have a lambda expression for `f`. Part of the weakening 
could be that `f => C` is actually `B * (A => C)` for a lambda expresion type
`A => C`. I think it should be harder: assuming `C = { d:D | F(d) }` take
`{ (b:B,c:A => C)| forall a. f(a) = b -> F(c(a)) }`.
Assumed here is that every member `f => C` canonically extends to a 
function `A => C`, defined by its lambda expression, since the lambda expression
expresses operations that don't care about the constraint on the inputs.
However, `C` can be a solution set, and express constraints that only hold
for certain values of `A`.
This looks like it kickstarts the escalating complexity of first order logic
with lambda expressions, but maybe not?
1. formulas on the right hand side of the arrow are limited to equations.
2. equality of lambda expressions has to be consistent, but it doesn't have to
be extensial equality, i.e. a lot of equalities are allowed to be false.

The weaker equality of lambda expressions means that Church encodings for other 
data types won't work well. Other structure, like colimits and recursive
types may therefore need some help.

# 3/6/19

I am rewriting formulas to equivalents, which chould be easier to evaluate. This
 involves making a lot of copies. That is why I think it would be nice to keep 
track of formulas modulo equivalence. Equivalence classes of syntax, that is
what I want.

## new idea
normalize to a kind of disjunction normal form, 
until 


# 2/6/19

Finally working on the type checker as I should.

To design a runtime for it:
- generating fresh variables
- call-by-need semantics
  that requires a heap... but we could try to use 'the' heap
- deep recursion

## learnings
- Skolemize, to reduce the problem to dependent equation solving.
- Conjunction-of-clauses may not be a good normal form for my purposes.
- Suspending terms with `() => eval(...)` leads to uncontrolled recursion, and 
  no efficient option for adding variables to the stack
- Normalized terms and 'tasks' should both by valid arguments.
- A strategy is needed to deal with unbound variables. They are the reason I 
  hang onto the names. Perhaps we can split into three kinds: 
    existentially bound,
    universally bound,
    unbound
  trouble with unbound is that existentials aren't depending on them.
- We keep needing fresh variables deep into the process, and we also have to
  keep track of the list of universally bound variables the exitstential ones
  have to be applied to.
- I use a neat trick for putting recursive structure together after breaking
  them down, but it won't handle equivalent subsentences correctly.

## variables
Every variable has a unique position in the source. So, we can use positions
to prevent variables capture: book keep where the binding happened.

## another idea
Learn Haskell and do this in Haskell. Simply use the state monad
Or use the state monad of Scalaz en hope it works out...

# 4/2/18

Simple idea: where the derivative recognizer uses booleans, the parser uses 
'sets' of values.

## folding out

I am stuck on the last bit: getting the values out at the end. That is not part
of the applicative plus specification.

One option is a foldable structure, were all the matching structures are
generated one by one. Trying to construct one shows some assumptions
fatally wrong. Just consider the following rules:

```scala
1. empty.foldMap(f) == empty
2. (a <+> b).foldMap(f) == a.foldMap(f) append b.foldMap(g)
3. a.point.foldMap(f) == f(a)
4. (a <*> b).foldMap(f) == a.foldMap(c => b.foldMap(d => f(d(c))))
5. (a <*> empty) == empty
6. (a <*> (b <+> c)) == (a <*> b) <+> (a <*> c)
```

Rule 4. is the idea I needed and missed. Rules 2. and 6. show what is difficult.
Getting the types right will be hell.

# 3/2/18

## Modularisation of parsing

Idea: read the symbols into a tree structure that reflects the structure of the 
parser, then use an applicativeplus

```scala
P[I] => (I => P[I])*
R[I] => i × (R[I]*)
```

## Extremism

I created a derivative parser according to the instructions at:
http://matt.might.net/articles/parsing-with-derivatives/
The structure merely recognizes whether a string matches, and
does not build up any structure based on what it matches. I wanted
to bolt this structure on, but things started to cancel out.

What we need is a fast ApplicativePlus/Alternative/Monoid-of-functors,
lazy, memoized and recursive.

Once we have that, the only thing missing is a function 
`(I => F[O]) => F[O]` which we use to feed data from a stream into the parser.
Now I'd swear there is an even better way:
Stuff the 'I => Boolean' in the same datastructure
For each i:I, map the whole structure (lazyly) to get the next step...
one problem: were do all the structure building steps go?

Something like this might work:
F[(I => Boolean)\/ B]
We now have to be careful about what the map does, however,
to keep it functioning as anything like a derivative.
Perhaps there are some thing we cannot cleanly separate.

# 16/12/18

Now what?

The terms kan be normalized to the form:
`\ab.cd`
where `a` is a list of variables,
`b` is a list of equation of terms,
`c` is a variable and
`d` is a list of terms that serve are arguments.
The domain of this term is determined by `ab`, and this has
become a more or less independent form `\a.cd`.

I don't see how any of there terms could fail at this point. Every condition
just transforms into new stricter conditions on application. The only thing
that makes sense now, is to somehow simplify the domains.

The fact that the domains are independent has some curious consequences.
`\ab.cd = \ef.gh` requires that `b |= f`, `f |= b` and roughly `b, f |= cd = gh`.
Equations become collection of clauses, that contain equations of either
simpler terms, or of simpler form. Here we run into the headache of keeping
track of variables.

Wait a minute... nope!
For I while I was thinking that the `b|=f`, `f|=b` part doesn't add anything
important, as these can be assumed. But it is an essential part of the equality.

The `b, f |= cd = gh` part is the hardest. The lead variables make it possible
to match up equations, then use a from of pattern mathcing.

- `a, (b |= cd = ef) |= cg = hi`
- `a |= b`, `a, d = g |= ef = hi`

I don't think this is the only rule to take into account. It feels like there
should be some method to eliminate variables and finally, some direct 
comparison of terms should play a role too. So I don't think this is complete.
Along the way, I have a problem dealing with all the variables, although I have
a kind of solution for it.

The obvious rule `ab = cd <=> a=c, b=d` can only be used under very special
circumstances, but don't a need to add something for that?

# 13/12/18

M = N <->
dom(M) = dom(N) & forall x:dom(M) => term(M) = term (N)

# 12/9/18

Just get rid of the variables

The situation makes it harder to deal with variables:
$nN*: B forall A* :- A[n] = product(C*,B), (N*:C* forall A*)

Forces the introduction of substitution:
(\:AM)NP*: B :- N:A, M[N]: B
(\:AM): product(A,B) :- M: B forall A

Maybe the trick is to match fully typed normal forms to arbitrary terms,
at least at this stage.

# 10/9/18

We want something prolog-like, with trees with variables etc. but
at the same time, we want to keep trakc of  lot of different types--otherwise,
why not just use prolog?

# 3/9/18

It just occurred to me tht if you just have declared rules in a
database, you can simply search a rule that fits best. No need
to have functions from the start.

M: A, forall(x: A)(NP*: B) <=> (x: A = M; N)P*:(x: A = M; B)
M: A, forall(x: B)(xN*: C) <=> forall(y: prod(x: A)B)yMN*: C

alpha clause (beta :- beta*)
beta clause  (forall (var,type)* gamma)
gamma clause typing, equation ...

# 2/9/18

       forall(x: A)(M: B) <=> fun(x: A)M: prod(x: A)B
 M: A, forall(x: B)(N: Z) <=> forall(y: prod(x: A)B)(N[x = yM]: Z)
        M: A, N[x = M]: B <=> x: A = M; N: B
      M = N, M = P, N = Q <=> refl M N: path P Q
     forall(x: A)(M = Nx) <=> fun(x: A)M = N
M: A, forall(x: B)(N = P) <=> forall(y: prod(x: A)B)(N[x = yM] = P)
       M: A, N[x = M] = P <=> (x: A = M; N) = P
             M = P, N = Q <=> refl M N = refl P Q

--- substitution ---
         x[x = M] :=> M
         y[x = M] :=> y
      (NP)[x = M] :=> (N[x = M])(P[x = M])
  (\x:A.N)[x = M] :=> \x:(A[x = M]).N
  (\y:A.N)[x = M] :=> \y:(A[x = M]).(N[x = M])
(refl N P)[x = M] :=> refl (N[x = M]) (P[x = M])
(path N P)[x = M] :=> path (N[x = M]) (P[x = M])

How do we apply this in practice?

This is such a struggle

## deeper analysis

There must be many layers to the problem

The rule that I found assumes that we can reduce statements to
a specific form, a conjunctions of clauses, where the clauses
equate certain normal forms of lambda terms. At that point,
proving the causes may require forking and backtracking and
introducing equation of an unrefined form.

typeOf(fun(x: A)M) = prod(x: A)typeOf(M)

This is beyond my abilities now. I need to break it down further.

# 1/9/18

A more complete term language:
Term = Var | \Var: Type. Term | Term Term | Var: Type = Term; Term | refl Term Term
Type = Var | forall Var: Type. Type | path Term Term

De Bruijn indexed alternative:
Term = Nat | fun Type Term | Term Term | Type = Term; Term | refl Term Term
Type = Nat | prod Type Type | path Term Term

But now we cannot do the reorderings.
       forall(A)(M: B), C = prod(A)B <=> fun(A)M: C
 M: A, forall(B)(N: Z), C = prod(A)B <=> forall(C)(N[0M]: Z)
                   forall(A)(M = N0) <=> fun(A)M = N
M: A, forall(B)(N = P), C = prod(A)B <=> forall(C)(N[0M] = P)

# 26/8/18

        forall(x: A)(M: B), C = prod(x: A)B <=> \xM: C
  M: A, forall(x: B)(N: Z), C = prod(x: A)B <=> forall(y: C)(N[x = yM]: Z)
            M = N -> P: A, C = e(M, N) -> A <=> M = N! P: C
M = N, forall(x: A)(P: Z), C = e(M, N) -> A <=> forall(y: C)(P[x = (M = N? y)]: Z)

                        forall(x: A)(M = Nx) <=> \xM = N
  M: A, forall(x: B)(N = P), C = prod(z :A)B <=> forall(y: C)(N[x = yM] = P)
                     M = N -> P = (M = N? Q) <=> (M = N! P) = Q
M = N, forall(x: A)(P = Q), C = e(M, N) -> A <=> forall(y: C)(P[x = (M = N? y)] = Q)

               M = N, P = Q -> R: A <=> (M = N? (P = Q! R)): A
              M = N, P = Q -> R = S <=> (M = N? (P = Q! R)) = S
           M: B, forall(x: B)(N: A) <=> (\xM)N: (\xA)N
          M: B, forall(x: B)(N = P) <=> (\xM)N = (\xM)P

We need some help:
--- substitution ---
         x[x = M] :=> M
         y[x = M] :=> y
      (NP)[x = M] :=> (N[x = M])(P[x = M])
     (\xN)[x = M] :=> \xN
     (\yN)[x = M] :=> \y(N[x = M])
(N = P! Q)[x = M] :=> (N[x = M]) = (P[x = M])! (Q[x = M])
(N = P? Q)[x = M] :=> (N[x = M]) = (P[x = M])? (Q[x = M])

How to do this? So much to account for.

# 20/8/18

Sketches may help to outline these algorithms, that I don't seem to be able to just write out in Scala...

I extended the lambda calculus as follows:
T = V | TT | \VT | V=T;T | T=T!T | T=T?T
The first three are standard. The V=T;T is an explicit substitution, which could just be syntactic sugar for
(\VT)T. The extra T=T! and T=T! generate terms of dependent types

I an unsure if the set of rules I have are complete, especially when it comes to deriving equalities from each other.
To deal with the complex task of searching a huge and complex space, I had the idea of framing everything as rewriting
certain propositions, because proposition can represent every possible state of the computation. This leaves me with
too much options and no clear direction to go in.

We could base it on anything.

I am looking into basing it on sequents now, using what is normally a proof search algorithm to search for types.
The rules seem to work outside in for abstraction and unification, but to work upwards from occurances of universally
quantified variables for application and assertions. I think this works for equations as well...

This looks sorta good...

--- useful equations ---
        forall(x: A)(M: B) <=> \xM: prod(x: A)B
  M: A, forall(x: B)(N: Z) <=> forall(y: prod(z :A)B)(N[x = yM]: Z)
             M = N -> P: A <=> M = N! P: e(M, N) -> A
 M = N, forall(x: A)(P: Z) <=> forall(y: e(M, N) -> A)(P[x = (M = N? y)]: Z)
      forall(x: A)(M = Nx) <=> \xM = N
 M: A, forall(x: B)(N = P) <=> forall(y: prod(z :A)B)(N[x = yM] = P)
   M = N -> P = (M = N? Q) <=> (M = N! P) = Q
M = N, forall(x: A)(P = Q) <=> forall(y: e(M, N) -> A)(P[x = (M = N? y)] = Q)

      M = N, P = Q -> R: A <=> (M = N? (P = Q! R)): A
     M = N, P = Q -> R = S <=> (M = N? (P = Q! R)) = S
  M: B, forall(x: B)(N: A) <=> (\xM)N: (\xA)N
 M: B, forall(x: B)(N = P) <=> (\xM)N = (\xM)P

We need some help:
--- substitution ---
         x[x = M] = M
         y[x = M] = y
      (NP)[x = M] = (N[x = M])(P[x = M])
     (\xN)[x = M] = \xN
     (\yN)[x = M] = \y(N[x = M])
(N = P! Q)[x = M] = (N[x = M]) = (P[x = M])! (Q[x = M])
(N = P? Q)[x = M] = (N[x = M]) = (P[x = M])? (Q[x = M])


