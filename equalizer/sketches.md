
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
Then the recrusive types are supposed to solve a lot of problem generically,
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


