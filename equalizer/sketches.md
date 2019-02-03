# 3/2/18
## Modularisation of parsing
Idea: read the symbols into a tree structure that reflects the structure of the 
parser, then use an applicativeplus
```
P[I] -> (I -> P[I])*
R[I] -> i × (R[I]*)
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


