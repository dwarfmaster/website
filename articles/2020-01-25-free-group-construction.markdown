---
title: Construction of the free group in coq
date: January 25, 2020
tags: mathematics, coq
...

$$

\newcommand\G{\mathscr{G}}
\newcommand\hom{\text{Hom}}
\newcommand\M{\mathscr{M}}
\newcommand\bet{\rightarrow}

$$


# Introduction

While following a course on algebra, I add the unavoidable course on the
construction of the free group. The teacher being a pure algebraist, the
construction was fully algebraic. It was a complicated piece of mathematics
where proving some intuitive result turned out very complicated.

And I could think was that the intuition behind the construction is not that
hard, you just have your set set of symbols, you had formal inverses, you take
the free monoid and when there is something of the form $aa^{-1}$ you eliminate
it. And it turns out this is the approach described in
[Wikipedia](https://en.wikipedia.org/wiki/Free_group#Construction).

But there usually is a big difference between an intuitive description and a
formal one, with the proof of the relevant properties, and sometimes the most
intuitive approach is the harder to formalize. I wanted to see if I could
formalize the reduction approach using tools from lambda calculus, and see if it
was more complicated or not. Turns out that this approach was easy enough, so I
present it here.

And it provides me with a perfect exercice to start using Coq again, so I will
provide some Coq snippets of the definitions, and some proofs, along the way.
For simplicity sake, I will assume function extensionality for those proofs. The
final code can be found [here](https://github.com/dwarfmaster/free_group_coq).

# What's a free group anyway

Let's start by defining what's a free group. Intuitively, a free group over a
set $X$ is the most general group in which one can inject $X$, and no relations
between the injected elements.

Can we make it more formal ? Indeed, the definition is that if you have a set
$X$, the free group $\G(X)$ over $X$ is the group such that for every other
group $G$, there is a bijection between $\hom(\G(X), G)$ and $G^X$ (the
functions from $X$ to $G$).

Intuitively, it means that for every group morphism, knowing the image of the
generators of the free group is sufficient. The other direction is the *most
general* part : no matter what images one impose on the generators, it is
possible to create a group morphism that respect them.

For those interested in going further, there is a more general notion of [free
object](https://ncatlab.org/nlab/show/free+object), which itself comes from the
idea of a [free fonctor](https://ncatlab.org/nlab/show/free+functor).

# The free monoid

The basis of our construction will be the free monoid over a set $X$ : $\M(X)$.
It is defined using the exact same definition, replacing free group by free
monoid and group morphism by monoid morphism.

Let's formalize that in Coq. First we need to define a monoid structure :

```Coq

Record Monoid : Type := mkMon {
    type          : Type;
    op            : type -> type -> type;
    empty         : type;
    emptyCorrect  : forall (x : type), op x empty = x /\ op empty x = x;
    associativity : forall (x y z : type), op x (op y z) = op (op x y) z;
}.

Definition is_monoid_morphism (M M' : Monoid) (f : type M -> type M') : Prop :=
    f (empty M) = empty M' /\ forall (x y : type M), f (op M x y) = op M' (f x) (f y).

Record MonoidMorphism (M M' : Monoid) : Type := mkMonMorphism {
    monmor : type M -> type M';
    monmor_correct : is_monoid_morphism M M' monmor;
}.

```

Now we can define the proposition that tells us what a free monoid is :

```Coq

Definition is_free_monoid_over (T : Type) (M : Monoid) : Prop :=
    forall (M' : Monoid),
      exists (F : (T -> type M') -> MonoidMorphism M M'),
      exists (G : (type M -> type M') -> (T -> type M')),
           (forall (f : T -> type M'), G (monmor M M' (F f)) = f)
        /\ (forall (m : MonoidMorphism M M'), monmor M M' (F (G (monmor M M' m))) = monmor M M' m).

```

Now we need to build the free monoid. This is a classic result that I won't
bother proving : the free monoid over a set $X$ is the set of finite sequences
of elements of $X$, and the operation is the concatenation. If you want more
details see on [wikipedia](https://en.wikipedia.org/wiki/Free_monoid) or on the
[nlab](https://ncatlab.org/nlab/show/free+monoid).

You can also find the formalised proof in the coq file, for the following
definition of free monoid over a type :

```Coq

Inductive FreeMonT (T : Type) : Type
    := App : T -> FreeMonT T -> FreeMonT T
    | Empty : FreeMonT T
    .

Fixpoint append {T : Type} (x y : FreeMonT T) : FreeMonT T :=
    match x with
    | Empty _   => y
    | App _ h t => App T h (append t y)
    end.

```

# Building the free group

Let's start a bit more seriously now. Let's review our intuitions : we want need
to add formal inverses, and reduce when something is multiplied by its formal
inverse. Let's note our base set $X$. We define $X^{-1}$ a set in bijection with
$X$, such that $X\cup X^{-1} = \emptyset$. If $a\in X$, we denote its image by
the bijection $a^{-1}$. Those will be our formal inverses.

Now let $M$ be $\M(X\uplus X^{-1})$ the free monoid over our initial set and the
formal inverses. Here is how to implement something like that in coq :

```Coq

Inductive WithInv (T : Type) : Type
    := Reg    : T -> WithInv T
    |  ForInv : T -> WithInv T
    .

```

Now we want a way to say that if, somewhere in the sequence, one find $aa^{-1}$
or $a^{-1}a$, they should be removed. But what does it means to *removes* a
subsequence. It sounds more like we're describing a procedure : start with a
sequence, rewrite every reducible pair until there are no more. This sounds a
bit like the $\beta$-reduction from lambda calculus. Let's roll with this idea
and see where it leads us.

## The reduction

We need to define the reduction first :

$$
\left\{\begin{array}{l}
    \forall x\in X, xx^{-1} \bet \epsilon \\
    \forall x\in X, x^{-1}x \bet \epsilon \\
    \forall \omega_1, \omega_2, \omega_2', \omega_3\in\M,
        \omega_2\bet\omega_2' \implies
            \omega_1\omega_2\omega_3 \bet \omega_1\omega_2'\omega_3 \\
\end{array}\right.
$$

The way we're going to define it in coq is equivalent, but will make it a bit
easier to manipulate : given a word, either the redex is at the start, or we
reduce the tail of the word. It gives the following definition :

```Coq

Inductive Reduction (T : Type) : FreeMonT (WithInv T) -> FreeMonT (WithInv T) -> Prop
    := LeftRed  : forall (x : T), forall (tl : FreeMonT (WithInv T)),
                  Reduction T (App (WithInv T) (ForInv T x) (App (WithInv T) (Reg T x) tl)) tl
    |  RightRed : forall (x : T), forall (tl : FreeMonT (WithInv T)),
                  Reduction T (App (WithInv T) (Reg T x) (App (WithInv T) (ForInv T x) tl)) tl
    |  CtxRed   : forall (x : WithInv T), forall (m m' : FreeMonT (WithInv T)),
                  Reduction T m m' -> Reduction T (App (WithInv T) x m) (App (WithInv T) x m')
    .

```

And now we would like to quotient $\M(X\uplus X^{-1})$ by an equivalence
relationship that says that two terms that have a common reduction are
equivalent. This is actually easy because our reduction relation has two very
interesting properties.

## Strong normalisation

The first thing is that no matter in which order we do the reduction, we will
always reach a normal form, that is to say a form that cannot be reduced
further.

The intuitive argument is easy : everytime we reduce a word, we strictly reduce
its length, so we must stop at some point.

Formally proving that in coq is a bit harder. First we need to define strong
normalisation. Here we use the insight that if we define $x \preceq y$ by $y
\rightarrow x$, we create a new relationship such that $\rightarrow$ is strongly
normalizing if and only if $\preceq$ is
[well-founded](https://en.wikipedia.org/wiki/Well-founded_relation). So we can
just use the well founded module in Coq to get our definition :

```Coq

Definition Inv (T : Type) (R : T -> T -> Prop) : T -> T -> Prop
    := fun (x y : T) => R y x.

Definition strongly_normalizing (T : Type) (R : T -> T -> Prop) : Prop
    := well_founded (Inv T R).

```

The proof is then trying to formalize the previous intuition. That is to say we
first prove that if we have a relation preserving mapping from one relation to
the other, and that the second one is well founded, then the first relation is
well founded. Then we prove that `length` is a relation preserving mapping to
the natural with usual order. The fact that they are well founded is proved in
the Coq standard library, so we use that to conclude.

```Coq

Definition monotome_morphism (T T' : Type) (R : T -> T -> Prop)
        (R' : T' -> T' -> Prop) (f : T -> T') : Prop :=
    forall (x y : T), R x y -> R' (f x) (f y).
 
Theorem preimage_well_founded (T T' : Type) (R : T -> T -> Prop)
        (R' : T' -> T' -> Prop) (f : T -> T') :
    monotome_morphism T T' R R' f -> well_founded R' -> well_founded R.

Lemma monoidLength_monotone (T : Type) :
    monotome_morphism (FreeMonT (WithInv T)) nat
                      (Inv (FreeMonT (WithInv T)) (Reduction T)) lt
                      (monoidLength (WithInv T)).

Theorem reduction_normalizing (T : Type) :
    strongly_normalizing (FreeMonT (WithInv T)) (Reduction T).

```

## Confluence

[Confluence](https://en.wikipedia.org/wiki/Confluence_(abstract_rewriting)) is a
sort of weakened determinism. It's the idea that despite the fact that the
reduction is not deterministic, if two terms come from the same original terms,
we can reduce them further to reach a common reduction.

Our reduction system has an even stronger property, that is *strong confluence*.
It means that if we have three terms $\omega$, $\omega_1$ and $\omega_2$ such
that $\omega\rightarrow\omega_1$ and $\omega\rightarrow\omega_2$, then either
$\omega_1 = \omega_2$ or there exists $\omega'$ such that
$\omega_1\rightarrow\omega'$ and $\omega_2\rightarrow\omega'$.

This property is actually captured by the following definition (the definition
is actually a bit less strong because that's the official definition, but in out
specific case the two cases where $\omega_1\rightarrow\omega_2$ or vice-versa
never happen) :

```Coq

Definition strongly_confluent (T : Type) (R : T -> T -> Prop) : Prop
     := forall (a b c : T), R a b -> R a c
          -> (b = c) \/ (R b c) \/ (R c b) \/ (exists (d : T), R b d /\ R c d).

Theorem reduction_strongly_confluent' (T : Type) :
    forall (a b c : (FreeMonT (WithInv T))), Reduction T a b -> Reduction T a c
        -> (b = c) \/ (exists (d : (FreeMonT (WithInv T))), Reduction T b d /\ Reduction T c d).

Theorem reduction_strongly_confluent (T : Type) :
    strongly_confluent (FreeMonT (WithInv T)) (Reduction T).

```

## Normal forms

Those two properties, strong normalisation and confluence, means that to test if
two terms have a common reduction, we can just reduce them both however we want
until they are in normal form, and check the normal form for equality.

This is all well and good, but for now we've just defined the normal form as
being a term that cannot be reduced further. Surely we can characterize it
better ? And indeed, it is just a word such that there now sub word of the form
$xx^{-1}$ or $x^{-1}x$.

We can just define that naïvely in Coq and it works (we call a word without sub
words of the form $xx^{-1}$ or $x^{-1}x$ *stable*, and show it is equivalent to
being normal for the reduction) :

```Coq

Definition normal_form { T : Type } (R : T -> T -> Prop) (x : T) : Prop
    := forall (y : T), R x y -> False.

Inductive is_stable { T : Type } : FreeMonT (WithInv T) -> Prop
    := StableEmpty : is_stable (Empty (WithInv T))
    |  StableSing : forall (x : WithInv T), is_stable (App (WithInv T) x (Empty (WithInv T)))
    |  StableApp : forall (x y : WithInv T), forall (w : FreeMonT (WithInv T)),
            inv x <> y -> is_stable (App (WithInv T) y w)
                -> is_stable (App (WithInv T) x (App (WithInv T) y w)).

Theorem stable_is_normal_form { T : Type } :
    forall (x : FreeMonT (WithInv T)), is_stable x <-> normal_form (Reduction T) x.

```

## Computable reduction

It turns out than under some reasonable assumption of the base type, our
reduction also has a very interesting property : it is decidable. To show that
we implement a coq function that fully reduce a term and prove it is correct. In
order to do so we need the base type to have a decidable equality, which we
would automatically have if we had assumed the excluded middle.

Here is the definition of the function, and the theorem proving it is correct :

```Coq

Fixpoint liftEq { T : Type } (deceq : T -> T -> bool) (x y : WithInv T) : bool
    := match x, y with
     | Reg    _ a, Reg    _ b => deceq a b
     | ForInv _ a, ForInv _ b => deceq a b
     | _,          _          => false
    end.

Fixpoint reduce { T : Type } (deceq : T -> T -> bool) (x : FreeMonT (WithInv T)) : FreeMonT (WithInv T) :=
    let witheq : WithInv T -> WithInv T -> bool := liftEq deceq in
    match x with
    | (App _ a w) => let reduced : FreeMonT (WithInv T) := reduce deceq w in
            match reduced with
            | (App _ b w') => if (witheq (inv a) b)
                                then w'
                                else (App (WithInv T) a (App (WithInv T) b w'))
            | Empty _      => App (WithInv T) a (Empty (WithInv T))
            end
    | Empty _ => Empty (WithInv T)
    end.

Theorem reduce_is_unique_normal_form { T : Type } (deceq : T -> T -> bool) (decC : DecEqCorrect deceq) :
    forall (x y : FreeMonT (WithInv T)),
        normal_form_of (Reduction T) x y <-> y = reduce deceq x.

```

## Building the quotient

We can now create the candidate group by quotienting by the relation of having a
common normal form. First, we need to check that the concatenation operation is
compatible with this operation, which is obviously the case. The statement in
coq is (where `trefl_closure` is the reflexive transitive closure of the
relation) :

```Coq

Theorem reduction_compatible_append (T : Type) :
    forall (a b c d : FreeMonT (WithInv T)),
        trefl_closure (Reduction T) a c -> trefl_closure (Reduction T) b d
        -> trefl_closure (Reduction T) (append a b) (append c d).

```

We would like to quotient in coq, but turns out that taking quotients in coq is
[hard](https://www.reddit.com/r/Coq/comments/ammiod/quotient_types_with_coq/).
So instead we will cheat : we have a canonical representative of every quotient
elements, the normal form. So we can define the type of elements that are in
normal form, and that will be our quotient. 

```Coq

Record FreeGrpT (T : Type) : Type := mkFreeGrp {
    elem       : FreeMonT (WithInv T);
    elemNormal : is_stable elem;
}.

```

But there is a problem with that approach. It's that there can be more than one
proof of stability for each element, leading to an equality on `FreeGrpT` that
is not just the equality of the element it stores. There are multiple way to fix
that. The first one would be to assume [axiom
K](https://ncatlab.org/nlab/show/axiom+K+%28type+theory%29). I don't really like
this solution since axiom K is incompatible with univalence. Another solution
would be to make sure `is_stable` is a [mere
proposition](https://ncatlab.org/nlab/show/mere+proposition), but my knowledge
of homotopy type theory is not good enough to make use of that yet. So instead I
just added an axiom for that specific case.

```Coq

Axiom eq_freegrp: forall (T : Type), forall (a b : FreeGrpT T), elem T a = elem T b -> a = b.

```

## Proving it is a free group

As we've previously mentioned, the free group can be characterized by a
universal property. This is easy enough to define in Coq. The definitions are a
bit long, so we refer you to the file `FreeGroup.v` in the
[repo](https://github.com/dwarfmaster/free_group_coq) for the complete
definitions and proofs.

The idea of the proof is to define a function from group morphism from the free
group on $X$ to a group $G$ and the functions from $X$ to $G$ by composing the
morphism $\phi$ to the injection $\iota$ of $X$ in $\G(X)$. This is injective
because two morphisms that have the same image have the same image on the image
of $\iota$, and every element of $\G(X)$ is generated from elements of the image
of $\iota$, so the morphisms are fully determined. To show it is surjective, one
can take a function $f : X \rightarrow G$ and extend it on $\G(X)$ the naïve
way: $\tilde{f}(x_1^{\epsilon_1}\dots x_n^{\epsilon_n}) =
f(x_1)^{\epsilon_1}\dots f(x_n)^{\epsilon_n}$. When restricted along $\iota$, we
get $f$ again.


# Conclusion

We have finally managed to construct the free group in Coq. The informal proof
can be described in 2 pages of LaTeX, but the Coq proof took ~900 lines of code.
This is probably because I'm still a beginner regarding Coq, but also because
there where a few intuitive results I used without proving that are actually not
that immediate to formally prove. All things considered it was a fun way to do
some Coq again.

I also had to use an axiom, which is a bit disappointing. I'm planning to read
more of homotopy type theory, and hopefully I'll find a way to circumvent the
need for that axiom.




