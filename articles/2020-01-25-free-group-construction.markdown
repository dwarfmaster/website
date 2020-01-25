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
For simplicity sake, I will assume function extensionality and axiom K for those
proofs. The final code can be found
[here](https://github.com/dwarfmaster/free_group_coq).

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

Inductive WithFormalInverse (T : Type) : Type
    := Reg : T -> WithFormalInverse T
    |  Inv : T -> WithFormalInverse T.

Let FreeMonInv (T : Type) : Monoid := FreeMon (WithFormalInverse T).

```

Now ze want a way to say that if, somewhere in the sequence, one find $aa^{-1}$
or $a^{-1}a$, they should be removed. But what does it means to *removes* a
subsequence. It sounds more like we're describing a procedure : start with a
sequence, rewrite every reductible pair until there are no more. This sounds a
bit like the $\beta$-reduction from lambda calculus. Let's roll with this idea
and see where it leads us.

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





