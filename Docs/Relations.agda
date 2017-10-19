-- Here we focus on the preorder, substitutivity follows from defn.

data _∧_ (A B : Set) : Set where
  conj : A -> B -> A ∧ B

postulate

  Term : Set
  
  -- Improvement is a preorder [M&S]
  _I_ : Term -> Term -> Set
  I-refl : ∀ {x} -> x I x
  I-trans : ∀ {x y z} -> x I y -> y I z -> x I z

  -- Weak improvement is a preorder [M&S]
  _WI_ : Term -> Term → Set
  WI-refl : ∀ {x} -> x WI x
  WI-trans : ∀ {x y z} -> x WI y -> y WI z -> x WI z  
  
  -- Improvement is a subset of weak improvement [M&S]
  I⊆WI : ∀ {x y} -> x I y -> x WI y

-- Define cost-equivalence in terms of improvement
_CE_ : Term -> Term -> Set
x CE y = (x I y) ∧ (y I x)

CE-refl : ∀ {x} -> x CE x
CE-refl = conj I-refl I-refl

CE-trans : ∀ {x y z} -> x CE y -> y CE z -> x CE z
CE-trans (conj xIy yIx) (conj yIz zIy) = conj (I-trans xIy yIz) (I-trans zIy yIx)

CE-sym : ∀ {x y} → x CE y → y CE x
CE-sym (conj x y) = conj y x

-- Define weak cost-equivalence in terms of weak improvement
_WCE_ : Term -> Term -> Set
x WCE y = (x WI y) ∧ (y WI x)

WCE-refl : ∀ {x} -> x WCE x
WCE-refl = conj WI-refl WI-refl

WCE-trans : ∀ {x y z} -> x WCE y -> y WCE z -> x WCE z
WCE-trans (conj xWIy yWIx) (conj yWIz zWIy) = conj (WI-trans xWIy yWIz) (WI-trans zWIy yWIx)

WCE-sym : ∀ {x y} → x WCE y → y WCE x
WCE-sym (conj xWCEy yWCEx) = conj yWCEx xWCEy

-- Cost-equivalence implies cost-equivalence
CE⊆CE : ∀ {x y} -> x CE y -> x CE y
CE⊆CE xCEy = xCEy

-- Cost-equivalence implies weak cost-equivalence
CE⊆WCE : ∀ {x y} -> x CE y -> x WCE y
CE⊆WCE (conj xIy yIx) = conj (I⊆WI xIy) (I⊆WI yIx)

-- Cost-equivalence implies improvement
CE⊆I : ∀ {x y} -> x CE y -> x I y
CE⊆I (conj xIy _) = xIy

-- Cost-equivalence implies weak improvement
CE⊆WI : ∀ {x y} -> x CE y -> x WI y
CE⊆WI (conj xIy _) =  I⊆WI xIy

-- Weak cost-equivalence implies weak cost-equivalence
WCE⊆WCE : ∀ {x y} -> x WCE y -> x WCE y
WCE⊆WCE xWCEy = xWCEy

-- Weak cost-equivalence implies weak improvement
WCE⊆WI : ∀ {x y} -> x WCE y -> x WI y
WCE⊆WI (conj xWIy _) = xWIy

-- Improves implies improves
I⊆I : ∀ {x y} -> x I y -> x I y
I⊆I xIy = xIy

-- Improves implies weak improves
-- { postulated }

-- Weak improves implies weak improves
WI⊆WI : ∀ {x y} -> x WI y -> x WI y
WI⊆WI xWIy = xWIy
