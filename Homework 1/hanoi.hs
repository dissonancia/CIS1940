module Hanoi where

type Peg  = String
type Move = (Peg, Peg)

hanoi n a b c = hanoiAux n a b c []
  where
    hanoiAux 0 _ _ _ = id
    hanoiAux n a b c = hanoiAux (n-1) a c b . ((a, b) :) . hanoiAux (n-1) c b a
