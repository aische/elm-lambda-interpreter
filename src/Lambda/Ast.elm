module Lambda.Ast exposing(..)

import Set exposing (Set)

type Term a
  = Var a String 
  | Abs a String (Term a)
  | App a (Term a) (Term a)

getAnn : Term a -> a
getAnn term =
  case term of 
    Var ann x -> 
      ann
    Abs ann x b -> 
      ann
    App ann a b -> 
      ann

mapAnn : (a -> b) -> Term a -> Term b
mapAnn f term =
  case term of 
    Var ann x -> 
      Var (f ann) x
    Abs ann x b -> 
      Abs (f ann) x (mapAnn f b)
    App ann a b -> 
      App (f ann) (mapAnn f a) (mapAnn f b)
      

lambda : String -> Term () -> Term ()
lambda = Abs ()

apply : Term () -> Term () -> Term ()
apply = App ()

var : String -> Term ()
var = Var ()

let_ : String -> Term () -> Term () -> Term ()
let_ x e t = apply (lambda x t) e
