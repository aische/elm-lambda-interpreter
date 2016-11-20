module Lambda.Eval exposing(..)

import Set exposing (Set)
import String 

import Lambda.Ast exposing (..)


type alias Path = List Int

type alias TermInfo = 
  { free : Set String
  , bound : Set String
  }

type alias Term2 = Term TermInfo


replace : String -> Term a -> Term a -> Term a
replace name value term =
  case term of 
    Var p name1 ->
      if name == name1 then value 
      else term
    Abs p name1 body ->
      if name == name1 then term 
      else Abs p name1 (replace name value body)
    App p a b ->
      App p (replace name value a) (replace name value b)


freeVars : Term a -> Set String
freeVars term =
  case term of 
    Var _ name ->
      Set.singleton name 
    Abs _ name body ->
      Set.remove name (freeVars body)
    App _ a b ->
      Set.union (freeVars a) (freeVars b)


boundVars : Term a -> Set String
boundVars term =
  case term of 
    Var _ name ->
      Set.empty
    Abs _ name body ->
      Set.insert name (boundVars body)
    App _ a b ->
      Set.union (boundVars a) (boundVars b)


renameBoundVar : Int -> String -> Term a -> (Int, Term a)
renameBoundVar counter name term =
  case term of
    Var p name ->
      (counter, Var p name)
    Abs p name1 body ->
      if name == name1 then 
        let 
          (c1, new) = newName counter name1
          body1 = replace name1 (Var p new) body
          (c2, body2) = renameBoundVar c1 name body1
        in 
          (c2, Abs p new body2)
        --Abs p new (renameBoundVar name new' (replace name (Var p new) body))
      else 
        let 
          (c1, body1) = renameBoundVar counter name body
        in 
          (c1, Abs p name1 body1)
    App p a b ->
      let 
        (c1, a1) = renameBoundVar counter name a 
        (c2, b1) = renameBoundVar c1 name b
      in 
        (c2, App p a1 b1)


renameBoundVars : Int -> List String -> Term a -> (Int, Term a)
renameBoundVars counter vars term =
  case vars of 
    [] -> 
      (counter, term)
    v::vs ->
      let 
        (counter1, term1) = renameBoundVar counter v term 
      in 
        renameBoundVars counter1 vs term1
        

newName : Int -> String -> (Int, String)
newName counter name =
  (counter + 1, ("_" ++ toString counter))


updateTerm : Path -> Term a -> Term a -> Term a
updateTerm path value term =
  case (path, term) of
    ([], _) ->
      value
    
    (0 :: path1, App p t1 t2) ->
      App p (updateTerm path1 value t1) t2
    
    (_ :: path1, App p t1 t2) ->
      App p t1 (updateTerm path1 value t2)

    (_ :: path1, Abs p x body) ->
      Abs p x (updateTerm path1 value body)

    _ ->
      term


lookupTerm : Path -> Term a -> Maybe (Term a)
lookupTerm path term =
  case (path, term) of
    ([], _) ->
      Just term
    
    (0 :: path1, App _ t1 t2) ->
      lookupTerm path1 t1
    
    (_ :: path1, App _ t1 t2) ->
      lookupTerm path1 t2

    (_ :: path1, Abs _ x body) ->
      lookupTerm path1 body

    _ ->
      Nothing


updateName : Path -> String -> Term a -> Term a
updateName path value term =
  case (path, term) of
    ([], Var p _) ->
      Var p value
    
    ([], Abs p x body) ->
      Abs p value body

    (0 :: path1, App p t1 t2) ->
      App p (updateName path1 value t1) t2
    
    (_ :: path1, App p t1 t2) ->
      App p t1 (updateName path1 value t2)

    (_ :: path1, Abs p x body) ->
      Abs p x (updateName path1 value body)

    _ ->
      term


leftmostInnermostReducable : Path -> Term a -> Maybe Path
leftmostInnermostReducable path term = 
  case term of
    Var _ name -> 
      Nothing

    Abs _ name t -> 
      Nothing

    App _ t1 t2 -> 
      case t1 of 
        Abs _ name body ->
          Just path
        _ ->
          leftmostInnermostReducable (0::path) t1


leftmostInnermostReducable1 : Path -> Term a -> Maybe Path
leftmostInnermostReducable1 path term = 
  case term of
    Var _ name -> 
      Nothing

    Abs _ name t -> 
      Nothing

    App _ t1 t2 -> 
      case t1 of 
        Abs _ name body ->
          Just path
        _ ->
          case leftmostInnermostReducable1 (0::path) t1 of
            Just path ->
              Just path

            Nothing ->
              leftmostInnermostReducable1 (1::path) t2


applicativeReducable : Path -> Term a -> Maybe Path
applicativeReducable path term = 
  case term of
    Var _ name -> 
      Nothing

    Abs _ name t -> 
      Nothing

    App _ t1 t2 -> 
      case applicativeReducable (1::path) t2 of 
        Just path ->
          Just path

        Nothing ->
          case t1 of 
            Abs _ name body ->
              Just path
            _ ->
              applicativeReducable (0::path) t1


allReducables : Path -> Term a -> List Path
allReducables path term =
  case term of
    Var _ name -> 
      []

    Abs _ name t -> 
      allReducables (0::path) t

    App _ t1 t2 -> 
      allReducables (0::path) t1
      ++  
      allReducables (1::path) t2
      ++
      case t1 of 
        Abs _ name body ->
          [path]
        _ ->
          []


freeAnn : Term a -> Term (Set String)
freeAnn term =
  case term of 
    Var _ x -> Var (Set.singleton x) x
    Abs _ x b -> 
      let 
        b1 = freeAnn b 
        fv = getAnn b1
      in Abs (Set.remove x fv) x b1 
    App _ a b -> 
      let 
        a1  = freeAnn a
        b1  = freeAnn b
        fv1 = getAnn a1
        fv2 = getAnn b1
      in App (Set.union fv1 fv2) a1 b1


freeBoundAnn : Term a -> Term2
freeBoundAnn term =
  case term of 
    Var _ x -> 
      Var 
        { free = Set.singleton x
        , bound = Set.empty
        } 
        x
    Abs _ x b -> 
      let 
        b1 = freeBoundAnn b 
        ann = getAnn b1
      in 
        Abs { free = Set.remove x ann.free, bound = Set.insert x ann.bound } x b1 

    App _ a b -> 
      let 
        a1  = freeBoundAnn a
        b1  = freeBoundAnn b
        ann1 = getAnn a1
        ann2 = getAnn b1
      in 
        App { free = Set.union ann1.free ann2.free, bound = Set.union ann1.bound ann2.bound } a1 b1


hasCollisions : TermInfo -> TermInfo -> Bool
hasCollisions p1 p2 =
  let 
    fv = p1.free 
    bv = p2.bound 
  in 
    not <| Set.isEmpty <| Set.intersect fv bv


alphaOrBeta : Int -> TermInfo -> TermInfo -> String -> Term2 -> Term2 -> (Int, Term2, Bool)
alphaOrBeta counter p1 p2 name body arg =
  let 
    func = Abs p2 name body
    p_arg = getAnn arg
    fv = p_arg.free --freeVars arg
    bv = p2.bound -- boundVars func
    collisions = Set.toList <| Set.intersect fv bv
  in 
    if List.isEmpty collisions then 
      (counter, replace name arg body, True)
    else 
      let 
        (counter1, func1) =
          renameBoundVars counter collisions func
      in 
        (counter1, App p1 func1 arg, False)


alphaAndBeta : Int -> TermInfo -> TermInfo -> String -> Term2 -> Term2 -> (Int, Term2, Bool)
alphaAndBeta counter p1 p2 name body arg =
  let 
    func = Abs p2 name body
    p_arg = getAnn arg
    fv = p_arg.free --freeVars arg
    bv = p2.bound -- boundVars func
    collisions = Set.toList <| Set.intersect fv bv
  in 
    if List.isEmpty collisions then 
      (counter, replace name arg body, True)
    else 
      let 
        (counter1, func1) =
          renameBoundVars counter collisions func
      in
        case func1 of 
          Abs p2 name body ->
            (counter1, replace name arg body, True)
          _ ->
            Debug.crash "impossible! Abs is not Abs anymore after renaming?!?!"


unsafeBeta : Int -> TermInfo -> TermInfo -> String -> Term2 -> Term2 -> (Int, Term2, Bool)
unsafeBeta counter p1 p2 name body arg =
  (counter, replace name arg body, True)


-- call by name
cps : Int -> Term a -> (Int, Term ())
cps counter term =
  case term of
    Var _ name -> 
      let 
        (c0, k) = newName counter ""
      in 
        (c0, Abs () k (App () (Var () k) (Var () name)))
    
    Abs _ name body ->
      let 
        (c0, k1) = newName counter ""
        (c1, k2) = newName c0 ""
        (c2, body1) = cps c1 body 
      in 
        (c2, Abs () k1 (App () (Var () k1) (Abs () k2 (Abs () name (App () body1 (Var () k2))))))

    App _ a b ->
      let 
        (c0, k) = newName counter ""
        (c1, t0) = newName c0 ""
        (c2, t1) = newName c1 ""
        (c3, a1) = cps c2 a
        (c4, b1) = cps c3 b
      in 
      (c4, Abs () k (App () a1 (Abs () t0 (App () b1 (Abs () t1 (App () (App () (Var () t0) (Var () k)) (Var () t1)))))))


nextCounter : Term a -> Int
nextCounter term =
  let 
    bv = boundVars term
    f name = 
      if String.startsWith "_" name then
        Maybe.withDefault (-1) <| Result.toMaybe <| String.toInt <| String.dropLeft 1 name
      else
        -1
  in 
    1 + (Maybe.withDefault (-1) <| List.maximum <| List.map f <| Set.toList bv)
