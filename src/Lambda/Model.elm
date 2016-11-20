module Lambda.Model exposing (..)

import Dict as Dict
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html 
import Http
import Html.Events exposing (onClick, onInput, onCheck)
import Html.Attributes as A
import Json.Decode exposing (Value)
import Json.Decode as Json
import Json.Encode
import Random
import Set exposing (Set)
import String
import Task
import Time

import Lambda.Ast exposing (..)
import Lambda.Eval exposing (..)
import Lambda.Parser exposing (..)


type EvaluationOrder
  = Normal
  | Normal2
  | Applicative
  | RandomOrder


type alias Model =
  { term : Term2
  , counter : Int
  , seed : Random.Seed
  , alpha : Bool
  , noalpha : Bool
  , order : EvaluationOrder
  }


type Msg
  = Update Int Path Term2
  | Rename Path String
  | DoNextReduction
  | ReduceAt Path


initModel : Term a -> Random.Seed -> Bool -> Bool -> EvaluationOrder -> Model
initModel term seed alpha noalpha order =
  { term = freeBoundAnn term
  , counter = 0
  , seed = seed 
  , alpha = alpha
  , noalpha = noalpha
  , order = order
  }


update : Msg -> Model -> Model
update msg model =
  case  {- Debug.log "msg" -} msg of
    Update counter path value ->
      { model 
      | term = freeBoundAnn <| updateTerm (List.reverse path) value model.term
      , counter = counter
      }

    Rename path value ->
      { model 
      | term = updateName (List.reverse path) value model.term
      }

    DoNextReduction ->
      nextReduction model

    ReduceAt path ->
      reduceAt (List.reverse path) model


getAllReducables : Model -> List Path
getAllReducables model =
  List.map List.reverse <|
  allReducables [] model.term


reduceAt : Path -> Model -> Model
reduceAt path model =
  case lookupTerm path model.term of
    Just (App p1 (Abs p2 name body) arg) ->
      let 
        (counter1, result, _) = 
          if model.noalpha then 
            unsafeBeta model.counter p1 p2 name body arg
          else 
            if model.alpha then
              alphaOrBeta model.counter p1 p2 name body arg
            else 
              alphaAndBeta model.counter p1 p2 name body arg
      in 
        { model 
        | term = freeBoundAnn <| updateTerm path result model.term
        , counter = counter1
        }

    _ -> 
      model

nextReduction model =
  case model.order of 
    RandomOrder ->
      randomReduction model
    Applicative ->
      applicativeReduction model
    Normal ->
      normalReduction model
    Normal2 ->
      normalReduction1 model

normalReduction model =
  case leftmostInnermostReducable [] model.term of
    Nothing ->
      model
    Just path ->
      reduceAt (List.reverse path) model

-- looks also in right branch of App nodes, if no reducable is found in left branch
normalReduction1 model =
  case leftmostInnermostReducable1 [] model.term of
    Nothing ->
      model
    Just path ->
      reduceAt (List.reverse path) model

applicativeReduction model =
  case applicativeReducable [] model.term of
    Nothing ->
      model
    Just path ->
      reduceAt (List.reverse path) model

randomReduction : Model -> Model
randomReduction model = 
  let 
    paths = 
      getAllReducables model 
    len = 
      List.length paths
  in 
    if len > 0 then 
      let 
        (ix, seed1) = Random.step (Random.int 0 (len-1)) model.seed
      in 
      case List.head (List.drop ix paths) of 
        Nothing ->
          { model | seed = seed1 }

        Just path ->
          reduceAt path { model | seed = seed1 }
    else 
      model

cpsTransform model =
  let 
    (counter1, term1) = cps model.counter model.term
  in
    { model 
    | term = freeBoundAnn term1
    , counter = counter1
    }
