module Lambda.ExpressionView exposing (..)

import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Html.Attributes as A
import Set exposing (Set)

import Lambda.Ast exposing (..)
import Lambda.Eval exposing (..)
import Lambda.Parser exposing (..)
import Lambda.Model exposing (..)


renderTerm : Model -> Html Msg
renderTerm model = 
  div 
  []
  ( renderTermP model model.counter [] model.term
  )

renderTermP : Model -> Int -> Path -> Term2 -> List (Html Msg)
renderTermP model counter path term =
  case term of
    Var p name -> 
      [ Html.span [] [text name]
      ]

    Abs p name t -> 
      [ text "\\"
      , text name
      , text "."
      ]
      ++
      renderTermP model counter (0::path) t
    
    App p1 t1 t2 -> 
      let 
        reduceButton = 
          case t1 of 
            Abs p2 name body ->
              let 
                p_arg = getAnn t2 
                collisions = Set.intersect p2.bound p_arg.free
                isBeta = not model.alpha || (Set.isEmpty <| collisions)
              in 
              [ Html.span 
                [ onClick (ReduceAt path)
                , A.style 
                  [ ("color", if isBeta then "blue" else "red")
                  ]
                ]
                [text "@"]
              ]
            _ 
              -> [text "@"]
      in
        reduceButton
        ++
        renderTermP model counter (0::path) t1
        ++
        [text " "]
        ++
        renderTermP model counter (1::path) t2
        


