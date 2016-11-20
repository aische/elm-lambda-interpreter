module Main exposing(..)

import Combine as Parser exposing (Parser)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick, onCheck, onInput)
import Html.Attributes as A
import Http
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
import Lambda.Parser 
import Lambda.ExpressionView
import Lambda.TreeView
import Lambda.Model as Lambda

-- MAIN

main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : (Model, Cmd Msg)
init = update (SetExample "t1") model

-- MODEL

type alias Model = 
  { term : Lambda.Model
  , input : String
  , auto : Bool
  , deltaTime : Int
  , viewFlags : Set String
  , factor : Int
  }

model : Model 
model = 
  { term = 
      Lambda.initModel t1 (Random.initialSeed 21366) False False Lambda.Normal2
  , input = ""
  , auto = False
  , deltaTime = 10
  , viewFlags = Set.fromList ["expression", "tree"]
  , factor = 10
  }

myconst = Abs () "x" (Abs () "y" (Var () "x"))
myid = Abs () "x" (Var () "x")
mycompose = Abs () "f" (Abs () "g" (Abs () "x" (App () (Var () "f") (App () (Var () "g") (Var () "x")))))
myid2 = App () (App () mycompose myid) myid

t1 = App () (App () (App () myconst myid2) (Var () "oha")) (Var () "ende")
t2 = App () (App () (Abs () "id" (Abs () "x" (App () (Var () "id") (Var () "x")))) myid) (Var () "ui")
t3 = App () (App () myconst (Var () "y")) (Var () "noo")
t4 = App () (Abs () "y" (App () (App () myconst (App () (Var () "y") (Var () "y"))) (Var () "noo"))) (Abs () "y" (App () (App () myconst (App () (Var () "y") (Var () "y"))) (Var () "noo")))
t5 = App () (App () (App () myconst myid) myid) (App () (App () myconst t4) t3)
t6 = App () (Abs () "_y_1" (App () (App () (App () (Abs () "y" (App () (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))) (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))))) (Var () "noo")) (App () (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))) (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))))) (Abs () "_y_3" (App () (Abs () "y" (App () (App () (Abs () "x" (Abs () "_y_4" (App () (Var () "y") (Var () "y")))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))) (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))))))) (Abs () "_y_2" (App () (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo"))) (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (App () (Var () "y") (Var () "y"))) (Var () "noo")))))
t7 = 
  let_ "id" (lambda "x" (var "x")) <|
  let_ "const" (lambda "x" (var "x")) <|
  (apply (var "const") (var "id"))
t8 = App () (Abs () "y" (App () (App () (Abs () "x" (Abs () "y" (Var () "x"))) (Var () "y")) (Var () "noo"))) (Var () "yes")

examples = Dict.fromList
  [ ("t1", t1)
  , ("t2", t2)
  , ("t3", t3)
  , ("t4", t4) 
  , ("t5", t5) 
  , ("t6", t6) 
  , ("t7", t7) 
  , ("t8", t8) 
  ]

-- UPDATE

type Msg 
  = Inc 
  | Dec 
  | SetDeltaTime Int
  | TermMsg Lambda.Msg
  | SetViewFlag String Bool
  | SetAuto Bool
  | SetAlpha Bool
  | SetNoAlpha Bool
  | CopyTermToInput
  | SetOrder Lambda.EvaluationOrder
  | SetExample String
  | SetInput String
  | LoadExample
  | Cps

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Inc ->
      ( { model | factor = model.factor + 1 }
      , Cmd.none
      )
    Dec ->
      ( { model | factor = max 2 (model.factor - 1) }
      , Cmd.none
      )
    SetDeltaTime i ->
      ( { model | deltaTime = max 1 i }
      , Cmd.none
      )
    TermMsg msg1 ->
      let term1 = Lambda.update msg1 model.term 
      in 
      ( { model 
        | term = term1
        }
      , Cmd.none
      )
    
    SetAlpha e ->
      let s = model.term
          s1 = { s | alpha = e}
      in
      ( { model
        | term = s1
        }
      , Cmd.none 
      )
    
    SetNoAlpha e ->
      let s = model.term
          s1 = { s | noalpha = e}
      in
      ( { model
        | term = s1
        }
      , Cmd.none 
      )
    
    SetOrder e ->
      let s = model.term
          s1 = { s | order = e}
      in
      ( { model
        | term = s1
        }
      , Cmd.none 
      )
    
    SetViewFlag name b ->
      ( { model
        | viewFlags = (if b then Set.insert name else Set.remove name) model.viewFlags
        }
      , Cmd.none 
      )
    SetExample s ->
      let t = Maybe.withDefault t1 <| Dict.get s examples
      in
      ( { model
        | term = 
            Lambda.initModel t model.term.seed model.term.alpha model.term.noalpha model.term.order
        , input = Lambda.Parser.showTerm t
        }
      , Cmd.none 
      )
    SetAuto auto ->
      ( { model
        | auto = auto
        }
      , Cmd.none 
      )
    SetInput s ->
      case Parser.parse Lambda.Parser.pTerm s of 
        Err r ->
          ( { model
            | input = s
            }
          , Cmd.none 
          )
        Ok (_,_,t) -> 
          let 
            term = 
              model.term

            term1 = 
              Lambda.initModel t model.term.seed model.term.alpha model.term.noalpha model.term.order

            term2 =
              { term1 | counter = nextCounter t }

          in
          ( { model
            | input = s
            , term = term2
            }
          , Cmd.none 
          )
    LoadExample ->
      ( model
      , Http.send (\r -> Result.withDefault (Inc) <| Result.map SetInput r) (Http.getString "example.txt")
      )

    Cps ->
      ( { model 
        | term = (Lambda.cpsTransform model.term)
        }
      , Cmd.none
      )

    CopyTermToInput ->
      update (SetInput <| Lambda.Parser.showTerm model.term.term) model
  

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.auto of
    False ->
      Sub.none
    True ->
      Time.every (Time.second * (0.1 * toFloat model.deltaTime)) (\_ -> TermMsg (Lambda.DoNextReduction))


-- VIEW

switchButton : String -> Model -> Html Msg
switchButton key model =
  let
    value = Set.member key model.viewFlags
    activeStyle = [("background-color", "#aaaaff")]
    inactiveStyle = []
  in 
  button 
    [ onClick (SetViewFlag key (not value))
    , A.style (if value then activeStyle else inactiveStyle) 
    ]
    [ text key
    ]

view : Model -> Html Msg
view model =
  div 
  [ 
  ] 
  [ div []
    ( [ button [onClick (TermMsg (Lambda.DoNextReduction))] [text "step"] 
      , if model.auto then 
          button 
            [ onClick (SetAuto False)
            , A.style 
              [ ("background-color", "red")
              ]
            ]
            [ text "pause"
            ]
        else  
          button 
            [ onClick (SetAuto True)
            , A.style 
              [ ("background-color", "#88ff88")
              ]
            ]
            [ text "play"
            ]
      , button [onClick (SetInput model.input)] [text "reset"] 
      , button [onClick (SetOrder Lambda.Normal2), A.style (if model.term.order == Lambda.Normal2 then [("background-color", "yellow")] else []) ] [text "normal"] 
      , button [onClick (SetOrder Lambda.Applicative), A.style (if model.term.order == Lambda.Applicative then [("background-color", "yellow")] else []) ] [text "Applicative"] 
      , button [onClick (SetOrder Lambda.RandomOrder), A.style (if model.term.order == Lambda.RandomOrder then [("background-color", "yellow")] else []) ] [text "random"] 
            
      , button 
          [ onClick (SetAlpha <| not model.term.alpha)
          , A.style (if model.term.alpha then [("background-color", "yellow")] else []) 
          ] [text "alpha"] 
      , button 
          [ onClick (SetNoAlpha <| not model.term.noalpha)
          , A.style (if model.term.noalpha then [("background-color", "red")] else []) 
          ] [text "ignore alpha conversion"] 
      , button [onClick (Cps)] [text "cps"] 
      , button [onClick (SetDeltaTime (model.deltaTime - 1))] [text "faster"] 
      , button [onClick (SetDeltaTime (model.deltaTime + 1))] [text "slower"] 
      , text (toString (toFloat model.deltaTime * 0.1))
      , button [onClick CopyTermToInput] [text "copy to input"] 
      , button [onClick Dec] [text "-"]
      , text " zoom "
      , button [onClick Inc] [text "+"]
      ]
    )
  , div []
    ( [ button [onClick (LoadExample)] [text "example"] 
      ] 
      
      ++
      List.map (\k -> button [onClick (SetExample k)] [text k]) (Dict.keys examples)
    )
  , div []
    (
      List.map (\k -> switchButton k model) ["expression", "tree"]
    )
  
  , div 
      [ A.style 
          [ ("width", "100%")
          ]
      ] 
      [ Html.textarea 
        [ A.value <| model.input
        , onInput SetInput
        , A.style 
          [ ("box-sizing", "border-box")
          , ("width", "100%")
          , ("max-width", "100%")
          ]
        ] 
        [] 
      ]
  , if Set.member "expression" model.viewFlags then 
      div 
        [ A.style 
          [ ("border", "1px solid black")
          , ("padding", "10px")
          , ("overflow", "wrap")
          , ("min-height", "50px")
          , ("margin-bottom", "2px")
          ]
        ]
        [ Html.map TermMsg <| Lambda.ExpressionView.renderTerm model.term
        ]
    else 
      text ""
  , if Set.member "tree" model.viewFlags then 
      div 
        []
        [ div 
            [ A.style 
              [ ("border", "1px solid black")
              , ("margin-bottom", "2px")
              ]
            ]
            [ Html.map TermMsg <| Lambda.TreeView.renderTerm model.term (toFloat model.factor / 20)
            ]
        ]
    else 
      text ""
  , Html.br [] []
  , Html.br [] []
  , Html.br [] []
  , Html.br [] []
  ]
