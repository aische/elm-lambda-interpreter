module Lambda.TreeView exposing(..)

import Html exposing (Html, div, text)
import Html.Attributes as A
import Html.Events exposing (onClick)
import Set exposing (Set)
import String
import Svg exposing (Svg)
import Svg.Attributes as SA

import Lambda.Ast exposing (..)
import Lambda.Eval exposing (..)
import Lambda.Model exposing (..)


renderTerm : Model -> Float -> Html Msg
renderTerm model factor =
  let 
    term1 = layoutTerm (0,yoffset+10) model.term
    c = Tuple.first <| getAnn term1
    term2 = shiftX 0 <| term1
    width = round (factor * toFloat (c.w + 10))
    height = round (factor * toFloat (c.h + 20))
  in
    Svg.svg 
    [ SA.width (toString width)
    , SA.height (toString height)
    , SA.transform ("scale(" ++ toString factor ++ "," ++ toString factor ++ ")")
    ]
    (
      drawTerm model [] <| term2
    )


type alias TermCoords =
  { x : Int
  , y : Int
  , w : Int
  , h : Int
  , xoff : Int
  }


shiftXTermCoords : Int -> TermCoords -> TermCoords
shiftXTermCoords xoff to =
  { to | x = to.x + xoff }


fontSize : Int
fontSize = 8


yoffset : Int
yoffset = 10


shiftX xoff term =
  case term of
    Var (c,p) name ->
      Var (shiftXTermCoords xoff c, p) name

    Abs (c,p) name body ->
      Abs (shiftXTermCoords xoff c, p) name (shiftX (xoff + c.xoff) body)

    App (c,p) a b ->
      App (shiftXTermCoords xoff c, p) (shiftX (xoff + c.xoff) a) (shiftX (xoff + c.xoff) b)


layoutTerm : (Int, Int) -> Term2 -> Term (TermCoords, TermInfo)
layoutTerm (x,y) term =
  case term of
    Var p name ->
      let 
        width = String.length name * fontSize
        coords = {x=x + (width // 2), y=y, w=width, h=yoffset, xoff=0}
      in
        Var (coords, p) name

    Abs p name body ->
      let 
        width = (String.length name + 2) * fontSize
        body1 = layoutTerm (x, y + (yoffset*2)) body
        (c,_) = getAnn body1
        width1 = max width c.w
        xoff = if width < c.w then 0 else ((width - c.w) // 2)
        coords = {x=x + (width1 // 2), y=y, w=width1, h=c.h+ (yoffset*2), xoff=xoff}
      in
        Abs (coords, p) name body1
              
    App p a b ->
      let 
        a1 = layoutTerm (x,y + (yoffset*2)) a
        (ca,_) = getAnn a1
        b1 = layoutTerm (x + ca.w + fontSize,y + (yoffset*2)) b
        (cb,_) = getAnn b1
        width = ca.w+cb.w+fontSize
        coords = {x=x+ (width//2), y=y, w=width, h=(max ca.h cb.h) + (yoffset*2), xoff=0}
      in
        App (coords, p) a1 b1


drawTerm : Model -> Path -> Term (TermCoords, TermInfo) -> List (Svg Msg)
drawTerm model path term =
  case term of
    Var (c, _) name ->
      [ Svg.text_ 
          [ SA.x (toString c.x)
          , SA.y (toString c.y)
          , SA.textAnchor "middle"
          , SA.class "ani"
          ] 
          [
            Svg.text name
          ]
      ]

    Abs (c, _) name body ->
      [ Svg.text_
          [ SA.x (toString c.x)
          , SA.y (toString c.y)
          , SA.textAnchor "middle"
          , SA.class "ani"
          ] 
          [
            Svg.text ("\\" ++ name)
          ]
      , drawLine (c.x, c.y) (let (c1,_) = getAnn body in (c1.x, c1.y-fontSize))
      ] ++ drawTerm model (0::path) body

    App (c, _) a b ->
      let 
        label =
          case a of 
            Abs p2 name body ->
              let 
                p_arg = getAnn b 
                collisions = Set.intersect (Tuple.second p2).bound (Tuple.second p_arg).free
                isBeta = not model.alpha || (Set.isEmpty <| collisions)
              in 
                Svg.text_
                  [ SA.x (toString c.x)
                  , SA.y (toString c.y)
                  , SA.textAnchor "middle"
                  --, A.color (if isBeta then "blue" else "red")
                  , SA.style ("fill: " ++ if isBeta then "blue" else "red")
                  , onClick (ReduceAt path)
                  , SA.class "ani"
                  ] 
                  [
                    Svg.text "@"
                  ]
            _ 
              -> 
                Svg.text_
                  [ SA.x (toString c.x)
                  , SA.y (toString c.y)
                  , SA.textAnchor "middle"
                  , SA.class "ani"
                  ] 
                  [
                    Svg.text "@"
                  ]
      in 
        [ label
        , drawLine (c.x, c.y) (let (c1,_) = getAnn a in (c1.x, c1.y-fontSize))
        , drawLine (c.x, c.y) (let (c1,_) = getAnn b in (c1.x, c1.y-fontSize))
        ] ++ (drawTerm model (0::path) a ++ drawTerm model (1::path) b)


drawLine : (Int, Int) -> (Int, Int) -> Svg msg
drawLine (x1,y1) (x2,y2) =
  Svg.line 
  [ SA.x1 <| toString x1
  , SA.y1 <| toString y1
  , SA.x2 <| toString x2
  , SA.y2 <| toString y2
  , SA.stroke "grey"
  , A.attribute "stroke-opacity" "0.5"
  , SA.strokeWidth "2"
  , SA.class "ani"
  ] []
