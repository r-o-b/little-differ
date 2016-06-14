import Html exposing (Html, Attribute, div, input, span, text, textarea, node, em, p, header, footer, main')
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onInput)
import Html.App as Html
import String
import List exposing (map2, head, tail)
import Json.Decode exposing (Decoder, at, string, int, object2)
import Json.Encode as Json
import Maybe


main : Program Never
main =
  Html.beginnerProgram
    { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name1 : String
  , name2 : String
  , text1 : String
  , text2 : String
  }


model : Model
model =
  Model "Text 1" "Text 2" "" ""


-- UPDATE

type Msg
    = Name1 String
    | Name2 String
    | Text1 String
    | Text2 String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Name1 name1 ->
      { model | name1 = name1 }
      
    Name2 name2 ->
      { model | name2 = name2 }

    Text1 text1 ->
      { model | text1 = text1 }

    Text2 text2 ->
      { model | text2 = text2 }


-- VIEW

view : Model -> Html Msg
view model =
  let
    text1 = model.text1
    text2 = model.text2
    lines1 = String.split "\n" text1
    lines2 = String.split "\n" text2
    lineCount1 = List.length lines1
    lineCount2 = List.length lines2
    isSameLineCounts = lineCount1 == lineCount2
    textThatsLonger =
      if lineCount1 > lineCount2 then
        "Text 1"
      else
        "Text 2"
    minLineCount = Basics.min lineCount1 lineCount2
    lines1Overlap = List.take minLineCount lines1
    lines2Overlap = List.take minLineCount lines2
    isOverlapSame = isLinesSame lines1Overlap lines2Overlap
    isOverlapSameNoCase = isLinesSameNoCase lines1Overlap lines2Overlap
    isOverlapSameNoSpace = isLinesSameNoSpace lines1Overlap lines2Overlap
    isOverlapSameNoCaseSpace = isLinesSameNoCaseSpace lines1Overlap lines2Overlap
    validationMessage =
      if isTextBlank text1 && isTextBlank text2 then
        span [style [("color", "black")]] [text "Ready."]
      else if isTextBlank text1 then
        span [style [("color", "black")]] [text "Waiting for Text 1."]
      else if isTextBlank text2 then
        span [style [("color", "black")]] [text "Waiting for Text 2."]
      else if isOverlapSame then
        if isSameLineCounts then
          span [style [("color", "green")]] [text "Texts match."]
        else
          span [style [("color", "green")]] [text (textThatsLonger ++ " has more lines, but the overlapping lines match.")]
      else if isOverlapSameNoCase then
        if isSameLineCounts then
          span [style [("color", "#8a6d3b")]] [text "Text 1 and Text 2 match except case (capitalization)."]
        else
          span [style [("color", "#8a6d3b")]] [text (textThatsLonger ++ " has more lines, but the overlapping lines match except case (capitalization).")]
      else if isOverlapSameNoSpace then
        if isSameLineCounts then
          span [style [("color", "#8a6d3b")]] [text "Text 1 and Text 2 match except whitespace (spaces and tabs)."]
        else
          span [style [("color", "#8a6d3b")]] [text (textThatsLonger ++ " has more lines, but the overlapping lines match except whitespace (spaces and tabs).")]
      else if isOverlapSameNoCaseSpace then
        if isSameLineCounts then
          span [style [("color", "#8a6d3b")]] [text "Text 1 and Text 2 match except case and whitespace (capitalization, spaces, and tabs)."]
        else
          span [style [("color", "#8a6d3b")]] [text (textThatsLonger ++ " has more lines, but the overlapping lines match except case and whitespace (capitalization, spaces, and tabs).")]
      else
        if isSameLineCounts then
          span [style [("color", "#B94A48")]] [text "Text 1 and Text 2 don't match."]
        else
          span [style [("color", "#B94A48")]] [text (textThatsLonger ++ " has more lines, and the overlapping lines don't match.")]
    lineNumColors =
      makeColorList lines1 lines2
  in
    div [class "likeBody"]
      [ header [] [ fieldInput "text" Name1 "Name 1" model.name1
                  , fieldInput "text" Name2 "Name 2" model.name2 ]
      , main' [tabindex -1] [ div [id "scrolltogether", class "layout horizontal"] [ fieldTextarea Text1 "Text 1" text1 True
                                                                                   , fieldLineNumbers lineNumColors
                                                                                   , fieldTextarea Text2 "Text 2" text2 False ] ]
      , footer [] [validationMessage]
      ]


fieldInput : String -> (String -> Msg) -> String -> String -> Html Msg
fieldInput fieldType toMsg name content =
    input
      [ type' fieldType
      , placeholder name
      , Html.Attributes.title "Click to rename" -- include namespace of title attribute to avoid conflict with port for title tag
      , tabindex -1
      , value content
      , onInput toMsg
      ]
      []


fieldTextarea : (String -> Msg) -> String -> String -> Bool -> Html Msg
fieldTextarea toMsg name content focus =
  ironAutogrowTextarea
    [ rows 20
    , autofocus focus
    , class "flex-auto"
    , value content
    , onInput toMsg
    ]
    []

    
fieldLineNumbers : String -> Html Msg
fieldLineNumbers colorList =
  lineNumbers
    [ colors colorList
    , class "flex-none"
    ]
    []
    

makeColorList : List String -> List String -> String
makeColorList linesOne linesTwo =
    let
      lengthMin = 21 -- to match the minimum number of textarea rows + 1
      lengthOne = List.length linesOne
      lengthTwo = List.length linesTwo
      longestLength = Basics.max lengthOne lengthTwo
      shortestLength = Basics.min lengthOne lengthTwo
      extraLinesToMin = Basics.max (lengthMin - longestLength) 0 -- how many lines to get up to minimum
      extraLinesLenth = longestLength - shortestLength + extraLinesToMin + 1 -- always have at least 1 extra line, because textareas always have at least 1 blank row at bottom
      extraLines = List.repeat extraLinesLenth "black"
      overlappingLines = List.map2 linesToColor linesOne linesTwo
    in
      join (overlappingLines ++ extraLines)
    
    
linesToColor : String -> String -> String
linesToColor lineFrom1 lineFrom2 =
    if lineFrom1 == lineFrom2 then
      "green" -- exact match
    else
      {-- if String.toUpper lineFrom1 == String.toUpper lineFrom2 then
        "yellow" -- match except for case
      else
        if String.words lineFrom1 == String.words lineFrom2 then
          "blue" -- match except for whitespace
        else
          "red" -- don't match
      --}
      if toUpperWords lineFrom1 == toUpperWords lineFrom2 then
        "yellow" -- match except for case and/or whitespace
      else
        "red"

        
-- VIEW HELPERS

toUpperWords : String -> List String
toUpperWords theText =
  theText |> String.toUpper |> String.words


isTextBlank : String -> Bool
isTextBlank str =
  String.length str == 0
  

isLinesSame : List String -> List String -> Bool
isLinesSame list1 list2 =
  list1 == list2


isLinesSameNoCase : List String -> List String -> Bool
isLinesSameNoCase list1 list2 =
  let
    list1NoCase = List.map String.toUpper list1
    list2NoCase = List.map String.toUpper list2
  in
    list1NoCase == list2NoCase


removeWhitespace : String -> String
removeWhitespace str =
  String.words str |> join
    
    
isLinesSameNoSpace : List String -> List String -> Bool
isLinesSameNoSpace list1 list2 =
  let
    list1NoSpace = List.map removeWhitespace list1
    list2NoSpace = List.map removeWhitespace list2
  in
    list1NoSpace == list2NoSpace


isLinesSameNoCaseSpace : List String -> List String -> Bool
isLinesSameNoCaseSpace list1 list2 =
  let
    list1NoCaseSpace = List.map String.toUpper list1 |> List.map removeWhitespace
    list2NoCaseSpace = List.map String.toUpper list2 |> List.map removeWhitespace
  in
    list1NoCaseSpace == list2NoCaseSpace


-- HTML HELPERS

ironAutogrowTextarea : List (Attribute msg) -> List (Html msg) -> Html msg
ironAutogrowTextarea =
    node "iron-autogrow-textarea"


lineNumbers : List (Attribute msg) -> List (Html msg) -> Html msg
lineNumbers =
    node "line-numbers"    
    
    
colors : String -> Attribute msg
colors str =
    toProp "colors" str


toProp : String -> String -> Attribute msg
toProp propName str =
  str
    |> Json.string
    |> property propName


tvalue : Decoder String
tvalue =
      at ["target", "value"] string


last : List String -> List String
last list =
  case List.length list of
    0 ->
      []

    1 ->
      list

    _ ->
      last (Maybe.withDefault [] (List.tail list))


join : List String -> String
join list =
  case List.length list of
    0 ->
      ""

    1 ->
      String.join "" list

    _ ->
      ("," ++ (String.join "" (last list)))
        |> String.append
            (List.take ((List.length list) - 1) list
              |> String.join ","
            )