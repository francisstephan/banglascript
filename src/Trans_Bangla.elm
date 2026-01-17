module Trans_Bangla exposing (transl)

import Dict
import Set exposing (Set)

selectFull: Set String
selectFull = Set.fromList [" ","a","a'","i","i'","u","u'","e","e'","o","o'","o\"","r'"]

vowel: Set String
vowel = Set.fromList ["a","a'","i","i'","u","u'","e","e'","o","o'","o\"","r'"]

-- vowel rendering:
-- if preceding char is in selectFull, use full vowel
-- else use matra vowel

latFull : Dict.Dict String String
latFull = Dict.fromList[("a" , "\u{0985}")
                       ,("a'" , "\u{0986}")
                       ,("i" , "\u{0987}")
                       ,("i'" , "\u{0988}")
                       ,("u" , "\u{0989}")
                       ,("u'" , "\u{098A}")
                       ,("e" , "\u{098F}")
                       ,("e'" , "\u{0990}")
                       ,("o" , "\u{0993}")
                       ,("o'" , "\u{0994}")
                       ,("r'" , "\u{098B}")
                       ]

latMatra : Dict.Dict String String
latMatra = Dict.fromList[("a" , "")
                        ,("a'" , "\u{09BE}")
                        ,("i" , "\u{09BF}")
                        ,("i'" , "\u{09C0}")
                        ,("u" , "\u{09C1}")
                        ,("u'" , "\u{09C2}")
                        ,("e" , "\u{09C7}")
                        ,("e'" , "\u{09C8}")
                        ,("o" , "\u{09CB}")
                        ,("o'" , "\u{09CC}")
                        ,("r'" , "\u{09C3}")
                        ]

latBangla : Dict.Dict String String
latBangla = Dict.fromList [("k" , "\u{0995}") ,("k'" , "\u{0996}")
                         ,("g" , "\u{0997}"), ("g'" , "\u{0998}"), ("G" , "\u{0999}")

                         ,("c" , "\u{099A}"), ("c'" , "\u{099B}")
                         ,("j" , "\u{099C}"), ("j'" , "\u{099D}"), ("n'" , "\u{099E}")

                         ,("T" , "\u{099F}"), ("T'", "\u{09A0}")
                         ,("D" , "\u{09A1}"), ("D'" , "\u{09A2}"), ("N" , "\u{09A3}")
                         ,("R" , "\u{09DC}"), ("R'", "\u{09DD}")
                         ,("t" , "\u{09A4}"), ("t'" , "\u{09A5}")
                         ,("d" , "\u{09A6}"), ("d'" , "\u{09A7}"), ("n" , "\u{09A8}")
                         ,("t\"", "\u{09CE}")
                         ,("p" , "\u{09AA}"), ("p'" , "\u{09AB}"), ("f" , "\u{09AB}")
                         ,("b" , "\u{09AC}"), ("b'" , "\u{09AD}"), ("v" , "\u{09AD}"), ("m" , "\u{09AE}")

                         ,("y" , "\u{09AF}"), ("y'" , "\u{09DF}"), ("r" , "\u{09B0}"), ("l" , "\u{09B2}")
                         ,("s'" ,"\u{09B6}"), ("s\"","\u{09B7}"), ("s" , "\u{09B8}"),("h","\u{09B9}")
                         ,("(" , "\u{0981}"),("('" , "\u{0982}") -- bangla candrabindu, anusvara
                         ,("-" , "\u{09CD}")  -- virama
                         ,("0","\u{09E6}"),("1","\u{09E7}"),("2","\u{09E8}"),("3","\u{09E9}"),("4","\u{09EA}")
                         ,("5","\u{09EB}"),("6","\u{09EC}"),("7","\u{09ED}"),("8","\u{09EE}"),("9","\u{09EF}")
                         , ("." , "\u{0964}") --  দাড়ি Dari
                         , (".'" , "॰")
                         , (":'" , "\u{0983}")
                         , ("æ" , "\u{09CD}\u{09AF}\u{09BE}")
                         , ("_" , "") -- filter _
                         ]

latBangla_ : Dict.Dict String String -- characters to be preceded by _ (underscore)
latBangla_= Dict.fromList  [("g" , "\u{095A}") -- use rather g" , see above
                          , ("(" , "(")
                          ]

diacritics : List String
diacritics = ["'", "\""]

subst : String -> (Dict.Dict String String) -> String -- substitute one char (or char + diacritics) on the basis of dictionary
subst car dict =
  Maybe.withDefault car (Dict.get car dict) -- if car is in dict, substitute, else keep car

subst_ : (String,String) -> String -- select dictionary on the basis of previous char : _ or not _, and substitute char
subst_ dble =
  let
     (carac, sub) = dble
  in
    if sub == "_" then subst carac latBangla_ else
      if Set.member carac vowel then
        if Set.member sub selectFull then subst carac latFull
        else subst carac latMatra
    else subst carac latBangla

szip : List String -> List (String,String) -- zip s with a right shift of itself
szip s =
    List.map2 Tuple.pair s (" " :: s)

foldp : List String -> List String -> List String -- concatenate letters with their diacritics, if any
foldp acc list =
  case list of
    [] ->
      acc
    x::xs ->
      case xs of
        [] ->
          x::acc
        y::ys ->
          if List.member y diacritics then -- 1 diacritic
            case ys of
              [] ->
                (x++y)::acc
              _ ->
                foldp ((x++y)::acc) ys
          else
            foldp (x::acc) xs

trich : String -> String -- sort diacritics, if more than 1 present
trich s =
  if String.length s < 3 then s -- 0 or 1 diacritic, no need to sort
  else
    let
      h = String.slice 0 1 s -- head character
      b = String.slice 1 5 s -- b contains the diacritics, which will be sorted according to Unicode value
    in
      h ++ (b |> String.toList |> List.map String.fromChar |> List.sort |> List.foldr (++) "")

transl : String -> String
transl chaine =
    chaine
    |> String.toList
    |> List.map String.fromChar
    |> foldp []
    |> List.reverse
    |> List.map trich
    |> szip
    |> List.map subst_
    |> List.foldr (++) ""
