{-# LANGUAGE FlexibleContexts #-}

module Rogui.Components.List
  ( list,
  )
where

import Rogui.Components.Types (Component (..), emptyComponent)
import Rogui.Graphics.Console (TextAlign)
import Rogui.Graphics.DSL.Instructions (Colours, setColours, strLn)

list :: [a] -> (a -> String) -> TextAlign -> Colours -> Colours -> Maybe Int -> Component m
list items toText baseAlignment baseColour highlightedColours selection =
  let displayItem (item, idx) = do
        if (Just idx == selection)
          then setColours highlightedColours
          else setColours baseColour
        strLn baseAlignment (toText item)
   in emptyComponent {draw = \_ _ -> mapM_ displayItem (zip items [1 ..])}