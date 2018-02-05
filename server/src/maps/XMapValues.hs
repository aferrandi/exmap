module XMapValues(Text, XMapValue) where

newtype Text = Text String

class XMapValue a where
    defaultValue :: () -> a

instance XMapValue Int
    where defaultValue () = 0

instance XMapValue Double
    where defaultValue () = 0.0

instance XMapValue Text
    where defaultValue () = Text ""

instance XMapValue Bool
    where defaultValue () = False
