module NameParser exposing (..)

import Regex as Regex exposing (..)

nameFromString : String -> Result String String
nameFromString t = if Regex.contains (Regex.regex "\\w[\\w\\d]*") t
                       then Ok t
                       else Err "The text must start with a letter and contain only letters and numbers"