module Parser.Lexer.Common

import public Text.Lexer

%default total

||| In `comment` we are careful not to parse closing delimiters as
||| valid comments. i.e. you may not write many dashes followed by
||| a closing brace and call it a valid comment.
export
comment : Lexer
comment
   =  is '-' <+> is '-'                  -- comment opener
  <+> many (is '-') <+> reject (is '}')  -- not a closing delimiter
  <+> many (isNot '\n')                  -- till the end of line

export
isOpChar : Char -> Bool
isOpChar c = inLatin || inArrows || inMathematicalOperators
  where
    inRange : (Int, Int) -> Lazy Bool
    inRange (lowerBound, upperBound) = (c >= chr lowerBound && c <= chr upperBound)
    inLatin : Bool
    inLatin = c `elem` (unpack ":!#$%&*+./<=>?@\\^|-~")
    inArrows : Bool
    inArrows = inRange (8592, 8703)
    inMathematicalOperators : Bool
    inMathematicalOperators = inRange (8704, 8959)

nonOpCharUnicode : Char -> Bool
nonOpCharUnicode c = (c > chr 160) && not (isOpChar c)

-- Identifier Lexer
-- There are multiple variants.

public export
data Flavour = AllowDashes | Capitalised | Normal

isIdentStart : Flavour -> Char -> Bool
isIdentStart _           '_' = True
isIdentStart Capitalised  x  = isUpper x || nonOpCharUnicode x
isIdentStart _            x  = isAlpha x || nonOpCharUnicode x

isIdentTrailing : Flavour -> Char -> Bool
isIdentTrailing AllowDashes '-'  = True
isIdentTrailing _           '\'' = True
isIdentTrailing _           '_'  = True
isIdentTrailing _            x   = isAlphaNum x || nonOpCharUnicode x

export %inline
isIdent : Flavour -> String -> Bool
isIdent flavour string =
  case unpack string of
    []      => False
    (x::xs) => isIdentStart flavour x && all (isIdentTrailing flavour) xs

export %inline
ident : Flavour -> Lexer
ident flavour =
  (pred $ isIdentStart flavour) <+>
    (many . pred $ isIdentTrailing flavour)

export
isIdentNormal : String -> Bool
isIdentNormal = isIdent Normal

export
identNormal : Lexer
identNormal = ident Normal

export
identAllowDashes : Lexer
identAllowDashes = ident AllowDashes

namespaceIdent : Lexer
namespaceIdent = ident Capitalised <+> many (is '.' <+> ident Capitalised <+> expect (is '.'))

export
namespacedIdent : Lexer
namespacedIdent = namespaceIdent <+> opt (is '.' <+> identNormal)

export
spacesOrNewlines : Lexer
spacesOrNewlines = some (space <|> newline)
