-- This is a copy of the module in contrib.
-- We can't use that module because it would break bootstrapping.
module Data.String.Iterator

export
record StringIterator where
  constructor MkSI
  string : String

  -- backend-dependent offset into the string
  -- see prim__readChar below
  offset : Int

export
fromString : String -> StringIterator
fromString s = MkSI s 0

private
data ReadResult
  = EOF
  | Character Char Int  -- character, width

-- takes a backend-dependent offset into the string
-- on ML-based backends, this is in bytes
-- in Scheme, this is in codepoints
private
%foreign "scheme:read-string-char"
prim__readChar : Int -> String -> ReadResult

export
uncons : StringIterator -> Maybe (Char, StringIterator)
uncons (MkSI s ofs) =
  case prim__readChar ofs s of
    EOF => Nothing
    Character ch width => Just (ch, MkSI s (ofs + width))

export
foldl : (a -> Char -> a) -> a -> String -> a
foldl f acc s = loop 0 acc
  where
    loop : Int -> a -> a
    loop ofs acc =
      case prim__readChar ofs s of
        EOF => acc
        Character ch width => loop (ofs + width) (f acc ch)
