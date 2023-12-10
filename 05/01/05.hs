{-# OPTIONS_GHC -Wall #-}

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance (Read a) => Read (Tree a) where
  readsPrec _ ('{' : '}' : s) = [(Leaf, s)]
  readsPrec _ ('<' : s) =
    [ (Node l x r, rest)
      | (l, s') <- reads s,
        (x, s'') <- reads s',
        (r, '>' : rest) <- reads s''
    ]
  readsPrec _ _ = []

-- readPrec :: Read a => ReadPrec (Tree a)
-- readPrec = readLeaf <|> readNode

-- readChar :: Char -> ReadPrec Char
-- readChar c = do
--   c' <- get
--   if c == c' then return c else pfail

-- readLeaf :: (Read a) => ReadPrec (Tree a)
-- readLeaf = Leaf <$ traverse readChar "{}"

-- readNode :: (Read a) => ReadPrec (Tree a)
-- readNode = do
--   _ <- readChar '<'
--   left <- readLeaf <|> readNode
--   x <- readPrec
--   right <- readLeaf <|> readNode
--   _ <- readChar '>'
--   return $ Node left x right
