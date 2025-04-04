-- Homework 3 
-- Connor Baldes

module Sentence where
-- Grammar for the animal sentence language:
--
--   <sentence> ->  <noun> <verb> [<noun>]  
--                |  <sentence> `and` <sentence>
--  
--   <noun>  -> <adj> <noun> | <noun> `and` <noun>
-- | `cats` | `dogs` | `ducks` | `bunnies`
--   <verb> ->  `chase` | `cuddle` | `hug` | `scare`
--   <adj> -> `silly` | `small` | `old` | `happy`

data Sentence
   = NVN Noun Verb Noun-- finish noun verb noun sentence
   | NV Noun Verb-- finish noun verb sentence
   | And Sentence Sentence-- finish sentence and sentence
   | End
  deriving (Eq,Show)

data Adj = Silly | Small | Old | Happy -- finish adjectives
  deriving (Eq,Show)

data Noun = Cats | Dogs | Ducks | Bunnies-- finish
    | NP Adj Noun  -- Noun phrase
    | NAnd Noun Noun  -- Finish noun and noun
  deriving (Eq,Show)

data Verb = Chase | Cuddle | Hug | Scare  -- finish
  deriving (Eq,Show)
-- | The sentence: cats cuddle ducks and dogs cuddle ducks
ex1 :: Sentence
ex1 = NVN Cats Hug Dogs
ex2 :: Sentence
ex2 = NVN (NP Silly Cats) Hug Dogs
ex3 :: Sentence
ex3 = NVN (NAnd Dogs Cats) Chase Ducks
ex4 :: Sentence
ex4 = NVN (NAnd (NP Silly Dogs) Cats) Chase Ducks

-- | Build a sentence from a noun verb noun.
-- | buildS2 Cats Hug Cats
-- | NVN Cats Hug Cats
buildS2 :: Noun -> Verb -> Noun -> Sentence
buildS2 = NVN

-- | Build a sentence from a noun verb 
-- | buildS1 Cats Hug 
-- | NV Cats Hug 
buildS1 :: Noun -> Verb ->Sentence
buildS1 = NV

-- | Build a noun phrase from an adjective and noun
-- | buildNP Silly Dogs
-- | NP Silly Dogs
buildNP :: Adj -> Noun -> Noun
buildNP = NP

-- | Build a noun conjunction from two nouns
-- | buildNAnd Dogs Cats
-- | NAnd Dogs Cats
buildNAnd :: Noun -> Noun -> Noun
buildNAnd = NAnd

-- | Build a sentence that is a conjunction of a list of other sentences.
-- | conjunction [ex1, ex2]
-- | And (NVN Cats Hug Dogs) (NVN (NP Silly Cats) Hug Dogs)
--  
conjunction :: [Sentence] -> Sentence
conjunction []    = End
conjunction [s] = s
conjunction (s:ss) = And s (conjunction ss)

-- | Pretty print a sentence.
pretty :: Sentence -> String
pretty (NVN s v o) = prettyNoun s ++ " " ++ prettyVerb v ++ " " ++ prettyNoun o
pretty (And l r)   = pretty l ++ " and " ++ pretty r
pretty (NV s v)     = prettyNoun s ++ " " ++ prettyVerb v
pretty (End) = "."
-- | Pretty print a noun.
prettyNoun :: Noun -> String
prettyNoun Cats  = "cats"
prettyNoun Dogs  = "dogs"
prettyNoun Ducks  = "ducks"
prettyNoun Bunnies  = "bunnies"

-- finish

prettyNoun (NP a n) = prettyAdj a ++ " " ++ prettyNoun n
prettyNoun (NAnd m n) = prettyNoun m ++ " and " ++prettyNoun n
-- | Pretty print a verb.
prettyVerb :: Verb -> String
prettyVerb Chase  = "chase"
prettyVerb Cuddle  = "cuddle"
prettyVerb Hug = "hug"
prettyVerb Scare  = "scare"
-- finish

-- | Pretty print an adjective.
prettyAdj :: Adj -> String
prettyAdj Silly  = "silly"
prettyAdj Small  = "small"
prettyAdj Old  = "old"
prettyAdj Happy  = "happy"
-- finish

-- | Does the sentence contain only cuddling and hugs?
-- | isNice ex2
-- |   True
isNice :: Sentence -> Bool
isNice (NVN _ Chase _)  = False
isNice (NVN _ Cuddle _) = True
isNice (NVN _ Hug _) = True
isNice (NVN _ Scare _) = False
isNice _ = False

-- |Count the number of words in a sentence
-- | wordCount ex4
--    6
wordCount :: Sentence -> Int
wordCount (NVN (NP _ _) _ _) = 4
--wordCount (NVN ( NAnd _ _) _ _) = 5
wordCount (NVN (NAnd (NP _ _) _) _ _) = 6
wordCount (NVN _ _ _) = 3
wordCount (NV _ _) = 2
wordCount (And s1 s2) = wordCount s1 + wordCount s2
wordCount End = 0

-- finish
