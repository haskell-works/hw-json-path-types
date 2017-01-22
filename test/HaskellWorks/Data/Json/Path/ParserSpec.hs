
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Path.ParserSpec (spec) where

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Path.ParserSpec" $ do
  it "Blah" $ do
    1 `shouldBe` (1 :: Int)

-- $.store.book[*].author	The authors of all books
-- $..author	All authors
-- $.store.*	All things, both books and bicycles
-- $.store..price	The price of everything
-- $..book[2]	The third book
-- $..book[0,1]	The first two books
-- $..book[:2]	All books from index 0 (inclusive) until index 2 (exclusive)
-- $..book[1:2]	All books from index 1 (inclusive) until index 2 (exclusive)
-- $..book[-2:]	Last two books
-- $..book[2:]	Book number two from tail
-- $..book[?(@.isbn)]	All books with an ISBN number
-- $.store.book[?(@.price < 10)]	All books in store cheaper than 10
-- $..book[?(@.price <= $['expensive'])]	All books in store that are not "expensive"
-- $..book[?(@.author =~ /.*REES/i)]	All books matching regex (ignore case)
-- $..*	Give me every thing
-- $..book.length()	The number of books
