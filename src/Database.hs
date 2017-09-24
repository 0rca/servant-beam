{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database (
    ShoppingCartDb(..),
    shoppingCartDb
) where

import Database.Beam
import Database.Beam.Sqlite
import Protolude

import User

newtype ShoppingCartDb f
    = ShoppingCartDb
    { _shoppingCartUsers :: f (TableEntity UserT) }
    deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings
