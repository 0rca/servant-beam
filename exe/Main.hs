{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Protolude

import Database
import Lib
import User

-- | Seeds the database with sample users
insertUsers :: Connection -> IO ()
insertUsers conn = withDatabaseDebug putStrLn {- for debug output -} conn $
    runInsert $ insert (_shoppingCartUsers shoppingCartDb) $
    insertValues [ User "james@example.com" "James" "Smith"  "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                 , User "betty@example.com" "Betty" "Jones"  "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                 , User "sam@example.com"   "Sam"   "Taylor" "332532dcfaa1cbf61e2a266bd723612c" {- sam   -} ]

printAllUsers :: Connection -> IO ()
printAllUsers conn = do
    let allUsers = all_ (_shoppingCartUsers shoppingCartDb)
    withDatabaseDebug putStrLn {- for debug output -} conn $ do
        users <- runSelectReturningList $ select allUsers
        mapM_ (liftIO . print) users

printSortedUsers :: Connection -> IO ()
printSortedUsers conn = do
    let sortUsersByFirstName =
            orderBy_ (\u -> (asc_ (_userFirstName u), desc_ (_userLastName u)))
            (all_ (_shoppingCartUsers shoppingCartDb))
    withDatabaseDebug putStrLn {- for debug output -} conn $ do
        users <- runSelectReturningList $ select sortUsersByFirstName
        mapM_ (liftIO . print) users

printBoundedQuery :: Connection -> IO ()
printBoundedQuery conn = do
    let boundedQuery :: Q SqliteSelectSyntax _ _ _
        boundedQuery = limit_ 1 $ offset_ 1 $
                       orderBy_ (asc_ . _userFirstName) $
                       all_ (_shoppingCartUsers shoppingCartDb)
    withDatabaseDebug putStrLn conn $ do
        users <- runSelectReturningList (select boundedQuery :: SqlSelect SqliteSelectSyntax _)
        mapM_ (liftIO . print) users

printUserCount :: Connection -> IO ()
printUserCount conn = do
    let userCount = aggregate_ (\u -> as_ @Int countAll_)
                               (all_ (_shoppingCartUsers shoppingCartDb))
    withDatabaseDebug putStrLn conn $ do
        Just c  <- runSelectReturningOne (select userCount)
        liftIO $ putText ("We have " <> show c <> " users in the database")

insertMoarUsers :: Connection -> IO ()
insertMoarUsers conn =
    withDatabaseDebug putStrLn conn $
        runInsert $
        insert (_shoppingCartUsers shoppingCartDb) $
        insertValues [ User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f" {- betty -}
                    , User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c" {- james -}
                    , User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c" {- sam -}
                    , User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c" {- sam -} ]

printCountByName :: Connection -> IO ()
printCountByName conn = do
    let numberOfUsersByName =
            aggregate_ (\u -> (group_ (_userFirstName u), as_ @Int countAll_)) $
            all_ (_shoppingCartUsers shoppingCartDb)
    withDatabase conn $ do
        countedByName <- runSelectReturningList $ select numberOfUsersByName
        mapM_ (liftIO . print) countedByName

main :: IO ()
main = do
    conn <- open "shoppingcart.db"
    insertUsers conn
    printAllUsers conn
    printSortedUsers conn
    printBoundedQuery conn
    printUserCount conn
    insertMoarUsers conn
    printCountByName conn
