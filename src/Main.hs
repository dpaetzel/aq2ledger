{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Main
Description : Main entry point
Copyright   : David Pätzel, 2019
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

This is the main entry point of the @buchhaltung@ executable.
-}
module Main where

import Buchhaltung
import Buchhaltung.Aqbanking
import Buchhaltung.Aqbanking.Request
import Buchhaltung.Format
import Buchhaltung.Parse
import Buchhaltung.Prelude hiding (option)
import Control.Arrow ((&&&))
import Data.Decimal
import Data.List (lookup)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Data.Time.Format -- (defaultTimeLocale, parseTimeM)
import Hledger.Data
import Hledger.Read
import Options.Applicative
import Test.QuickCheck.Hledger hiding (def)
import TestConnections
import Text.Parsec hiding (option)

-- TODO Add better parse errors here
dateOption :: Mod OptionFields Day -> Parser Day
dateOption =
  option . eitherReader $ \s -> parseTimeM True defaultTimeLocale "%Y%m%d" s

optionalDateOption :: Mod OptionFields (Maybe Day) -> Parser (Maybe Day)
optionalDateOption =
  option
    . fmap Just
    . eitherReader
    $ \s -> parseTimeM True defaultTimeLocale "%Y%m%d" s

data Options
  = Download
      { from :: Day,
        to :: Maybe Day
        -- configFile :: FilePath
      }
  | Print
      { from :: Day,
        to :: Maybe Day
        -- configFile :: FilePath
      }
  deriving (Show)

downloadOptions :: Parser Options
downloadOptions =
  Download
    <$> dateOption
          ( long "from"
              <> short 'f'
              <> metavar "YYYYMMDD"
              <> help "First date to download transactions for"
          )
    <*> optionalDateOption
          ( long "to"
              <> short 't'
              <> metavar "YYYYMMDD"
              <> help
                   "Last date to download transactions for (leave out for\
                   \ 'today')"
              <> value Nothing
          )

printOptions :: Parser Options
printOptions =
  Print
    <$> dateOption
          ( long "from"
              <> short 'f'
              <> metavar "YYYYMMDD"
              <> help "Only print transactions after this date"
          )
    <*> optionalDateOption
          ( long "to"
              <> short 't'
              <> metavar "YYYYMMDD"
              <> help
                   "Only print transactions before this date (leave out for\
                   \ 'today')"
              <> value Nothing
          )

options :: Parser Options
options =
  subparser
    ( command "download"
        ( info downloadOptions (progDesc "Download transactions")
        )
        <> command "print"
             ( info printOptions (progDesc "Print downloaded transactions")
             )
    )

main :: IO ()
main = main' =<< execParser options'
  where
    options' =
      info (options <**> helper)
        ( fullDesc
            -- <> progDesc "Wrapper around aqbanking"
            <> header
                 "buchhaltung – gettin' your online banking stuff into\
                 \ (H)ledger"
        )

main' :: Options -> IO ()
main' opts = do
  putText . show $ opts
  let con = testConnectionKSK2
  case opts of
    Download {} ->
      putText "Download not yet supported by CLI"
    Print from to -> do
      s <- runReaderT localTransactions con
      case parse (listtrans fromIBANDE) "" (T.unpack s) of
        Left err -> putText . show $ err
        Right t ->
          sequence_
            $ putStrLn . showTransaction
              <$> (restrictTxs from to . t $ accountNameMap con)

restrictTxs :: Day -> Maybe Day -> [Transaction] -> [Transaction]
restrictTxs from Nothing = filter (\tx -> from <= tdate tx)
restrictTxs from (Just to) = filter (\tx -> from <= tdate tx && tdate tx <= to)

-- TODO Text/String things are bad here and elsewhere
accountNameMap :: ConnectionConfig -> (String, String) -> Text
accountNameMap conf x =
  T.pack
    . fromMaybe "TODO"
    . lookup x
    . fmap (first (T.unpack $ blz conf,))
    $ accounts (parent conf)

