{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Aq2Ledger.Options
Description : Command line options
Copyright   : David Pätzel, 2020
License     : GPL-3
Maintainer  : David Pätzel <david.paetzel@posteo.de>
Stability   : experimental

This module specifies how command line arguments are parsed into an 'Options'
value.
-}
module Aq2Ledger.Options where

import Aq2Ledger.Config
import Aq2Ledger.Prelude hiding (option)
import Data.Time.Calendar (Day)
import Data.Time.Format
import Options.Applicative
import System.Directory (getHomeDirectory)

-- * Defaults for option values

{-|
The default location for the configuration file is
@$HOME\/.config\/buchhaltung\/config.yml@.
-}
defaultConfigFile :: IO FilePath
defaultConfigFile = fmap (++ "/.config/buchhaltung/config.yml") getHomeDirectory

-- * Parsers that are used by multiple subcommands
configFileOption :: Parser (Maybe FilePath)
configFileOption =
  optional . strOption
    $ long "config"
      <> short 'C'
      <> metavar "FILE"
      <> help
           "The config file to use (default:\
           \ $HOME/.config/buchhaltung/config.yml)"

connectionOption :: Parser (Maybe ConnectionName)
connectionOption =
  optional . argument str
    $ metavar "CONNECTIONNAME"
      <> help
           "NOT YET IMPLEMENTED: The name of the configured connection to run\
           \ the command on (leave out for ‘all connections’)"

-- TODO Add better parse errors here
-- TODO Unify used format string with Aq2Ledger.Format.asAqDate
dateOption :: Mod OptionFields Day -> Parser Day
dateOption =
  option . eitherReader $ parseTimeM True defaultTimeLocale "%Y%m%d"

-- * Options datatype
data Options
  = Download
      { from :: Day,
        to :: Maybe Day,
        configFile :: Maybe FilePath
      }
  | Print
      { from :: Day,
        to :: Maybe Day,
        configFile :: Maybe FilePath,
        connection :: Maybe ConnectionName
      }
  | ExampleConfig
  deriving (Show)

-- * Subcommand parsers
downloadOptions :: Parser Options
downloadOptions =
  Download
    <$> dateOption
          ( long "from"
              <> short 'f'
              <> metavar "YYYYMMDD"
              <> help "First date to download transactions for"
          )
      <*> ( optional . dateOption
              $ long "to"
                <> short 't'
                <> metavar "YYYYMMDD"
                <> help
                     "Last date to download transactions for (leave out for\
                     \ 'today')"
          )
      <*> configFileOption

printOptions :: Parser Options
printOptions =
  Print
    <$> dateOption
          ( long "from"
              <> short 'f'
              <> metavar "YYYYMMDD"
              <> help "Only print transactions after this date"
          )
    <*> ( optional . dateOption
            $ long "to"
              <> short 't'
              <> metavar "YYYYMMDD"
              <> help
                   "Only print transactions before this date (leave out for\
                   \ 'today')"
        )
    <*> configFileOption
    <*> connectionOption

-- * Overall option parser
options :: Parser Options
options =
  subparser
    ( command "download"
        ( info downloadOptions (progDesc "Download transactions")
        )
        <> command "print"
             ( info printOptions (progDesc "Print downloaded transactions")
             )
        <> command "exampleconf"
             ( info (pure ExampleConfig) (progDesc "Print downloaded transactions")
             )
    )
