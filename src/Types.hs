{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text (Text)


data Config = Config
    { useHaskellName :: Bool
    , colors :: [Color]
    , title :: Text
    , size :: Int
    }

data Color = Color { unColor :: Text }

defaultColors :: [Color]
defaultColors = [
      Color "#1abc9c"
    , Color "#2ecc71"
    , Color "#3498db"
    , Color "#9b59b6"
    , Color "#f39c12"
    , Color "#d35400"
    , Color "#e74c3c"
    , Color "#bdc3c7"
    , Color "#95a5a6"
    , Color "#7f8c8d"
    ]



data ProgramOpts = ProgramOpts
    { fileIn  :: String
    , fileOut :: String
    , conf    :: Config
    }
