{-# LANGUAGE OverloadedStrings #-}
module Args (options) where


import Types
import Options.Applicative
import qualified Data.Text as T


config :: Parser Config
config = Config <$> pure True
                <*> pure defaultColors
                <*> (fmap T.pack . strOption $ (long "title" <> short 't' <> help "The title of the diagram"))
                <*> option auto (long "size" <> short 's' <> help "The size of the diagram")


options :: Parser ProgramOpts
options = ProgramOpts <$> (strOption (long "input" <> short 'i' <> help "The persistent file to read"))
                      <*> (strOption (long "output" <> short 'o' <> help "The output ER file"))
                      <*> config
