{-# LANGUAGE OverloadedStrings #-}
module Main where


import Database.Persist.Quasi
import Database.Persist.Types
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)
import Types
import Data.Monoid
import Data.Maybe
import Options.Applicative

import Args


main :: IO ()
main = execParser opts >>= program
    where
        opts = info (helper <*> options)
            (  fullDesc
            <> header "persist2er - Create a ER from a persist's model file"
            )


program :: ProgramOpts -> IO ()
program opts = do
    txt <- T.readFile (fileIn opts)
    let con                   = conf opts
        defs                  = parse lowerCaseSettings txt
        (entities, relations) = unzip . map (convert con) . zip [0..] $ defs
        entitiesTxt           = T.unlines entities
        relationsTxt          = T.unlines relations

    T.writeFile (fileOut opts) (genTitle con <> entitiesTxt <> "\n" <> relationsTxt)




genTitle :: Config -> Text
genTitle config = "title { label: \"" <> title config <> "\", size: \"" <> (T.pack . show . size $ config) <> "\"}"

-- | Generate a entity
--  This return a tuple of an entity and the relations
convert :: Config -> (Int, EntityDef) -> (Text, Text)
convert config (i, edef) = (entity, relations)
    where
        entity = T.unlines $
            [ "[" <> genEntityName config edef <> "] { bgcolor: \"" <> selectColor <> "\"}"
            , genIdField config edef
            ] ++ map (genEntityFields config) (entityFields edef)
            

        relations = T.unlines . catMaybes $ map (genRelationsField config edef) (entityFields edef)

        selectColor = unColor $ (colors config) !! (i `mod` length (colors config))

-- | Get the entity name acording to the config
genEntityName :: Config -> EntityDef -> Text
genEntityName config edef = entityName (useHaskellName config)
    where
        entityName True  = unHaskellName . entityHaskell $ edef
        entityName False = unDBName . entityDB $ edef


-- | Generate the Id of the Entity
genIdField :: Config -> EntityDef -> Text
genIdField config edef = "*" <> genEntityFields config (entityId edef)


genEntityFields :: Config -> FieldDef -> Text
genEntityFields config fdef = genFieldName (useHaskellName config) fdef <> " {label: \"" <> genFieldLabel fdef <> "\"}"
    where

        genFieldName True  = unHaskellName . fieldHaskell
        genFieldName False = unDBName . fieldDB

        genFieldLabel fdef = T.intercalate ", " $
            [ genFieldType (useHaskellName config) fdef
            ] ++ (maybeToList . genFieldNull (useHaskellName config) $ fdef)
            

        genFieldType True  = getType . fieldType
            where
                getType (FTTypeCon _ name) = name
        genFieldType False = getTypeSql . fieldSqlType
            where

        genFieldNull True fdef = if fieldStrict fdef then Nothing else Just "Maybe"
        genFieldNull False fdef = if fieldStrict fdef then Just "NOT NULL" else Just "NULL"


-- | Generate the relations.
genRelationsField :: Config -> EntityDef -> FieldDef -> Maybe Text
genRelationsField config edef fdef =
    case (T.stripSuffix "Id" . getFieldType . fieldType $ fdef) of
        Nothing -> Nothing
        Just x -> Just $ x <> "\t1--*\t" <> genEntityName config edef

    where
        getFieldType (FTTypeCon _ typ) = typ



-- | Convert a SqlType to Tetx.
-- TODO: Currently this dosen't work because we need to know the backend
-- (postgres, mysql, etc) that is begin used by the user.
getTypeSql :: SqlType -> Text
getTypeSql SqlString        = "TEXT"
getTypeSql SqlInt32         = "INT"
getTypeSql SqlInt64         = "INT64"
getTypeSql SqlReal          = "FLOAT"
getTypeSql (SqlNumeric _ _) = "INT"
getTypeSql SqlBool          = "BOOL"
getTypeSql SqlDay           = "DAY"
getTypeSql SqlTime          = "TIME"
getTypeSql SqlDayTime       = "DATETIME"
getTypeSql SqlBlob          = "BLOB"
getTypeSql (SqlOther other) = other
