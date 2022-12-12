{-# LANGUAGE
    DeriveGeneric
  , UnicodeSyntax
  #-}

module Shelter.Types
  ( ShelterConfig
  , ShelterNode (..)
  , getShelterConfig
  , updateShelterConfig
  ) where

import           Paths                 (getShelterConfPath)

import           GHC.Generics

import           Data.Aeson.Types      (defaultOptions, genericToJSON)
import           Data.Yaml

import           Control.Monad         ((<=<))

import qualified Data.ByteString.Char8 as BS

import           System.Directory      (doesFileExist)

data ShelterNode
  = ShelterNode
      { target   :: String
      , task     :: String
      , upstream :: String
      , branch   :: String
      , vcs      :: String
      , hash     :: Maybe String
      }
  deriving (Generic, Show)

type ShelterConfig = [ShelterNode]

instance FromJSON ShelterNode
instance ToJSON ShelterNode where
  toJSON = genericToJSON defaultOptions

ymlDecode ∷ FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
ymlDecode = decodeThrow <=< BS.readFile

ymlEncode ∷ ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
ymlEncode = (. encode) . BS.writeFile

getShelterConfig ∷ IO (Maybe ShelterConfig)
getShelterConfig = getShelterConfPath >>= \shelterCfgPath →
  doesFileExist shelterCfgPath >>= \shelterCfgExists →
    if shelterCfgExists then ymlDecode shelterCfgPath
                        else return Nothing

updateShelterConfig ∷ ShelterConfig → IO ()
updateShelterConfig shelter = getShelterConfPath >>= \shelterCfgPath →
  ymlEncode shelterCfgPath shelter
