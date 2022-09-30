module Portage.Config
  where

data PortageConfig =  PortageConfig
                        {
                           config ::  String,
                           tree   :: String
                        }

portageConfig :: IO PortageConfig
portageConfig = do
  let config = "A"
  let tree = "B"
  return ( PortageConfig config tree )
