module Logger
  ( hakuLog
  , hakuLogger
  ) where

import           Types

import           Data.Foldable       (for_)
import           Data.List           (isPrefixOf)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (getZonedTime)

import           System.Console.ANSI


import           Text.Parsec
import           Text.Parsec.String

colorParser ∷ Parser [SGR]
colorParser = choice
  [ string "Default"  *> pure [ SetColor Foreground Dull Cyan ]
  , string "Red"      *> pure [ SetColor Foreground Dull Red ]
  , string "Magenta"  *> pure [ SetColor Foreground Dull Magenta ]
  , string "Green"    *> pure [ SetColor Foreground Dull Green ]
  , string "Yellow"   *> pure [ SetColor Foreground Dull Yellow ]
  , string "Blue"     *> pure [ SetColor Foreground Dull Blue ]
  , string "White"    *> pure [ SetColor Foreground Dull White ]
  ]

textParser ∷ Parser String
textParser = many (noneOf "<>")

pairParser ∷ [SGR] -> Parser ([SGR], String)
pairParser defaultColor = do
    _     <- char '<'
    color <- colorParser <|> return defaultColor
    _     <- char '>'
    text  <- textParser
    pure (color, text)

stringParser ∷ Parser [([SGR], String)]
stringParser = do
    defaultColor  <- pairParser [ SetColor Foreground Dull Cyan ]
    restPairs     <- many (pairParser (fst defaultColor))
    pure (defaultColor : restPairs)

parseString :: String -> Either ParseError [([SGR], String)]
parseString input = parse stringParser "" input

hakuLogger ∷ String -> IO ()
hakuLogger msg = do
  setSGR [ SetColor Foreground Vivid Magenta ]
  putStr ∘ formatTime defaultTimeLocale "%F %T" =<< getZonedTime
  setSGR [ SetConsoleIntensity BoldIntensity ]
  putStr " "
  let input = if "<" `isPrefixOf` msg
                then msg
                else "<Default>" ++ msg
  for_ (msgWithColors input) $ \(sgr, chunk) ->
    setSGR sgr >> putStr chunk
  setSGR [ Reset ]
  putStrLn []
 where msgWithColors ∷ String -> [( [SGR], String )]
       msgWithColors input =
          case parseString input of
            Left  err ->
              [ ([ SetColor Foreground Vivid Red ], ("log parse error: " ++ show err ++ "\n"))
              , ([ SetColor Foreground Dull Cyan ], msg) ]
            Right result -> result

hakuLog ∷ String -> HakuEnv -> IO ()
hakuLog = runReaderT ∘ hLogM
 where hLogM ∷ HakuMonad m ⇒ String -> m ()
       hLogM msg = liftIO ∘ flip logger msg =<< ask
