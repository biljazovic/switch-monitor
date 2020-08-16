module Lib
  ( Monitor (..),
    Mouse,
    Mode (..),
    nextMousePosition,
    parseMonitors,
    parseMouse,
  )
where

import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Monitor = Mn {sizeX :: Int, sizeY :: Int, offsetX :: Int, offsetY :: Int}
  deriving (Show, Eq)

instance Ord Monitor where
  Mn _ _ x1 y1 <= Mn _ _ x2 y2 = (x1, y1) <= (x2, y2)

type Mouse = (Int, Int)

data Mode = Next | Previous

monitorP :: Parser Monitor
monitorP = do
  space1 >> nonSpace >> space1 >> nonSpace >> space1
  sx <- L.decimal <* char '/' <* L.decimal <* char 'x'
  sy <- L.decimal <* char '/' <* L.decimal
  ofx <- char '+' *> L.decimal
  ofy <- char '+' *> L.decimal
  _ <- many anySingle
  return (Mn sx sy ofx ofy)
  where
    nonSpace = some (anySingleBut ' ')

mouseP :: Parser Mouse
mouseP = do
  x <- string "X=" *> L.decimal <* newline
  y <- string "Y=" *> L.decimal <* many anySingle
  return (x, y)

inMonitor :: Monitor -> Mouse -> Bool
inMonitor (Mn sx sy ofx ofy) (x, y) =
  x >= ofx && x <= ofx + sx
    && y >= ofy
    && y <= ofy + sy

monitorCenter :: Monitor -> (Int, Int)
monitorCenter (Mn sx sy ofx ofy) = (ofx + sx `div` 2, ofy + sy `div` 2)

parseMonitors :: String -> [Monitor]
parseMonitors raw = sort $ mapMaybe (parseMaybe monitorP) (lines raw)

parseMouse :: String -> Mouse
parseMouse raw = fromMaybe (0, 0) $ parseMaybe mouseP raw

nextMousePosition :: Mode -> [Monitor] -> Mouse -> Mouse
nextMousePosition mode ms mouse = monitorCenter (ms !! nex)
  where
    n = length ms
    cur = fromMaybe 0 $ findIndex (flip inMonitor mouse) ms
    nex = (cur + offset + n) `mod` n
    offset = case mode of
      Next -> 1
      Previous -> -1