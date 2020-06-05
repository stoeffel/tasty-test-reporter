module Test.Console.Color
  ( red,
    magenta,
    black,
    blue,
    yellow,
    green,
    grey,
    underlined,
    Colored (Colors, NoColors),
    Style,
    Styled (..),
    unlines,
    style,
    styled,
    unstyled,
  )
where

import Data.Function (on)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Prelude hiding (unlines)
import qualified System.Console.ANSI as Console
import System.Console.Concurrent (Outputable (..))

type Style = [Console.SGR]

code :: IsString a => Style -> a
code = fromString . Console.setSGRCode

newtype Styled a = Styled { getStyled :: Seq (Either [Console.SGR] a) }

instance Semigroup (Styled a) where
  (<>) a b = Styled (on (<>) getStyled a b)

instance Monoid (Styled a) where
  mempty = Styled mempty

instance IsString a => IsString (Styled a) where
  fromString = Styled . Seq.singleton . Right . fromString

instance (IsString a, Outputable a) => Outputable (Styled a) where
  toOutput = foldMap (either code toOutput) . getStyled

unlines :: IsString t => [Styled t] -> Styled t
unlines [] = mempty
unlines (t : ts) = t <> fromString "\n" <> unlines ts

style :: Style -> Styled a
style col = (Styled . Seq.singleton) (Left col)

styled :: Style -> t -> Styled t
styled col t =
  style col <> (Styled . Seq.singleton) (Right t) <> style [reset]

unstyled :: Monoid a => Styled a -> a
unstyled = foldMap (either mempty id) . getStyled

data Colored = Colors | NoColors

reset :: Console.SGR
reset = Console.Reset

red :: Console.SGR
red = Console.SetColor Console.Foreground Console.Dull Console.Red

magenta :: Console.SGR
magenta = Console.SetColor Console.Foreground Console.Dull Console.Magenta

blue :: Console.SGR
blue = Console.SetColor Console.Foreground Console.Dull Console.Blue

yellow :: Console.SGR
yellow = Console.SetColor Console.Foreground Console.Dull Console.Yellow

green :: Console.SGR
green = Console.SetColor Console.Foreground Console.Dull Console.Green

grey :: Console.SGR
grey = Console.SetColor Console.Foreground Console.Vivid Console.Black

black :: Console.SGR
black = Console.SetColor Console.Foreground Console.Dull Console.White

underlined :: Console.SGR
underlined = Console.SetUnderlining Console.SingleUnderline
