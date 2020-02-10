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
    styled,
    maybeStyled,
  )
where

import qualified Data.Text
import Data.Text (Text)
import qualified System.Console.ANSI as Console

type Style = [Console.SGR]

code :: Style -> Text
code = Data.Text.pack . Console.setSGRCode

styled :: Style -> Text -> Text
styled col t = code col <> t <> code [reset]

maybeStyled :: Colored -> Style -> Text -> Text
maybeStyled colored styles text =
  case colored of
    Colors -> styled styles text
    NoColors -> text

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
