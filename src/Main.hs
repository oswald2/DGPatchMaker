module Main
where



import GtkInterface


import Data.DrumDrops.Types
import Data.DrumDrops.Utils



main :: IO ()
main = do
    gui <- initMainWindow

    gtkInterfaceMainLoop

    return ()
