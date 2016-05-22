module Main
where



import GtkInterface



main :: IO ()
main = do
    gui <- initMainWindow

    gtkInterfaceMainLoop

    return ()
