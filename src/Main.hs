module Main
where



import GtkInterface


main = do
    gui <- initMainWindow

    gtkInterfaceMainLoop

    return ()
