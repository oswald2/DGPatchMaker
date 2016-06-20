module Main
where


import System.Environment
import System.Console.GetOpt
import System.FilePath

import Data.List
import Data.Maybe


main :: IO ()
main = do
    (conf, _) <- (getArgs >>= parseOpts)
    case optVersion conf of
        True -> putStrLn "Version 1.1"
        False -> do
            writeDGBuilder conf
            writeIPBuilder conf


writeDGBuilder :: CmdOptions -> IO ()
writeDGBuilder conf = do
    content <- readFile (optGladeFile conf)
    let outfile = (optOutFilePath conf </> "DGPatchMakerBuilder.hs")
    putStrLn $ "Processing file " ++ (optGladeFile conf) ++ " into " ++ outfile
    writeFile outfile (newCont content)
    where
        newCont content = concat [header, escapedContent content, footer]
        header  = "module Gtk.DGPatchMakerBuilder\n(builderFileAsString)\nwhere\n\n\nbuilderFileAsString :: String\nbuilderFileAsString = \""
        footer = "\"\n\n"
        escapedContent cont = intercalate "\\n" $ map escapeDoubleQuotes $ lines cont
        escapeDoubleQuotes str = go str
            where go [] = []
                  go (x:xs) = case x of
                                '"' -> '\\' : '"' : go xs
                                _ -> x : go xs

writeIPBuilder :: CmdOptions -> IO ()
writeIPBuilder conf = do
    content <- readFile (optGladeFile1 conf)
    let outfile = (optOutFilePath conf </> "InstrumentPageBuilder.hs")
    putStrLn $ "Processing file " ++ (optGladeFile conf) ++ " into " ++ outfile
    writeFile outfile (newCont content)
    where
        newCont content = concat [header, escapedContent content, footer]
        header  = "module Gtk.InstrumentPageBuilder\n(builderFileAsString)\nwhere\n\n\nbuilderFileAsString :: String\nbuilderFileAsString = \""
        footer = "\"\n\n"
        escapedContent cont = intercalate "\\n" $ map escapeDoubleQuotes $ lines cont
        escapeDoubleQuotes str = go str
            where go [] = []
                  go (x:xs) = case x of
                                '"' -> '\\' : '"' : go xs
                                _ -> x : go xs


data CmdOptions = CmdOptions {
    optVersion :: Bool,
    optGladeFile :: String,
    optGladeFile1 :: String,
    optOutFilePath :: String
    }
    deriving (Show)


defaultCmdOptions :: CmdOptions
defaultCmdOptions =
  CmdOptions {optVersion = False, optGladeFile = "DGPatchMaker.glade", optGladeFile1 = "InstrumentPage.glade", optOutFilePath = "src/Gtk" }

options :: [OptDescr (CmdOptions -> CmdOptions)]
options =
     [Option ['V','?'] ["version"]
         (NoArg (\ opts -> opts { optVersion = True }))
         "show version number"
     , Option ['g']     ["glade-file"]
         (OptArg ((\f opts -> opts { optGladeFile = f }) . fromMaybe "DGPatchMaker.glade") "FILE")
         "GTK builder FILE"
     , Option ['h']     ["glade-file-ip"]
         (OptArg ((\f opts -> opts { optGladeFile1 = f }) . fromMaybe "InstrumentPage.glade") "FILE")
         "GTK builder FILE"
     , Option ['o']     ["output-path"]
         (OptArg ((\f opts -> opts { optOutFilePath = f }) . fromMaybe "src/Gtk") "FILE")
         "Haskell output source FILE"
    ]


     -- , Option ['d']     ["domain"]
     --     (OptArg ((\d opts -> opts { confDomain = d }) . fromMaybe defaultDomain) "DOMAIN")
     --     "the domain prefix (e.g. 3A5)"


parseOpts :: [String] -> IO (CmdOptions, [String])
parseOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (Prelude.foldl (flip id) defaultCmdOptions o, n)
        (_,_,errs) -> ioError (userError (Prelude.concat errs ++ usageInfo header options))
    where header = "Usage: CreateGladeSource [OPTION...]"

