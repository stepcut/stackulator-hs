module Main where

import Control.Monad (msum)
import Happstack.Server (simpleHTTP, nullConf, asContentType, dir, serveDirectory, serveFile, nullDir, Browsing(EnableBrowsing))


main :: IO ()
main =
  simpleHTTP nullConf $ msum
    [ nullDir            >> serveFile (asContentType "text/html") "index.html"
    , dir "pace.js"      $ serveFile (asContentType "application/javascript") "pace.min.js"
    , dir "loading.css"  $ serveFile (asContentType "text/css") "loading.css"
    , serveDirectory EnableBrowsing [] "HtmlView.jsexe"
    ]

{-
  <script language="javascript" src="runmain.js" defer></script>

    <script language="javascript" src="rts.js"></script>
    <script language="javascript" src="lib.js"></script>
    <script language="javascript" src="out.js"></script>

-}
