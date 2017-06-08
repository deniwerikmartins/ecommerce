{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where
import Foundation
import Yesod
import Usuario
import Handlers
import Front
import Index
import Produtos5
import Produtoipad
import Produtops4
import Produtosurface
import Produtotv
import Produtoxbox

-- Application
mkYesodDispatch "Sitio" resourcesSitio
