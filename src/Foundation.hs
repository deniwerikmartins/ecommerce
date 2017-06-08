{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Departamento
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show

Pessoa
   nome Text
   idade Int
   salario Double
   deptoid DepartamentoId
   deriving Show
  
Usuario
   nome Text
   email Text
   senha Text
   UniqueEmail email
   deriving Show

Contato
    nome Text
    email Text
    assunto Text
    mensagem Textarea
    deriving Show
   
Categoria
    nome Text
    deriving Show
   
Produto
    nome Text
    preco Double
    catid CategoriaId
    deriving Show
    
Pedido
    produtoid ProdutoId
    usuarioid UsuarioId
    status Int
    quantidade Int
    deriving Show

    
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "config/routes")

mkMessage "Sitio" "messages" "pt-BR"

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized IndexR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized PedidoR _ = return Authorized
    isAuthorized Produtos5R _ = return Authorized
    isAuthorized ProdutoipadR _ = return Authorized
    isAuthorized Produtops4R _ = return Authorized
    isAuthorized ProdutosurfaceR _ = return Authorized
    isAuthorized ProdutotvR _ = return Authorized
    isAuthorized ProdutoxboxR _ = return Authorized
    isAuthorized ContatoR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized _ _ = isUser

--isAdmin = do
--    mu <- lookupSession "_ID"
--    return $ case mu of
--        Nothing -> AuthenticationRequired
--        Just "admin" -> Authorized
--        Just _ -> Unauthorized "Soh o admin acessa aqui!"

isUser = do
    mu <- lookupSession "_USER"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")
