{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql




formDepto :: Form Departamento
formDepto = renderDivs $ Departamento <$>
            areq textField "Nome" Nothing <*>
            areq textField FieldSettings{fsId=Just "hident2",
                           fsLabel="Sigla",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("maxlength","3")]} Nothing

formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Nome" Nothing <*>
             areq intField "Idade" Nothing <*>
             areq doubleField "Salario" Nothing <*>
             areq (selectField dptos) "Depto" Nothing
             
formPedido :: Form Pedido
formPedido = renderDivs $ Pedido <$>
              areq (selectField produtos) "ProdutoNome" Nothing <*>
              areq (selectField usuarios) "UsuarioNome" Nothing <*>
              areq intField "Status" Nothing <*>
              areq intField "Quantidade" Nothing
{-              
formPedidoProduto :: ProdutoId -> Produto -> UsuarioId -> Form Pedido
formPedidoProduto pid produto uid = renderDivs $ Pedido <$>
    areq intField "produtoId" (Just (fromIntegral (fromSqlKey pid) ) )<*>
    pure uid  <*>
    pure 0 <*>
    areq intField "quantidade" Nothing
  -}  

produtos = do
       entidades <- runDB $ selectList [] [Asc ProdutoNome] 
       optionsPairs $ fmap (\ent -> (produtoNome $ entityVal ent, entityKey ent)) entidades

usuarios = do
       entidades <- runDB $ selectList [] [Asc UsuarioNome] 
       optionsPairs $ fmap (\ent -> (usuarioNome $ entityVal ent, entityKey ent)) entidades
{-       
getPedidoR :: Handler Html
getPedidoR  = do
            (widget, enctype) <- generateFormPost formPedido
            defaultLayout $ do
            setTitle "Pedido"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_style_css
            addScript $ StaticR js_bootstrap_js
            [whamlet|
                <div ."container">
                    <header #"header">
                        <div ."row">
                            <div ."col-md-4 col-sm-12">
                                <h1 ."logo">
                                    <i ."glyphicon glyphicon-tag">My
                                    <span ."primary">
                                        Store
                                <p ."subtext">Bootstrap Ecommerce Template
                            <div ."col-md-8 col-sm-12">
                                <form ."form-inline">
                                    <a href=@{LoginR} title="" ."btn btn-default">Login
                                    <a href="#" title="" ."btn btn-default">Logout
                    <nav ."navbar navbar-default">
                        <div ."container-fluid">
                            <div ."navbar-header">
                                <button type="button" ."navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                                    <span ."sr-only">Toggle navigation
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                                    <span ."icon-bar">
                            <div #"navbar" ."navbar-collapse collapse">
                                <ul ."nav navbar-nav">
                                  <li ."active">
                                    <a href="#">Home
                                  <li>
                                    <a href="#">Contato
                                  <li>
                                    <a href="#">Minha conta
                    <section>
            
                    <div ."row">
                        <div ."col-md-8">
                            <div ."row">
                                <div ."col-md-4">
                                
                                    <img src=@{StaticR img_s5_png} ."main-img">
                                <div ."col-md-8">
                                    <h2>Samsung Galaxy S5
                                    <div ."price">
                                    <hr>
                                    <p>Quantity:
                                        <select name="" ."form-control">
                                            <option value="">1
                                            <option value="">2
                                            <option value="">3
                                            <option value="">4
                                            <option value="">5
                                        ^{widgetForm PedidoR enctype widget "Pedido"}
                                    <a href="#" title="" ."btn btn-success">Buy Now
                                    <br><br>
                                    <div ."panel-group" #"accordion" role="tablist" aria-multiselectable="true">
                                        <div ."panel panel-default">
                                            <div ."panel-heading" role="tab" #"headingOne">
                                                <h4 ."panel-title">
                                                    <a role="button" data-toggle="collapse" data-parent="#accordion" href="#collapseOne" aria-expanded="true" aria-controls="collapseOne">
                                                        Description
                                            <div #"collapseOne" ."panel-collapse collapse in" role="tabpanel" aria-labelledby="headingOne">
                                                <div ."panel-body">
                                                    Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
                <footer>
                    <p>MyStore Copyright &copy; 2015 <a href="#" title="">Terms &middot; <a href="#" title="">Privacy
                
            |]
            
-}
            --widgetForm PedidoR enctype widget "Pedido"
        
postPedidoR :: Handler Html
postPedidoR = do
            ((result, _), _) <- runFormPost formPedido
            case result of
                FormSuccess pedido -> do
                    runDB $ insert pedido
                    defaultLayout [whamlet|
                        <h1> Produto inserido no carrinho de compras.
                    |]
                _ -> redirect IndexR
              

dptos = do
       entidades <- runDB $ selectList [] [Asc DepartamentoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades

getHelloR :: Handler Html
getHelloR = defaultLayout [whamlet|
     <h1> _{MsgHello}
|]

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ do 
                 addStylesheet $ StaticR teste_css
                 widgetForm CadastroR enctype widget "Pessoas"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid 
             dpto <- runDB $ get404 (pessoaDeptoid pessoa)
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaNome pessoa}
                 <p> Salario: #{pessoaSalario pessoa}
                 <p> Idade: #{pessoaIdade pessoa}
                 <p> Departamento: #{departamentoNome dpto}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaNome]
             defaultLayout $ do 
             [whamlet|
                 <h1> Pessoas cadastradas:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaNome pessoa} 
                     <form method=post action=@{PessoaR pid}> 
                         <input type="submit" value="Deletar"><br>
             |] 
             toWidget [lucius|
                form  { display:inline; }
                input { background-color: #ecc; border:0;}
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaNome pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR

getDeptoR :: Handler Html
getDeptoR = do
             (widget, enctype) <- generateFormPost formDepto
             defaultLayout $ widgetForm DeptoR enctype widget "Departamentos"

postDeptoR :: Handler Html
postDeptoR = do
                ((result, _), _) <- runFormPost formDepto
                case result of
                    FormSuccess depto -> do
                       runDB $ insert depto
                       defaultLayout [whamlet|
                           <h1> #{departamentoNome depto} Inserido com sucesso. 
                       |]
                    _ -> redirect DeptoR

postPessoaR :: PessoaId -> Handler Html
postPessoaR pid = do
     runDB $ delete pid
     redirect ListarR

formContato :: Form Contato
formContato = renderDivs $ Contato <$>
             areq textField "Nome" Nothing <*>
             areq emailField "E-mail" Nothing <*>
             areq textField "Assunto" Nothing <*>
             areq textareaField "Mensagem" Nothing 

getContatoR :: Handler Html
getContatoR =  do
            (widget, enctype) <- generateFormPost formContato
            defaultLayout $ do 
               setTitle "Contato"
               addStylesheet $ StaticR css_bootstrap_css
               addStylesheet $ StaticR css_style_css
               addScript $ StaticR js_bootstrap_js
               [whamlet|
                   <div ."section">
                       <header ."container">
                           <div ."row">
                               <div ."col-md-6">
                                   ^{widgetForm ContatoR enctype widget "Contato"}
               |]
               --widgetForm ContatoR enctype widget "Contato"
                
postContatoR :: Handler Html
postContatoR = do
                ((result, _), _) <- runFormPost formContato
                case result of
                    FormSuccess contato -> do
                       runDB $ insert contato 
                       defaultLayout [whamlet| 
                           <h1> #{contatoNome contato} seu(sua) #{contatoAssunto contato} foi enviado aguarde nossa resposta. 
                       |]
                    _ -> redirect ContatoR
                    
getListaPR :: Handler Html
getListaPR = undefined
{-
do
            listaP <- runDB $ selectList [] [Asc ProdutoNome]
            defaultLayout $ do 
                [whamlet|
                    <h1> Produtos cadastradas:
                    <form>
                        {-$forall Entity pid produto <- listaP
                            Produto: <input value="#{produtoNome produto}"><br>
                            Preço<input value="#{produtoPreco produto}"><br>-}
                |] 
  -}      
getSessaoR :: Handler Html
getSessaoR = do
    sess <- getSession
    defaultLayout $ do
        [whamlet|
                <h1>#{show sess}
        |]
        
                
getListaPedidoR :: Handler Html
getListaPedidoR  = do
         listaP <- runDB $ selectList [] [Asc PedidoId]
         defaultLayout $ do 
         [whamlet|
             <h1> Pedidos cadastrados:
             $forall Entity pid pedido <- listaP
                 #{pedidoStatus pedido} 
         |] 
         toWidget [lucius|
            form  { display:inline; }
            input { background-color: #ecc; border:0;}
         |]

  
getPedidoR ::  ProdutoId -> Handler Html
getPedidoR pid = do
            produto <- runDB $ get404 pid 
            categoria <- runDB $ get404 (produtoCatid produto)
            defaultLayout $ do 
            setTitle "Produto"
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_style_css
            addScript $ StaticR js_jquery_js
            addScript $ StaticR js_bootstrap_js
            [whamlet|
            <div ."container">
                    <header #"header">
                    <div ."row">
                        <div ."col-md-4 col-sm-12">
                            <h1 ."logo">
                                <i ."glyphicon glyphicon-tag">My
                                <span ."primary">
                                    Store
                            <p ."subtext">Bootstrap Ecommerce Template
                        <div ."col-md-8 col-sm-12">
                <nav ."navbar navbar-default">
                    <div ."container-fluid">
                        <div ."navbar-header">
                            <button type="button" ."navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                                <span ."sr-only">Toggle navigation
                                <span ."icon-bar">
                                <span ."icon-bar">
                                <span ."icon-bar">
                        <div #"navbar" ."navbar-collapse collapse">
                            <ul ."nav navbar-nav">
                              <li ."active">
                                 <a href=@{IndexR}>Home
                              <li>
                                <a href="/contato">Contato
                <section>
                <div ."row">
                    <div ."col-md-8">
                        <div ."row">
                            <div ."col-md-8">
                                <h2>#{produtoNome produto}
                                <div ."price">R$#{produtoPreco produto}
                                <hr>
                                <form action=@{PedidoCadastrarR} method=post >
                                    <input type=hidden name=produtoid value=#{show $ fromSqlKey pid}>
                                    <input type=hidden name=usuarioid value=1>
                                    <input type=hidden name=status value=0>
                                    <input type=number name=quantidade value=1>
                                    <input type=submit value="cadastrar pedido">
            <footer>
                <p>MyStore Copyright &copy; 2015 <a href="#" title="">Terms &middot; <a href="#" title="">Privacy

               
            |]


postPedidoCadastrarR :: Handler ()
postPedidoCadastrarR = do
    pedidoCadastro <- runInputPost $ Pedido 
                            <$> ( fmap toSqlKey $ (ireq intField "produtoid") )
                            <*> (fmap toSqlKey $ (ireq intField "usuarioid" ) )
                            <*> ireq intField "status"
                            <*> ireq intField  "quantidade"
    runDB $ insert pedidoCadastro
    setMessage "PRODUTO INSERIDO COM SUCESSO"
    redirect IndexR
    
   