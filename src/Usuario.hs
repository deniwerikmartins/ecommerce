{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Usuario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
             areq textField "Nome" Nothing <*>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing 

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing 

getUsuarioR :: Handler Html
getUsuarioR = do
            (widget, enctype) <- generateFormPost formUsu
            defaultLayout $ do
            setTitle "Cadastro de Usuário"
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
                                    <div ."form-group">
                                        <label>Email
                                        <input type="email" ."form-control" placeholder="Enter Email...">
                                    <div ."form-group">
                                        <label>Password
                                        <input type="password" ."form-control" placeholder="Enter Password...">
                                    <button type="submit" ."btn btn-default">Login
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
                            <div ."col-md-12">
                                ^{widgetForm UsuarioR enctype widget "Cadastro de Usuários"}
                <footer>
                    <p>MyStore Copyright &copy; 2015 <a href="#" title="">Terms &middot; <a href="#" title="">Privacy
            |]
            

-- para contrar duplicagem de email, procurar getBy
postUsuarioR :: Handler Html
postUsuarioR = do
                ((result, _), _) <- runFormPost formUsu
                case result of
                    FormSuccess usu -> do
                       runDB $ insert usu
                       defaultLayout [whamlet|
                           <h1> #{usuarioNome usu} Inserido com sucesso. 
                       |]
                    _ -> redirect UsuarioR
                    
getLoginR :: Handler Html
getLoginR = do
            (widget, enctype) <- generateFormPost formLogin
            defaultLayout $ widgetForm LoginR enctype widget "Login page"


postLoginR :: Handler Html
postLoginR = do
                ((result, _), _) <- runFormPost formLogin
                case result of
                    FormSuccess (email,senha) -> do
                       temUsu <- runDB $ selectFirst [UsuarioEmail ==. email,UsuarioSenha ==. senha] []
                       case temUsu of
                           Nothing -> redirect LoginR
                           Just _ -> do
                               setSession "_USER" email
                               redirect IndexR
                              -- defaultLayout [whamlet| Usuário autenticado!|]
                    _ -> redirect IndexR

getLogoutR :: Handler Html
getLogoutR = do
    deleteSession "_USER"
    redirect LoginR

