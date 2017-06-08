{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Index where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Handlers
import Database.Persist.Postgresql

getIndexR :: Handler Html
getIndexR = defaultLayout $ do
        setTitle "Ecommerce"
        addStylesheet $ StaticR css_bootstrap_css
        addStylesheet $ StaticR css_style_css
        addStylesheetRemote "https://fonts.googleapis.com/css?family=Oswald:700"
        addScript $ StaticR js_bootstrap_js
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.min.js"
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
                                <a href="/contato">Contato
                              <li>
                                <a href=@{UsuarioR}>Cadastro
                              <li>
                                <a href=@{ListaPR}>Carrinho
                <section #"products">
                    <div ."row">
                        <div ."col-md-4">
                            <div ."thumbnail">
                              <img src=@{StaticR img_s5_png}>
                              <div ."caption">
                                <h4 ."pull-right">
                                  R$1.847,00
                                <h4>
                                    --<a href=@{Produtos5R}>
                                    <a href=@{PedidoR}>
                                        Samsung Galaxy S5
                                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec dignissim
                                <div ."ratings">
                                  <p ."pull-right">15 Reviews
                                  <p>
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                        <div ."col-md-4">
                            <div ."thumbnail">
                              <img src=@{StaticR img_xbox_png}>
                              <div ."caption">
                                <h4 ."pull-right">
                                  R$1.234,05
                                <h4>
                                    <a href=@{ProdutoxboxR}>
                                        Xbox One
                                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec dignissim
                                <div ."ratings">
                                  <p ."pull-right">8 Reviews
                                  <p>
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star-empty">
                        <div ."col-md-4">
                            <div ."thumbnail">
                              <img src=@{StaticR img_ps4_png}>
                              <div ."caption">
                                <h4 ."pull-right">
                                  R$4.000,00
                                <h4>
                                    <a href=@{Produtops4R}>
                                        Playstation 4
                                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec dignissim
                                <div ."ratings">
                                  <p ."pull-right">4 Reviews
                                  <p>
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                    <div ."row">
                        <div ."col-md-4">
                            <div ."thumbnail">
                              <img src=@{StaticR img_surface_png}>
                              <div ."caption">
                                <h4 ."pull-right">
                                  R$2.628,71
                                <h4>
                                    <a href=@{ProdutosurfaceR}>
                                        Microsoft Surface
                                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec dignissim
                                <div ."ratings">
                                  <p ."pull-right">15 Reviews
                                  <p>
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                                    <span ."glyphicon glyphicon-star">
                        <div ."col-md-4">
                            <div ."thumbnail">
                              <img src=@{StaticR img_tv_png}>
                              <div ."caption">
                                <h4 ."pull-right">
                                  R$1.673,07
                                <h4>
                                    <a href=@{ProdutotvR}>
                                        Smart TV
                                <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec dignissim
                                <div ."ratings">
                                    <p ."pull-right">22 Reviews
                                    <p>
                                        <span ."glyphicon glyphicon-star">
                                        <span ."glyphicon glyphicon-star">
                                        <span ."glyphicon glyphicon-star">
                                        <span ."glyphicon glyphicon-star">
                                        <span ."glyphicon glyphicon-star-empty">
                        <div ."col-md-4">
                            <div ."thumbnail">
                                <img src=@{StaticR img_ipad_png}>
                                <div ."caption">
                                    <h4 ."pull-right">
                                        R$1.699,90
                                    <h4>
                                        <a href=@{ProdutoipadR}>
                                            Apple iPad
                                    <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec dignissim
                                    <div ."ratings">
                                        <p ."pull-right">14 Reviews
                                        <p>
                                            <span ."glyphicon glyphicon-star">
                                            <span ."glyphicon glyphicon-star">
                                            <span ."glyphicon glyphicon-star">
                                            <span ."glyphicon glyphicon-star">
                                            <span ."glyphicon glyphicon-star">
            <footer>
                <p>MyStore Copyright &copy; 2015 <a href="#" title="">Terms &middot; <a href="#" title="">Privacy
        |]