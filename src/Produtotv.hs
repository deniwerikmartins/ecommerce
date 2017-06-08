{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Produtotv where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text

import Database.Persist.Postgresql

getProdutotvR :: Handler Html
getProdutotvR = defaultLayout $ do
    setTitle "Smart Tv"
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
                            <a href="#">Contato
                          <li>
                            <a href="#">Minha conta
            <section>
            <div ."row">
                <div ."col-md-8">
                    <div ."row">
                        <div ."col-md-4">
                            <img src=@{StaticR img_tv_png} ."main-img">
                        <div ."col-md-8">
                            <h2>Smart Tv
                            <div ."price">R$1.673,07
                            <hr>
                            <p>Quantity:
                                <select name="" ."form-control">
                                    <option value="">1
                                    <option value="">2
                                    <option value="">3
                                    <option value="">4
                                    <option value="">5
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
                
    

