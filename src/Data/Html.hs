{-# LANGUAGE OverloadedStrings #-}

module Data.Html
( breedableHtml
, trainerCard
, writeHtmlToFile
, pages
) where

import Control.Monad (forM_)
import Data.Pokemon (Pokemon,showPokemonHTML)

import qualified Data.ByteString.Lazy.Char8 as BL

import Prelude hiding (head,div)

import Text.Blaze.Html5 as H hiding (map,menu)
import Text.Blaze.Html.Renderer.Pretty
--import Text.Blaze.Html.Renderer.String -- exchange for debug to pretty
import Text.Blaze.Html5.Attributes as A

menuItem :: (Bool, String, String) -> Html
menuItem (b, l, s) = do
    li $ do
        a ! href (stringValue $ l ++ "html") !? (b,class_ "active") $ string s

pages :: [(String,String)]
pages =
    [ ("index."     ,"Trainer Card")
    , ("breedables.","Breedables")
    -- ,("impressum.","Impressum")
    {-, ("shiny."     ,"Shinies")
    , ("comp."      ,"Comp")-}
    ]

activePages :: Int -> [(Bool,String,String)]
activePages i = do
    (s1,s2) <- pages
    return ((s1,s2)==pages !! i,s1,s2)

menu :: Int -> Html
menu i = do
    ul ! class_ "menu" $ do
        forM_ (activePages i) menuItem

breedableHtml :: [Pokemon] -> Html
breedableHtml xs = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "utf-8"
        H.title $ string "breedable Pokémon"
        link ! rel "stylesheet" ! href "style.css"
    body $ do
        menu 1
        div ! class_ "pokemons" $ do
            forM_ xs showPokemonHTML

trainerCard :: Html
trainerCard = docTypeHtml ! lang "en" $ do
    head $ do
        meta ! charset "utf-8"
        H.title $ string "Trainer Card"
        link ! rel "stylesheet" ! href "style.css"
    body $ do
        menu 0
        div ! class_ "trainer" $ do
            h3 $ string "Trainer Card of /u/user"
            p $ string "This is my Spreadsheet. Of cause I trade breedables"
            table ! class_ "center" $ do
                tr $ do
                    th $ string "Game"
                    th $ string "IGN"
                    th $ string "TID"
                    th $ string "TSV"
                    th $ string "FC"
                tr $ do
                    td $ string "GAME"
                    td $ string "IGN"
                    td $ string "TID"
                    td $ string "TSV"
                    td $ string "FC"
            h4 $ string "Terms"
            p $ string "A o denotes that there is proof available, a x denotes that there is non available"

writeHtmlToFile :: FilePath -> Html -> IO ()
writeHtmlToFile f h = do
    writeFile f $ renderHtml h
