{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Data.Pokemon (
    Pokemon,
    readPokemonFile,
    readPokemonInput,
    showPokemonJSON,
    showPokemonHTML,
    addPokemonToFile
)
where

import Control.Monad
import Control.Exception
import Text.Printf

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes (class_,src)
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5.Attributes as A

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.Maybe (fromMaybe)
import Data.List (sort)

import System.Directory

import qualified Data.ByteString.Lazy.Char8 as BL


data Iv = IV Int | Undefined deriving (Ord, Eq)

instance Show Iv where
    show Undefined = "xx"
    show (IV n)    = printf "%02d" n

instance Bounded Iv where
  minBound = IV 0
  maxBound = IV 31

iv :: Int -> Iv
iv n = assert (n >= 0 && n <= 31) $ IV n

data MonNum = MN Int deriving (Ord,Eq)

instance Show MonNum where
    show (MN n) = printf "%03d" n

maxpokes = 706 -- This must be updated if new pokemon is released

instance Bounded MonNum where
    minBound = MN 0
    maxBound = MN maxpokes

monnum :: Int -> MonNum
monnum n = assert (n>=0 && n <= maxpokes) $ MN n

data Att = Att String deriving (Eq)

instance Show Att where
    show (Att s) = s

data Pokemon = Pokemon {
    shiny   :: Bool,
    ivs     :: (Iv,Iv,Iv,Iv,Iv,Iv),
    number  :: MonNum,
    name    :: String,
    nature  :: String,
    ability :: String,
    ball    :: String,
    atts    :: [Att]
} deriving (Eq, Show)

instance Ord Pokemon where
    compare p1 p2 = number p1 `compare` number p2

$(deriveJSON defaultOptions ''Iv)
$(deriveJSON defaultOptions ''Att)
$(deriveJSON defaultOptions ''MonNum)
$(deriveJSON defaultOptions ''Pokemon)

bb :: Pokemon
bb = Pokemon {
    shiny   = True,
    ivs     = (IV 0,Undefined,IV 31,IV 9,IV 31,IV 0),
    number  = MN 1,
    name    = "Balbasaur",
    nature  = "Adamant",
    ability = "Chlorophorm",
    ball    = "pokeball",
    atts    = [Att "Test"]
}

readPokemonFile :: FilePath -> IO [Pokemon]
readPokemonFile f = do
    file <- BL.readFile f
    let pokis = decode file :: Maybe [Pokemon]
    return $ fromMaybe [] pokis

doShinyQuestion :: IO Bool
doShinyQuestion = do
    putStr "Is the pokemon shiny? (y/n): "
    guess <- getLine
    if (guess `elem` ["y","yes","no","n"])
        then return $ guess `elem` ["y","yes"]
        else do
            putStrLn "Your format is wrong. Answer with y or n"
            doShinyQuestion

doIvQuestion :: IO Iv
doIvQuestion = do
    putStr "Enter the iv of the pokemon: "
    guess <- getLine
    if guess `elem` ["xx","x"] then
        return Undefined
    else if (read guess) < 0
        then do
            putStrLn "Too low!"
            doIvQuestion
        else if (read guess) > 31
            then do
                putStrLn "Too high!"
                doIvQuestion
            else return $ IV (read guess)

doNumberQuestion :: IO MonNum
doNumberQuestion = do
    putStr "Enter the number of the Pokemon: "
    guess <- getLine
    return $ monnum $ read guess

doStringQuestion :: String -> IO String
doStringQuestion s = do
    putStr $ " Enter the "++s++" of the Pokemon: "
    getLine

doIvs :: IO [Iv]
doIvs = replicateM 6 doIvQuestion

converts :: IO [String] -> IO [Att]
converts a = do
    vals <- a
    return $ Prelude.map Att vals

doAttsQuestion :: IO [Att]
doAttsQuestion = do
    putStr "How many eggmoves does the pokemon know?: "
    guess <- getLine
    if (read guess) < 0 then do
        putStrLn "A pokemon must at least know 0 eggmoves"
        doAttsQuestion
    else if (read guess) > 4 then do
        putStrLn "A pokemon must at most know 4 eggmoves"
        doAttsQuestion
    else
        converts $ replicateM (read guess) $ doStringQuestion "egg move"

convertIv :: [Iv] -> (Iv,Iv,Iv,Iv,Iv,Iv)
convertIv [i1,i2,i3,i4,i5,i6]   = (i1,i2,i3,i4,i5,i6)
convertIv _                     = error "not enought"

readPokemonInput :: IO Pokemon
readPokemonInput = do
    pShiny      <- doShinyQuestion
    pIvs        <- doIvs
    pNumber     <- doNumberQuestion
    pName       <- doStringQuestion "name"
    pNature     <- doStringQuestion "nature"
    pAbility    <- doStringQuestion "ability"
    pBall       <- doStringQuestion "ball"
    pAtts       <- doAttsQuestion
    return Pokemon {
        shiny   = pShiny,
        ivs     = convertIv pIvs,
        number  = pNumber,
        name    = pName,
        nature  = pNature,
        ability = pAbility,
        ball    = pBall,
        atts    = pAtts
    }

showPokemonJSON :: Pokemon -> IO ()
showPokemonJSON p = do
    BL.putStr $ encodePretty p

sprite :: Pokemon -> AttributeValue
sprite p = stringValue $ "http://www.serebii.net/" ++ (if shiny p then
            "Shiny/XY/" else "xy/pokemon/") ++ (show $ number p) ++ ".png"

showIvs :: Pokemon -> Html
showIvs = string . help . f . ivs
    where help :: [Iv] -> String
          help []           = ""
          help [x]          = show x
          help (x:xs)       = show x ++ "/" ++ help xs
          f (a,b,c,d,e,f)   = [a,b,c,d,e,f]

pokeballSprite :: Pokemon -> AttributeValue
pokeballSprite p = stringValue $ "http://www.greenchu.de/sprites/items/" ++ ball p ++ ".png"

fix :: [String] -> [String]
fix []          = ["-","-","-","-"]
fix [x]         = [x,"-","-","-"]
fix [x,y]       = [x,y,"-","-"]
fix [x,y,z]     = [x,y,z,"-"]
fix xs          = xs

showAtts :: Pokemon -> Html
showAtts p = do
    let a = map show $ atts p
    let b = map string $ fix a
    ul ! class_ "attack" $ do
        forM_ b li

showPokemonHTML :: Pokemon -> Html
showPokemonHTML poke = do
    H.div ! class_ "pokemon" $ do
        H.div ! class_ "species" $ img ! src (sprite poke)
        H.div ! class_ "centerbox" $ do
            h3 $ string $ "#" ++ show (number poke) ++ ": " ++ name poke
            H.div ! class_ "abilitynature" $ do
                p $ string $ ability poke
                p $ string $ nature  poke
            H.div ! class_ "bottomline" $ do
                H.div ! class_ "pokeball" $ do
                    img ! src (pokeballSprite poke)
                    img ! src "http://cdn.bulbagarden.net/upload/a/a5/Blue_pentagon.png"
                    img ! src "http://cdn.bulbagarden.net/upload/2/27/ShinyVIStar.png" ! class_ (stringValue $ (if (shiny poke) then "" else "no") ++ "shiny")
                H.div ! class_ "ivs" $ showIvs poke
        showAtts poke

pokemonsToFile :: [Pokemon] -> FilePath -> IO ()
pokemonsToFile xs f = do
    BL.writeFile f $ encodePretty $ sort xs

addPokemonToFile :: Pokemon -> FilePath -> IO ()
addPokemonToFile p f = do
    pokis <- readPokemonFile f
    pokemonsToFile (p:pokis) "tmp.json"
    removeFile f
    renameFile "tmp.json" f
