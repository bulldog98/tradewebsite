import Data.Pokemon
import Data.Html

import Control.Monad
import Options.Applicative

import System.IO

data Command = Gen
              | Add

parseCommand :: Parser Command
parseCommand = subparser $
    command "gen"   (parseGen   `withInfo` "Generate the html") <>
    command "add"  (parseAdd  `withInfo` "Add a new pokemon")

parseGen :: Parser Command
parseGen = pure Gen

parseAdd :: Parser Command
parseAdd = pure Add

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

addNumbering :: [(String,String)] -> Int -> [String]
addNumbering []     _   = []
addNumbering (x:xs) i   = (show i ++ ") " ++ snd x) :addNumbering xs (i+1)

askNumber :: IO Int
askNumber = do
    putStrLn "Where do you want to add a pokemon?"
    forM_ (addNumbering (tail pages) 1) putStrLn
    guess <- getLine
    if (read guess) < 1 || (read guess) >= length pages then do
        putStrLn "Number has to be in valid range"
        askNumber
    else
        return $ read guess

run :: Command -> IO ()
run Gen = do
    breedables <- readPokemonFile "breedables.json"
    writeHtmlToFile "breedables.html" $ breedableHtml breedables
    writeHtmlToFile "index.html" trainerCard
run Add = do
    hSetBuffering stdout NoBuffering
    i <- askNumber
    putStrLn $ "Adding to " ++ snd (pages !! i)
    poke <- readPokemonInput
    addPokemonToFile poke $ (fst (pages !! i)) ++ "json"

main :: IO ()
main = execParser opts >>= run
  where
    opts = parseCommand `withInfo` "interact with the trading website"
