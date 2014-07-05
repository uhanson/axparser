-- | Main entry point to the application.
--{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
    --import Text.Parsec
    --import Text.Parsec.String
    --import Text.Parsec.Token
    --import qualified  Data.Map as Map
    import Control.Monad
    import Text.ParserCombinators.Parsec
    import Control.Applicative hiding ( (<|>), many, optional )
    --import Data.List (intersperse)
    import GHCI

    data XPOElement = XPOForm String [XPOProperty] [XPOMethod]
                    | XPOTable String
                    | XPOClass String [XPOProperty] [XPOMethod]
                    | XPOProject String
        deriving (Show)

    data XPOFile     = XPOFile [XPOElement] deriving (Show)
    data XPOProperty = XPOProperty String String deriving (Show)
    data XPOMethod   = XPOMethod String [String]

    instance Show XPOMethod where
        show (XPOMethod name _) = "XPOMethod " ++ name ++ " <...>"

    anyAfter c = char c >> anyChar `manyTill` (char '\n')

    xpoSkip = do
        many $ (many1 $ oneOf " \t\n") <|> anyAfter ';'
        return ()

    xpoConst n = try $ do
        xpoSkip
        string n
        optional $ many1 (char ' ')
        option "" (anyAfter '#')

    xpoCode = try $ do
        xpoSkip
        anyAfter '#'

    xpoNamed = try $ do
        xpoSkip
        name <- many1 (noneOf " \n\t")
        many1 (char ' ')
        value <- anyAfter '#'
        return (name, value)

    xpoSectionV n p = do
        name <- xpoConst n
        value <- p
        xpoConst $ "END" ++ n
        return (name, value)

    xpoSection n p = between (xpoConst n) (xpoConst $ "END" ++ n) p
{-
    xpoSection n p = do
        xpoConst n
        values <- p
        xpoConst $ "END" ++ n
        return values
-}
    xpoSectionS n = do
        name <- xpoConst n
        anyChar `manyTill` (xpoConst $ "END" ++ n)
        return name

    xpoProperty = do
        (name, value) <- xpoNamed
        return $ XPOProperty name value

    xpoProperties = xpoSection "PROPERTIES" $ many xpoProperty

    xpoMethod = do
        (name, code) <- xpoSectionV "SOURCE" $ many xpoCode
        return $ XPOMethod name code

    xpoMethods = xpoSection "METHODS" $ do
                     optional $ xpoConst "Version: 3"
                     many $ try xpoMethod

    xpoClass = do
        xpoConst "***Element: CLS"
        xpoConst "CLSVERSION 1"
        (name, (ps, ms)) <- xpoSectionV "CLASS" $ do
            ps <- xpoProperties
            ms <- xpoMethods
            return (ps, ms)
        return $ XPOClass name ps ms

    xpoForm  = do
        xpoConst "***Element: FRM"
        xpoConst "FRMVERSION 5"
        (name, (ps, ms)) <- xpoSectionV "FORM" $ do
            ps <- xpoProperties
            ms <- xpoMethods
            xpoSectionS "OBJECTBANK"
            xpoSectionS "JOINS"
            xpoSectionS "DESIGN"
            return (ps, ms)
        return $ XPOForm name ps ms

    xpoTable = do
        xpoConst "***Element: DBT"
        xpoConst "TABLEVERSION 1"
        name <- xpoSectionS "TABLE"
        return $ XPOTable name

    xpoProject = do
        xpoConst "***Element: PRN"
        xpoConst "PROJECTVERSION 2"
        name <- xpoSectionS "PROJECT"
        return $ XPOProject name

    xpoElement = xpoClass <|> xpoForm <|> xpoTable <|> xpoProject
    
    xpoFile = do
        xpoConst "Exportfile for AOT version 1.0 or later"
        xpoConst "Formatversion: 1"
        elements <- xpoElement `manyTill` (try $ xpoConst "***Element: END")
        return $ XPOFile elements

    -- | The main entry point.
    main :: IO ()
    main = do
        raw <- readFile "SharedProject_TSM00760.xpo"
        parseTest xpoFile raw
