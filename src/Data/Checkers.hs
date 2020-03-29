{-# LANGUAGE OverloadedStrings #-}
module Data.Checkers
    (
    checkInt
    ,checkIntList
    ,checkFloat
    ,checkRational
    ,checkOctet
    ,checkFractional
    ,checkSampleRate
    )
where

import Control.Monad (void)

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Number (int)
import Prelude as P
import Data.Ratio
import Data.Text as T

import Numeric

import Data.ByteString.Lazy as B (ByteString, pack)




checkInt :: Text -> Integer -> Integer -> Maybe Text -> Either Text Integer
checkInt str low high ref =
    case parse number "" (T.unpack str) of
        Left err -> Left (T.pack (show err))
        Right x' -> proc x'
            where
                which = maybe "" (\x -> ": " `T.append` x) ref
                proc x
                    | x < low = Left $ which `T.append` " value must be >= " `T.append` (T.pack (show low))
                    | x > high = Left $ which `T.append` " value must be <= " `T.append` (T.pack (show high))
                    | otherwise = Right x

checkSampleRate :: Text -> Either Text Int
checkSampleRate str =
    case parse sampleRate "" (T.unpack str) of
        Left err -> Left ("Illegal sample rate: " <> T.pack (show err))
        Right x -> Right x
                    

checkIntList :: Text -> Integer -> Integer -> Maybe Text -> Either Text [Integer]
checkIntList str low high ref =
    case parse (listOf number) "" (T.unpack str) of
        Left err -> Left (T.pack (show err))
        Right x' -> proc x'
            where
                which = maybe "" (\x -> ": " `T.append` x) ref
                proc x =
                    let vx = P.filter (< low) x
                        vy = P.filter (> high) x
                    in
                        if not (P.null vx) then Left $ which `T.append` " values too low: " `T.append` (T.pack (show vx)) else
                        if not (P.null vy) then Left $ which `T.append` " values too high: " `T.append` (T.pack (show vy)) else Right x




checkFloat :: (Fractional a, Read a) => Text -> Either Text a
checkFloat str =
    case reads (unpack str) of
        ((x, _) :_) -> Right x
        _ -> Left $ "Illegal floating point value: " `append` str


checkRational :: Text -> Either Text Rational
checkRational str =
    either (Left . T.pack . show) Right $ parse ratio "" (T.unpack str)

checkFractional :: Text -> Either Text (Either Double Rational)
checkFractional str =
    either (Left . T.pack . show) Right $ parse fractional "" (T.unpack str)



checkOctet :: Text -> Either Text ByteString
checkOctet str =
    either (Left . T.pack . show) (Right . id) $ parse p "" str
    where
        p = do
            ls <- many hexDigit
            return $! B.pack $ P.map (fromIntegral.fromHex) $ group2 ls

group2 :: [Char] -> [[Char]]
group2 [] = []
group2 (x:[]) = [[x,'0']]
group2 (x:y:xs) = [x,y] : group2 xs


fromHex :: String -> Integer
fromHex = fst . P.head . readHex


number :: Parsec String u Integer
number = do
    let lexer = makeTokenParser haskellStyle
    integer lexer

sampleRate :: Parsec String u Int 
sampleRate = do 
    pre <- int 

    post <- option 0 decim
    let 
        sr = if post /= 0 then pre * 1000 + post else pre
    return sr
    where 
        decim = do 
            void $ char '.'
            int



fl :: Parsec String u Double
fl = do
    let lexer = makeTokenParser haskellStyle
    r <- optionMaybe $ char '-'
    f <- float lexer
    case r of
        Just _ -> return $ negate f
        Nothing -> return f

ratio :: Parsec String u Rational
ratio = do
    let lexer = makeTokenParser haskellStyle
    r <- optionMaybe $ char '-'
    n <- integer lexer
    spaces >> char '%' >> spaces
    d <- integer lexer
    let val = n % d
    case r of
        Just _ -> return $ negate val
        Nothing -> return val

fractional :: Parsec String u (Either Double Rational)
fractional = do
    (try fl >>= \f -> return (Left f))
    <|>
    (ratio >>= \r -> return (Right r))

listOf :: Parsec String u a -> Parsec String u [a]
listOf p = sepBy p listSep

listSep :: Parsec String u ()
listSep = do
    spaces
    void $ char ','
    spaces

