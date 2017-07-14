{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (decodeWord, main) where

import           Control.Monad.Extra          (whenJustM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Streaming    as BS
import           Data.Vector.Unboxed          (Vector)
import qualified Data.Vector.Unboxed          as Vector (freeze)
import qualified Data.Vector.Unboxed.Mutable  as Vector
import           Data.Word                    (Word16, Word8)
import           Streaming                    (Of, Stream)
import qualified Streaming.Prelude            as Streaming

type StreamOf a m = Stream (Of a) m ()

data Nucleotide = A | C | T | G deriving (Enum)

pattern LatinCapitalLetterA :: (Num a, Eq a) => a
pattern LatinCapitalLetterA = 0x41

pattern LatinCapitalLetterC :: (Num a, Eq a) => a
pattern LatinCapitalLetterC = 0x43

pattern LatinCapitalLetterG :: (Num a, Eq a) => a
pattern LatinCapitalLetterG = 0x47

pattern LatinCapitalLetterT :: (Num a, Eq a) => a
pattern LatinCapitalLetterT = 0x54

decodeNucleotide :: Word8 -> Maybe Nucleotide
decodeNucleotide = \case
    LatinCapitalLetterA -> Just A
    LatinCapitalLetterC -> Just C
    LatinCapitalLetterG -> Just G
    LatinCapitalLetterT -> Just T
    _                   -> Nothing

type GWord = [Nucleotide]

type Stat = Vector Int

encodeWord :: GWord -> Word16
encodeWord gWord =
    fromIntegral $
    sum [4 ^ i * fromEnum nuc | (i, nuc) <- zip [0 :: Int ..] gWord]

decodeWord :: Word16 -> GWord
decodeWord word =
    [toEnum . fromIntegral $ word `div` 4 ^ i `mod` 4 | i <- [0 .. 5 :: Int]]

countWords :: MonadIO m => StreamOf Word8 m -> m Stat
countWords = buildStat . collectGWords . Streaming.mapMaybe decodeNucleotide

collectGWords :: Monad m => StreamOf Nucleotide m -> StreamOf GWord m
collectGWords = go []
  where
    go buf nucs = case buf of
        n1 : rest@(n2 : n3 : n4 : n5 : n6 : _) -> do
            Streaming.yield [n1, n2, n3, n4, n5, n6]
            go rest nucs
        _ ->
            whenJustM (lift $ Streaming.uncons nucs) $ \(nuc, nucs') ->
                go (buf ++ [nuc]) nucs'

buildStat :: MonadIO m => StreamOf GWord m -> m Stat
buildStat = Streaming.foldM_ step initialize extract
  where
    initialize = liftIO $ Vector.new (4 ^ (6 :: Int))
    step stat gword = do
        liftIO . Vector.modify stat succ . fromIntegral $ encodeWord gword
        pure stat
    extract = liftIO . Vector.freeze

main :: IO ()
main = do
    let bytes = BS.unpack $ BS.readFile "genome.fna"
    stat <- runResourceT $ countWords bytes
    print stat
