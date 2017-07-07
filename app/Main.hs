{-# OPTIONS_GHC -ddump-simpl -dsuppress-module-prefixes -dsuppress-uniques -Wno-unused-imports #-}

{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

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

import           Lib                          (ordP)

type StreamOf a m = Stream (Of a) m ()

data Nucleotide = A | C | T | G deriving (Enum)

{-# NOINLINE decodeNucleotide #-}
decodeNucleotide :: Word8 -> Maybe Nucleotide
decodeNucleotide = \case
    [ordP|A|] -> Just A
    [ordP|C|] -> Just C
    [ordP|G|] -> Just G
    [ordP|T|] -> Just T
    _   -> Nothing

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
    go (n1 : rest@(n2 : n3 : n4 : n5 : n6 : _)) nucs = do
        Streaming.yield [n1, n2, n3, n4, n5, n6]
        go rest nucs
    go buf nucs =
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
