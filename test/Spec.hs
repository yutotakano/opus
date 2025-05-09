module Main(main) where

import           Codec.Audio.Opus.Encoder
import           Codec.Audio.Opus.Decoder
import           Lens.Micro
import           Control.Exception
import           Control.Monad (guard, forM_)
import qualified Data.ByteString          as B
import           Data.Bits
import           Data.List
import           Data.Word (Word8)
import           System.Directory (doesDirectoryExist)
import           Test.Hspec
import qualified OpusCompare as Opus

cfgs :: [EncoderConfig]
cfgs = [mkEncoderConfig sr s c | sr <- srs, s <- ss, c <- cs]
  where
    srs = [opusSR48k, opusSR24k, opusSR16k, opusSR12k, opusSR8k ]
    ss = [True, False]
    cs = [app_voip, app_audio, app_lowdelay]

seqWithCfgs :: Monad m => (EncoderConfig -> m a) -> m ()
seqWithCfgs a = sequence_ (a <$> cfgs)

testEncoderCreate :: HasEncoderConfig cfg => cfg -> SpecWith ()
testEncoderCreate cfg =
  let n = mconcat [ "create valid ", show $ cfg ^. encoderConfig, " encoder"]
  in it n $
     opusEncoderCreate cfg >>= (`shouldSatisfy` const True)


onlyIfTestVectorsExist :: IO () -> IO ()
onlyIfTestVectorsExist action = do
  exists <- doesDirectoryExist "opus_newvectors"
  if exists then action else fail "opus_newvectors directory not found"

decodeFile :: DecoderConfig -> B.ByteString -> IO B.ByteString
decodeFile decoderCfg bytes = do
  decoder <- opusDecoderCreate decoderCfg
  loop decoder bytes
  where

    -- | Convert four unsigned bytes to a 32-bit integer. fromIntegral is
    -- applied to each byte before shifting to not lose any bits.
    charToInt :: [Word8] -> Int
    charToInt (b1:b2:b3:b4:[]) = (fromIntegral b1) `shiftL` 24 .|. (fromIntegral b2) `shiftL` 16 .|. (fromIntegral b3) `shiftL` 8 .|. (fromIntegral b4)
    charToInt _ = error "wrong length to convert to int"

    maxPacket, maxFrameSize :: Int
    maxPacket = 1500
    maxFrameSize = 48000 * 2

    -- | A simplified port of the official opus_demo.c file's decoding loop
    loop decoder bytes
      | B.length bytes < 8 = pure mempty
      | otherwise = do
        -- lines 649 to 672 in opus_demo.c
        let inputLen = charToInt $ B.unpack $ B.take 4 bytes
        guard $ inputLen <= maxPacket && inputLen >= 0 -- invalid payload length

        let inputEncFinalRange = charToInt $ B.unpack $ B.take 4 $ B.drop 4 bytes
        let (inputData, remaining) = B.splitAt inputLen $ B.drop 8 bytes
        guard $ inputLen == B.length inputData -- ran out of input, expecting inputLen but got B.length bytes

        guard $ inputLen /= 0 -- lost packets are not supported for now in this test

        -- line 783
        let outputSamples = maxFrameSize
        decoded <- opusDecode decoder (mkDecoderStreamConfig decoderCfg outputSamples 0) inputData
        -- recursively continue with the rest
        (decoded <>) <$> loop decoder remaining

main :: IO ()
main = hspec $ do
  describe "opusEncoderCreate" $
    seqWithCfgs testEncoderCreate
  around_ onlyIfTestVectorsExist $ do
    -- These tests require the opus test vectors, downloaded from the official
    -- opus website.
    describe "opus mono test vectors" $
      forM_ ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"] $ \file -> do
        it ("mono testvector " <> file) $ do
          let decoderCfg = mkDecoderConfig opusSR48k False
          B.readFile ("opus_newvectors/testvector" <> file <> ".bit") >>= decodeFile decoderCfg >>= B.writeFile "tmp.out"
          Opus.compareFiles Opus.Mono opusSR48k ("opus_newvectors/testvector" <> file <> "m.dec") "tmp.out" >>= shouldBe True

    describe "opus stereo test vectors" $
      forM_ ["01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"] $ \file -> do
        it ("stereo testvector " <> file) $ do
          let decoderCfg = mkDecoderConfig opusSR48k True
          B.readFile ("opus_newvectors/testvector" <> file <> ".bit") >>= decodeFile decoderCfg >>= B.writeFile "tmp.out"
          Opus.compareFiles Opus.Stereo opusSR48k ("opus_newvectors/testvector" <> file <> ".dec") "tmp.out" >>= shouldBe True


{-

-}
{-
  let testEncoderEncodeWith = testEncoderEncode <$> srs <*> ss <*> cs
  describe "opusEncode" $ do
r <-
      sequence_ $ testEncoderEncodeWith <*> pure "empty input" <*> pure mempty >>= (`shouldSatisfy` const True)
    return ()
-}
