{-# LANGUAGE TemplateHaskell #-}

module Codec.Audio.Opus.Types
 ( -- * Sampling Rate
   SamplingRate, HasSamplingRate(..)
 , opusSR8k, opusSR12k, opusSR16k, opusSR24k, opusSR48k
   -- * Coding Mode
 , CodingMode, HasCodingMode(..), app_voip, app_audio, app_lowdelay
   -- * Exception
 , OpusException(..), ErrorCode, _ErrorCodeException
   -- * EncoderConfig
 , FrameSize
 , EncoderConfig, HasEncoderConfig(..), mkEncoderConfig
   -- * DecoderConfig
 , DecoderConfig, HasDecoderConfig(..), mkDecoderConfig
   -- * StreamConfig
 , StreamConfig, HasStreamConfig(..), mkStreamConfig
   -- * DecoderStreamConfig
 , DecoderStreamConfig, HasDecoderStreamConfig(..), mkDecoderStreamConfig
 ) where

import           Codec.Audio.Opus.Internal.Opus

import           Lens.Micro
import           Lens.Micro.TH
import           Control.Monad.Catch
import           Data.Typeable                  (Typeable)


data OpusException
  = OpusBadArg
  | OpusBufferToSmall
  | OpusInternalError
  | OpusInvalidPacket
  | OpusUnimplemented
  | OpusInvalidState
  | OpusAllocFail
  deriving (Eq, Show, Typeable)

instance Exception OpusException

_ErrorCodeException :: Traversal' ErrorCode OpusException
_ErrorCodeException f e
  | Just exc <- errorCodeException e = errorCodeException' <$> f exc
  | otherwise = pure e

errorCodeException' :: OpusException -> ErrorCode
errorCodeException' OpusBadArg        = opus_bad_arg
errorCodeException' OpusBufferToSmall = opus_buffer_too_small
errorCodeException' OpusInternalError = opus_internal_error
errorCodeException' OpusInvalidPacket = opus_invalid_packet
errorCodeException' OpusUnimplemented = opus_unimplemented
errorCodeException' OpusInvalidState  = opus_invalid_state
errorCodeException' OpusAllocFail     = opus_alloc_fail


errorCodeException :: ErrorCode -> Maybe OpusException
errorCodeException a
  | a == opus_bad_arg = Just OpusBadArg
  | a == opus_buffer_too_small = Just OpusBufferToSmall
  | a == opus_internal_error = Just OpusInternalError
  | a == opus_invalid_packet = Just OpusInvalidPacket
  | a == opus_unimplemented = Just OpusUnimplemented
  | a == opus_invalid_state = Just OpusInvalidState
  | a == opus_alloc_fail = Just OpusAllocFail
  | otherwise = Nothing


makeClassy ''SamplingRate
makeClassy ''CodingMode

data EncoderConfig = EncoderConfig
  { _encoderSamplingRate :: SamplingRate  -- ^ sampling rate of input signal
  , _encoderIsStereo     :: Bool -- ^ stereo mode? ('True' => 2 channels, 'False' => 1 channel)
  , _encoderCodingMode   :: CodingMode    -- ^ Coding mode. (See 'app_voip', 'app_audio', 'app_lowdelay')
  } deriving (Eq, Show)

makeClassy 'EncoderConfig

mkEncoderConfig :: SamplingRate -> Bool -> CodingMode -> EncoderConfig
mkEncoderConfig = EncoderConfig

instance HasSamplingRate EncoderConfig where
  samplingRate = encoderSamplingRate

instance HasCodingMode EncoderConfig where
  codingMode = encoderCodingMode

data DecoderConfig = DecoderConfig
  { _decoderSamplingRate :: SamplingRate
  , _decoderIsStereo     :: Bool
  } deriving (Eq, Show)

makeClassy 'DecoderConfig

mkDecoderConfig :: SamplingRate -> Bool -> DecoderConfig
mkDecoderConfig = DecoderConfig

instance HasSamplingRate DecoderConfig where
  samplingRate = decoderSamplingRate

type FrameSize = Int


data StreamConfig = StreamConfig
  { _streamEncoderConfig :: EncoderConfig
  , _streamFrameSize     :: FrameSize
  , _streamOutSize       :: Int
  } deriving (Eq, Show)

makeClassy ''StreamConfig

mkStreamConfig :: EncoderConfig -> FrameSize -> Int -> StreamConfig
mkStreamConfig = StreamConfig

instance HasEncoderConfig StreamConfig where
  encoderConfig = streamEncoderConfig

instance HasSamplingRate StreamConfig where
  samplingRate = encoderConfig . samplingRate

instance HasCodingMode StreamConfig where
  codingMode = encoderConfig . codingMode

data DecoderStreamConfig = DecoderStreamConfig
  { _deStreamDecoderConfig :: DecoderConfig
  , _deStreamFrameSize     :: FrameSize
  , _deStreamDecodeFec     :: Int
  } deriving (Eq, Show)

makeClassy ''DecoderStreamConfig

mkDecoderStreamConfig :: DecoderConfig -> FrameSize -> Int -> DecoderStreamConfig
mkDecoderStreamConfig = DecoderStreamConfig

instance HasDecoderConfig DecoderStreamConfig where
    decoderConfig = deStreamDecoderConfig

instance HasSamplingRate DecoderStreamConfig where
    samplingRate = decoderConfig . samplingRate

