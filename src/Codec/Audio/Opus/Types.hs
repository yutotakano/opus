{-# LANGUAGE TemplateHaskell #-}
-- | This module contains the types used in the higher-level API of this library.
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


-- | A potential error that can happen during encoding or decoding, as reported
-- by the Opus library. The descriptions of each error have been taken from the
-- [Opus documentation](https://opus-codec.org/docs/opus_api-1.5/group__opus__errorcodes.html).
data OpusException
  = OpusBadArg
  -- ^ One or more invalid/out of range arguments.
  | OpusBufferToSmall
  -- ^ Not enough bytes allocated in the buffer.
  | OpusInternalError
  -- ^ An internal error was detected.
  | OpusInvalidPacket
  -- ^ The compressed data passed is corrupted.
  | OpusUnimplemented
  -- ^ Invalid/unsupported request number.
  | OpusInvalidState
  -- ^ An encoder or decoder structure is invalid or already freed.
  | OpusAllocFail
  -- ^ Memory allocation has failed.
  deriving (Eq, Show, Typeable)

instance Exception OpusException

-- | A 'Traversal' that maps a function @f@ onto the contained 'OpusException'
-- if the input 'ErrorCode' can be converted into it.
_ErrorCodeException :: Traversal' ErrorCode OpusException
_ErrorCodeException f e
  | Just exc <- errorCodeException e = errorCodeException' <$> f exc
  | otherwise = pure e

-- | Convert an 'OpusException' into an 'ErrorCode'.
errorCodeException' :: OpusException -> ErrorCode
errorCodeException' OpusBadArg        = opus_bad_arg
errorCodeException' OpusBufferToSmall = opus_buffer_too_small
errorCodeException' OpusInternalError = opus_internal_error
errorCodeException' OpusInvalidPacket = opus_invalid_packet
errorCodeException' OpusUnimplemented = opus_unimplemented
errorCodeException' OpusInvalidState  = opus_invalid_state
errorCodeException' OpusAllocFail     = opus_alloc_fail

-- | Convert an 'ErrorCode' into an 'OpusException'. Returns Nothing if it is
-- not a known error code.
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


-- | A 'HasSamplingRate' typeclass, generated from the definition of
-- 'SamplingRate' using Template Haskell. This allows us to use 'samplingRate'
-- to access the 'SamplingRate' field of a data type such as 'EncoderConfig'.
makeClassy ''SamplingRate

-- | A 'HasCodingMode' typeclass, generated from the definition of 'CodingMode'
-- using Template Haskell. This allows us to use 'codingMode' to access the
-- 'CodingMode' field of a data type such as 'EncoderConfig'.
makeClassy ''CodingMode

-- | The configuration of an Opus encoder. Use 'mkEncoderConfig' to create a new
-- 'EncoderConfig'.
data EncoderConfig = EncoderConfig
  { _encoderSamplingRate :: SamplingRate
  -- ^ sampling rate of input signal
  , _encoderIsStereo     :: Bool
  -- ^ stereo mode? ('True' => 2 channels, 'False' => 1 channel)
  , _encoderCodingMode   :: CodingMode
  -- ^ Coding mode. (See 'app_voip', 'app_audio', 'app_lowdelay')
  } deriving (Eq, Show)

-- | A 'HasEncoderConfig' typeclass, generated from the definition of
-- 'EncoderConfig' using Template Haskell. This allows us to use 'encoderConfig'
-- to access the 'EncoderConfig' field of e.g. 'StreamConfig'.
makeClassy 'EncoderConfig

-- | Create a new 'EncoderConfig' with the given sampling rate, stereo mode, and
-- coding mode. Set the second argument to True for stereo mode, and False for
-- mono mode.
mkEncoderConfig :: SamplingRate -> Bool -> CodingMode -> EncoderConfig
mkEncoderConfig = EncoderConfig

-- | An 'EncoderConfig' has a reference to the 'SamplingRate' it is meant to be
-- used with.
instance HasSamplingRate EncoderConfig where
  samplingRate = encoderSamplingRate

-- | An 'EncoderConfig' has a reference to the 'CodingMode' it is meant to be
-- used with.
instance HasCodingMode EncoderConfig where
  codingMode = encoderCodingMode

-- | The configuration of an Opus decoder. Use 'mkDecoderConfig' to create a new
-- 'DecoderConfig'.
data DecoderConfig = DecoderConfig
  { _decoderSamplingRate :: SamplingRate
  , _decoderIsStereo     :: Bool
  } deriving (Eq, Show)

-- | A 'HasDecoderConfig' typeclass, generated from the definition of
-- 'DecoderConfig' using Template Haskell. This allows us to use 'decoderConfig'
-- to access the 'DecoderConfig' field of e.g. 'DecoderStreamConfig'.
makeClassy 'DecoderConfig

-- | Create a new 'DecoderConfig' with the given sampling rate and stereo mode.
-- Set the second argument to True for stereo mode, and False for mono mode.
mkDecoderConfig :: SamplingRate -> Bool -> DecoderConfig
mkDecoderConfig = DecoderConfig

-- | A 'DecoderConfig' has a reference to the 'SamplingRate' it is meant to be
-- used with.
instance HasSamplingRate DecoderConfig where
  samplingRate = decoderSamplingRate

-- | A type alias for the size of an Opus frame in integers.
type FrameSize = Int

-- | The configuration of an Opus encoder stream. Use 'mkStreamConfig' to
-- create a new 'StreamConfig'.
data StreamConfig = StreamConfig
  { _streamEncoderConfig :: EncoderConfig
  , _streamFrameSize     :: FrameSize
  , _streamOutSize       :: Int
  } deriving (Eq, Show)

-- | A 'HasStreamConfig' typeclass, generated from the definition of
-- 'StreamConfig' using Template Haskell.
makeClassy ''StreamConfig

-- | Create a new 'StreamConfig' with the given 'EncoderConfig', frame size, and
-- output size.
mkStreamConfig :: EncoderConfig -> FrameSize -> Int -> StreamConfig
mkStreamConfig = StreamConfig

-- | An 'StreamConfig' has a reference to the 'EncoderConfig' it was created
-- with.
instance HasEncoderConfig StreamConfig where
  encoderConfig = streamEncoderConfig

-- | An 'StreamConfig' has a reference to the 'SamplingRate' it is meant to be
-- used with.
instance HasSamplingRate StreamConfig where
  samplingRate = encoderConfig . samplingRate

-- | An 'StreamConfig' has a reference to the 'CodingMode' it is meant to be
-- used with.
instance HasCodingMode StreamConfig where
  codingMode = encoderConfig . codingMode

-- | The configuration of an Opus decoder stream. Use 'mkDecoderStreamConfig' to
-- create a new 'DecoderStreamConfig'.
data DecoderStreamConfig = DecoderStreamConfig
  { _deStreamDecoderConfig :: DecoderConfig
  , _deStreamFrameSize     :: FrameSize
  , _deStreamDecodeFec     :: Int
  } deriving (Eq, Show)

-- | A 'HasDecoderStreamConfig' typeclass, generated from the definition of
-- 'DecoderStreamConfig' using Template Haskell.
makeClassy ''DecoderStreamConfig

-- | Create a new 'DecoderStreamConfig' with the given 'DecoderConfig', frame
-- size, and FEC decode flag.
mkDecoderStreamConfig :: DecoderConfig -> FrameSize -> Int -> DecoderStreamConfig
mkDecoderStreamConfig = DecoderStreamConfig

-- | A 'DecoderStreamConfig' has a reference to the 'DecoderConfig' it was
-- created with.
instance HasDecoderConfig DecoderStreamConfig where
    decoderConfig = deStreamDecoderConfig

-- | A 'DecoderStreamConfig' has a reference to the 'SamplingRate' it is meant
-- to be used with.
instance HasSamplingRate DecoderStreamConfig where
    samplingRate = decoderConfig . samplingRate

