{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module contains the raw FFI bindings to the Opus library. It is not
-- meant to be consumed directly by users of this library, but rather to be
-- used by the higher-level API in "Codec.Audio.Opus".
module Codec.Audio.Opus.Internal.Opus where

import           Foreign
import           Foreign.C.Types
import           Foreign.C.String

#include <opus.h>

-- | Raw error codes returned by the Opus library, represented as an int.
newtype ErrorCode = ErrorCode { unErrorCode :: CInt }
    deriving (Eq, Show)

-- | Storable instance for 'ErrorCode' which is necessary for using it an
-- argument in FFI calls.
instance Storable ErrorCode where
  sizeOf (ErrorCode e) = sizeOf e
  alignment (ErrorCode e) = alignment e
  peek p = ErrorCode <$> peek (castPtr p)
  poke p = poke (castPtr p) . unErrorCode

-- | A mapping of Haskell definitions of error codes to C counterparts.
#{enum ErrorCode, ErrorCode
  , opus_ok               = OPUS_OK
  , opus_bad_arg          = OPUS_BAD_ARG
  , opus_buffer_too_small = OPUS_BUFFER_TOO_SMALL
  , opus_internal_error   = OPUS_INTERNAL_ERROR
  , opus_invalid_packet   = OPUS_INVALID_PACKET
  , opus_unimplemented    = OPUS_UNIMPLEMENTED
  , opus_invalid_state    = OPUS_INVALID_STATE
  , opus_alloc_fail       = OPUS_ALLOC_FAIL
  }

-- | Coding mode for the Opus encoder, represented as an int.
newtype CodingMode = CodingMode { unCodingMode :: CInt }
    deriving (Eq)

-- | Show instance for 'CodingMode'.
instance Show CodingMode where
  show a
    | app_voip == a = "voip coding"
    | app_audio == a = "audio coding"
    | app_lowdelay == a = "lowdelay coding"
    | otherwise = "unknown coding"

-- | A mapping of Haskell definitions of coding modes to C counterparts.
#{enum CodingMode, CodingMode
 , app_voip = OPUS_APPLICATION_VOIP
 , app_audio = OPUS_APPLICATION_AUDIO
 , app_lowdelay = OPUS_APPLICATION_RESTRICTED_LOWDELAY
 }

-- | Sampling rate for the Opus encoder, represented as an int.
newtype SamplingRate = SamplingRate { unSamplingRate :: Int }
    deriving (Eq)

-- | Show instance for 'SamplingRate' makes it human-readable.
instance Show SamplingRate where
  show (SamplingRate r) = mconcat [show $ r `div` 1000, "kHz"]

-- | Sampling rate 8kHz
opusSR8k :: SamplingRate
opusSR8k = SamplingRate 8000

-- | Sampling rate 12kHz
opusSR12k :: SamplingRate
opusSR12k = SamplingRate 12000

-- | Sampling rate 16kHz
opusSR16k :: SamplingRate
opusSR16k = SamplingRate 16000

-- | Sampling rate 24kHz
opusSR24k :: SamplingRate
opusSR24k = SamplingRate 24000

-- | Sampling rate 48kHz
opusSR48k :: SamplingRate
opusSR48k = SamplingRate 48000

-- Declare empty (i.e. opaque) data types for the encoder and decoder states.
-- This is not meant to be consumed by Haskell code, but is rather meant to
-- encapsulate the C types that FFI calls return and expect to be passed
-- modified in a subsequent FFI call.
--
-- For example, the encoder state can be created only by 'c_opus_encoder_create',
-- and destroyed by 'cp_opus_encoder_destroy'.

-- | Encoder state. Can be created only by 'c_opus_encoder_create',
-- and destroyed by 'cp_opus_encoder_destroy'.
data EncoderT

-- | Decoder state. Can be created only by 'c_opus_decoder_create',
-- and destroyed by 'cp_opus_decoder_destroy'.
data DecoderT


-- | Allocates and initializes an encoder state.
foreign import ccall unsafe "opus.h opus_encoder_create"
    c_opus_encoder_create
      :: SamplingRate
      -- ^ Sampling rate of input signal (Hz).
      -> Int32
      -- ^ Number of channels (1 or 2) in input signal
      -> CodingMode
      -- ^ Coding mode. (See 'app_voip', 'app_audio', 'app_lowdelay')
      -> Ptr ErrorCode
      -- ^ 'ErrorCode' pointer
      -> IO (Ptr EncoderT)

-- | Frees an 'EncoderT' that has been created using 'c_opus_encoder_create'.
foreign import ccall unsafe "opus.h &opus_encoder_destroy"
    cp_opus_encoder_destroy
      :: FunPtr (Ptr EncoderT -> IO ())

-- | Encode an Opus frame.
foreign import ccall unsafe "opus.h opus_encode"
    c_opus_encode
      :: Ptr EncoderT
      -- ^ Encoder state
      -> Ptr CShort
      -- ^ Input signal
      -> Int32
      -- ^ Frame size
      -> CString
      -- ^ Output payload
      -> Int32
      -- ^ Max data bytes
      -> IO Int32
      -- ^ Number of bytes written or negative in case of error

-- | Allocates and initializes a decoder state.
foreign import ccall unsafe "opus.h opus_decoder_create"
    c_opus_decoder_create
      :: SamplingRate
      -- ^ Sampling rate, same as encoder_create
      -> Int32
      -- ^ Number of channels in input signal
      -> Ptr ErrorCode
      -- ^ 'ErrorCode' pointer
      -> IO (Ptr DecoderT)

-- | Frees a 'DecoderT' that has been created using 'c_opus_decoder_create'.
foreign import ccall unsafe "opus.h &opus_decoder_destroy"
    cp_opus_decoder_destroy
      :: FunPtr (Ptr DecoderT -> IO ())

-- | Decodes an Opus frame.
foreign import ccall unsafe "opus.h opus_decode"
    c_opus_decode
      :: Ptr DecoderT
      -- ^ Decoder state
      -> Ptr CChar
      -- ^ Byte array of compressed data
      -> Int32
      -- ^ Exact number of bytes in the payload
      -> Ptr CShort
      -- ^ Decoded audio data
      -> Int32
      -- ^ Max duration of the frame in samples that can fit
      -> CInt
      -- ^ Flag to request that any in-band forward error correction data be
      -- decoded. If no such data is available, the frame is decoded as if it
      -- were lost.
      -> IO Int32
      -- ^ Number of decoded samples, or negative in case of error

