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

-- | libopus error: No error.
opus_ok :: ErrorCode
opus_ok = ErrorCode (#const OPUS_OK)

-- | libopus error: One or more invalid/out of range arguments.
opus_bad_arg :: ErrorCode
opus_bad_arg = ErrorCode (#const OPUS_BAD_ARG)

-- | libopus error: Not enough bytes allocated in the buffer.
opus_buffer_too_small :: ErrorCode
opus_buffer_too_small = ErrorCode (#const OPUS_BUFFER_TOO_SMALL)

-- | libopus error: An internal error was detected.
opus_internal_error :: ErrorCode
opus_internal_error = ErrorCode (#const OPUS_INTERNAL_ERROR)

-- | libopus error: The compressed data passed is corrupted.
opus_invalid_packet :: ErrorCode
opus_invalid_packet = ErrorCode (#const OPUS_INVALID_PACKET)

-- | libopus error: Invalid/unsupported request number.
opus_unimplemented :: ErrorCode
opus_unimplemented = ErrorCode (#const OPUS_UNIMPLEMENTED)

-- | libopus error: An encoder or decoder structure is invalid or already freed.
opus_invalid_state :: ErrorCode
opus_invalid_state = ErrorCode (#const OPUS_INVALID_STATE)

-- | libopus error: Memory allocation has failed.
opus_alloc_fail :: ErrorCode
opus_alloc_fail = ErrorCode (#const OPUS_ALLOC_FAIL)

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

-- | Best for most VoIP/videoconference applications where listening quality and
-- intelligibility matter most.
app_voip :: CodingMode
app_voip = CodingMode (#const OPUS_APPLICATION_VOIP)

-- | Best for broadcast/high-fidelity application where the decoded audio should
-- be as close as possible to the input.
app_audio :: CodingMode
app_audio = CodingMode (#const OPUS_APPLICATION_AUDIO)

-- | Only use when lowest-achievable latency is what matters most.
app_lowdelay :: CodingMode
app_lowdelay = CodingMode (#const OPUS_APPLICATION_RESTRICTED_LOWDELAY)

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

