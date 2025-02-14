-- | Conduit interface for decoding audio data with Opus.
module Codec.Audio.Opus.Decoder.Conduit
  ( decoderC, decoderLazyC
  , decoderSink
  ) where

import           Codec.Audio.Opus.Decoder
import           Conduit
import           Lens.Micro
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Conduit.Combinators
import           Prelude                  (($))

-- | Decode audio data with Opus.
decoderC :: (HasDecoderStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString ByteString m ()
decoderC cfg = withDecoder (cfg ^. deStreamDecoderConfig) $
  \d -> mapM (opusDecode d cfg)

-- | Decode lazy bytestring audio data with Opus.
decoderLazyC :: (HasDecoderStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString BL.ByteString m ()
decoderLazyC cfg = withDecoder (cfg ^. deStreamDecoderConfig) $
  \d -> mapM (opusDecodeLazy d cfg)

-- | A sink to decode audio data with Opus and return a lazy bytestring of the
-- whole stream.
decoderSink :: (HasDecoderStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString o m BL.ByteString
decoderSink cfg = withDecoder (cfg ^. deStreamDecoderConfig) $
  \d -> foldMapM (opusDecodeLazy d cfg)

-- | Run a conduit that uses a decoder with the given configuration.
withDecoder :: (HasDecoderConfig cfg, MonadResource m) =>
  cfg -> (Decoder -> ConduitT i o m r) -> ConduitT i o m r
withDecoder cfg = bracketP (opusDecoderCreate cfg) opusDecoderDestroy
