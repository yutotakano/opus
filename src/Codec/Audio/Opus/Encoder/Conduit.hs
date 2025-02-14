-- | Conduit interface for encoding audio data with Opus.
module Codec.Audio.Opus.Encoder.Conduit
  ( encoderC, encoderLazyC
  , encoderSink
  ) where

import           Codec.Audio.Opus.Encoder
import           Conduit
import           Lens.Micro
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Data.Conduit.Combinators
import           Prelude                  (($))

-- | Encode audio data with Opus.
encoderC :: (HasStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString ByteString m ()
encoderC cfg = withEncoder (cfg ^. streamConfig) $
  \e -> mapM (opusEncode e cfg)

-- | Encode lazy bytestring audio data with Opus.
encoderLazyC :: (HasStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString BL.ByteString m ()
encoderLazyC cfg = withEncoder (cfg ^. streamConfig) $
  \e -> mapM (opusEncodeLazy e cfg)

-- | A sink to encode audio data with Opus and return a lazy bytestring of the
-- whole stream.
encoderSink :: (HasStreamConfig cfg, MonadResource m) =>
  cfg -> ConduitT ByteString o m BL.ByteString
encoderSink cfg = withEncoder (cfg ^. streamConfig) $
  \e -> foldMapM (opusEncodeLazy e cfg)

-- | Run a conduit that uses an encoder with the given configuration.
withEncoder :: (HasEncoderConfig cfg, MonadResource m) =>
  cfg -> (Encoder -> ConduitT i o m r) -> ConduitT i o m r
withEncoder cfg = bracketP (opusEncoderCreate cfg) opusEncoderDestroy
