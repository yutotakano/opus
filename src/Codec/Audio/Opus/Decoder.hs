{-# LANGUAGE FlexibleContexts #-}
-- | This module contains the high-level API for decoding Opus audio.
module Codec.Audio.Opus.Decoder
  ( -- * Decoder
    Decoder, OpusException(..)
    -- ** create
  , withOpusDecoder, opusDecoderCreate, opusDecoderDestroy
    -- ** run
  , opusDecode, opusDecodeLazy
    -- * re-exports
  , module Codec.Audio.Opus.Types
  ) where

import           Codec.Audio.Opus.Internal.Opus
import           Codec.Audio.Opus.Types
import           Lens.Micro
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import           Foreign

-- | Decoder. Internally, it holds a pointer to the libopus decoder state and
-- a pointer to the (potential) last Opus error code.
newtype Decoder = Decoder (ForeignPtr DecoderT, ForeignPtr ErrorCode)
  deriving (Eq, Ord, Show)

-- | Allocates and initializes a decoder.
opusDecoderCreate :: (HasDecoderConfig cfg, MonadIO m) => cfg -> m Decoder
opusDecoderCreate cfg = liftIO $ do
  let cs = if isStereo then 2 else 1
      sr = cfg ^. (decoderConfig . samplingRate)
      isStereo = cfg ^. decoderIsStereo
  err <- mallocForeignPtr
  d <- withForeignPtr err (c_opus_decoder_create sr cs)
  d' <- newForeignPtr cp_opus_decoder_destroy d
  let enc = Decoder (d', err)
  opusLastError enc >>= maybe (pure enc) throwM

-- | Decode an Opus frame.
opusDecode
  :: (HasDecoderStreamConfig cfg, MonadIO m)
  => Decoder
  -- ^ 'Decoder' state
  -> cfg
  -- ^ The stream configuration that specifies the frame size, whether FEC is
  -- enabled, and the decoder configuration (sampling rate, channels).
  -> ByteString
  -- ^ Input signal (interleaved if 2 channels)
  -> m ByteString
opusDecode d cfg i =
  let fs = cfg ^. deStreamFrameSize
      fec = cfg ^. deStreamDecodeFec
      conf = cfg ^. deStreamDecoderConfig
      chans = if conf ^. decoderIsStereo then 2 else 1
      pcm_length = fs * chans
  in liftIO $
  BS.useAsCStringLen i $ \(i', ilen) ->
    allocaArray pcm_length $ \os ->
      runDecoderAction d $ \d' -> do
        r <- c_opus_decode d' i' (fromIntegral ilen) os
          (fromIntegral fs) (fromIntegral fec)
        let l = fromIntegral r
        if l < 0 then do
          let mbException = ErrorCode l ^? _ErrorCodeException 
          case mbException of
              Nothing -> throwM OpusInvalidPacket
              Just x  -> throwM x
        else do
          -- multiply by 2 because "os" is CShort i.e. Int16
          -- but CStringLen expects a CChar which is Int8
          BS.packCStringLen $ (castPtr os, (fromIntegral l) * 2 * chans)

-- | Decode an Opus frame, returning a lazy 'BL.ByteString'.
opusDecodeLazy :: (HasDecoderStreamConfig cfg, MonadIO m)
  => Decoder
  -- ^ 'Decoder' state
  -> cfg
  -- ^ The stream configuration that specifies the frame size, whether FEC is
  -- enabled, and the decoder configuration (sampling rate, channels).
  -> ByteString
  -- ^ Input signal (interleaved if 2 channels)
  -> m BL.ByteString
opusDecodeLazy d cfg = fmap BL.fromStrict . opusDecode d cfg

-- | For use with 'ResourceT' or any other monad that implements 'MonadResource'.
-- Safely allocate a 'Decoder' that will be freed upon exiting the monad, an
-- exception, or an explicit call to 'Control.Monad.Trans.Resource.release'.
withOpusDecoder :: (HasDecoderConfig cfg) => MonadResource m
  => cfg
  -> (Decoder -> IO ())
  -> m Decoder
withOpusDecoder cfg a =
  snd <$> allocate (opusDecoderCreate cfg) a

-- | Frees an 'Decoder'.
opusDecoderDestroy :: MonadIO m => Decoder -> m ()
opusDecoderDestroy (Decoder (d, err)) = liftIO $
  finalizeForeignPtr d >> finalizeForeignPtr err

-- | Get the last error from decoder.
opusLastError :: MonadIO m => Decoder -> m (Maybe OpusException)
opusLastError (Decoder (_, fp)) =
  liftIO $ (^? _ErrorCodeException) <$> withForeignPtr fp peek

-- | An 'DecoderAction' is an IO action that uses a 'DecoderT' for its operation.
type DecoderAction  a = Ptr DecoderT -> IO a

-- | Run a 'DecoderAction' using a 'Decoder', returning either 'OpusException'
-- for errors or the result of the action.
withDecoder' :: MonadIO m =>
  Decoder -> DecoderAction a -> m (Either OpusException a)
withDecoder' e@(Decoder (fp_a, _)) m = liftIO $
  withForeignPtr fp_a $ \a -> do
    r <- m a
    le <- opusLastError e
    pure $ maybe (Right r) Left le

-- | Run a 'DecoderAction'. Might throw an 'OpusException' if the action fails.
runDecoderAction :: (MonadIO m, MonadThrow m) =>
  Decoder -> DecoderAction a -> m a
runDecoderAction d m = withDecoder' d m >>= either throwM pure
