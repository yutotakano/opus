{-# LANGUAGE FlexibleContexts #-}
-- | This module contains the high-level API for encoding Opus audio.
module Codec.Audio.Opus.Encoder
  ( -- * Encoder
    Encoder, OpusException(..)
    -- ** create
  , withOpusEncoder, opusEncoderCreate, opusEncoderDestroy
    -- ** run
  , opusEncode, opusEncodeLazy
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

-- | Encoder. Internally, it holds a pointer to the libopus encoder state and
-- a pointer to the (potential) last Opus error code.
newtype Encoder = Encoder (ForeignPtr EncoderT, ForeignPtr ErrorCode)
  deriving (Eq, Ord, Show)

-- | Allocates and initializes an encoder.
opusEncoderCreate :: (HasEncoderConfig cfg, MonadIO m) => cfg -> m Encoder
opusEncoderCreate cfg = liftIO $ do
  let cs = if isStereo then 2 else 1
      sr = cfg ^. (encoderConfig . samplingRate)
      isStereo = cfg ^. encoderIsStereo
      cm = cfg ^. (encoderConfig . codingMode)
  err <- mallocForeignPtr
  e <- withForeignPtr err (c_opus_encoder_create sr cs cm)
  e' <- newForeignPtr cp_opus_encoder_destroy e
  let enc = Encoder (e', err)
  opusLastError enc >>= maybe (pure enc) throwM

-- | Encode an Opus frame.
opusEncode
  :: (HasStreamConfig cfg, MonadIO m)
  => Encoder
  -- ^ 'Encoder' state
  -> cfg
  -- ^ The stream configuration that specifies the frame size, the output size,
  -- and the encoder configuration (sampling rate, channels, coding mode).
  -> ByteString
  -- ^ Input signal (interleaved if 2 channels)
  -> m ByteString
opusEncode e cfg i =
  let fs = cfg ^. streamFrameSize
      n = cfg ^. streamOutSize
  in liftIO $
  BS.useAsCString i $ \i' ->
    allocaArray n $ \os ->
      runEncoderAction e $ \e' -> do
        r <- c_opus_encode e' (castPtr i') (fromInteger . toInteger $ fs) os
          (fromInteger . toInteger $ n)
        let l = fromInteger . toInteger $ r
            ol = (os, l)
        if l < 0 then throwM OpusInvalidPacket else
          BS.packCStringLen ol

-- | Encode an Opus frame. Returns a lazy 'BL.ByteString'.
opusEncodeLazy :: (HasStreamConfig cfg, MonadIO m)
  => Encoder
  -- ^ 'Encoder' state
  -> cfg
  -- ^ The stream configuration that specifies the frame size, the output size,
  -- and the encoder configuration (sampling rate, channels, coding mode).
  -> ByteString
  -- ^ Input signal (interleaved if 2 channels)
  -> m BL.ByteString
opusEncodeLazy e cfg = fmap BL.fromStrict . opusEncode e cfg


-- | For use with 'ResourceT' or any other monad that implements 'MonadResource'.
-- Safely allocate an 'Encoder' that will be freed upon exiting the monad, an
-- exception, or an explicit call to 'Control.Monad.Trans.Resource.release'.
withOpusEncoder :: (HasEncoderConfig cfg) => MonadResource m
  => cfg
  -> (Encoder -> IO ())
  -> m Encoder
withOpusEncoder cfg a =
  snd <$> allocate (opusEncoderCreate cfg) a


-- | Frees an 'Encoder'.
opusEncoderDestroy :: MonadIO m => Encoder -> m ()
opusEncoderDestroy (Encoder (e, err)) = liftIO $
  finalizeForeignPtr e >> finalizeForeignPtr err


-- | Get the last error from the encoder.
opusLastError :: MonadIO m => Encoder -> m (Maybe OpusException)
opusLastError (Encoder (_, fp)) =
  liftIO $ (^? _ErrorCodeException) <$> withForeignPtr fp peek

-- | An 'EncoderAction' is an IO action that uses a 'EncoderT' for its operation.
type EncoderAction  a = Ptr EncoderT -> IO a

-- | Run an 'EncoderAction' using an 'Encoder', returning either 'OpusException'
-- for errors or the result of the action.
withEncoder' :: MonadIO m =>
  Encoder -> EncoderAction a -> m (Either OpusException a)
withEncoder' e@(Encoder (fp_a, _)) m = liftIO $
  withForeignPtr fp_a $ \a -> do
    r <- m a
    le <- opusLastError e
    pure $ maybe (Right r) Left le

-- | Run an 'EncoderAction'. Might throw an 'OpusException' if the action fails.
runEncoderAction :: (MonadIO m, MonadThrow m) =>
  Encoder -> EncoderAction a -> m a
runEncoderAction e m = withEncoder' e m >>= either throwM pure
