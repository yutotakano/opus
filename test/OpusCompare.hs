-- | This module is a wrapper around opus_compare.c.
-- Calling 'compareFiles' is equivalent to running the opus_compare executable.
-- Due to conflicting definitions of "main" (one in opus_compare and one in our
-- Haskell program), we assume that opus_compare has been compiled with a define
-- that renames its main function to "opus_compare_main".
module OpusCompare where

import           Codec.Audio.Opus.Internal.Opus
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String

-- | Channel type
data Channel = Mono | Stereo
  deriving (Eq, Show)

-- | Compares two Opus files using the opus_compare executable code. The result
-- is True if the files are quality-wise identical (which doesn't necessarily
-- mean that they are byte-wise identical due to entropy in the Opus encoding).
-- The result is False if the files are measurably different.
compareFiles :: Channel -> SamplingRate -> FilePath -> FilePath -> IO Bool
compareFiles channel samplingRate filePath1 filePath2 =
    withCString "opus_compare" $ \cProgramName ->
      withCString "-r" $ \cSROpt ->  -- sampling rate option
        withCString (show $ unSamplingRate samplingRate) $ \cSRVal -> -- sampling rate value
          withCString filePath1 $ \cFile1 -> -- first file
            withCString filePath2 $ \cFile2 -> -- second file
              if channel == Mono
                then allocaArray 5 $ \p -> do
                  pokeArray p [cProgramName, cSROpt, cSRVal, cFile1, cFile2]
                  c_opus_compare_main 5 p >>= \r -> return (r == 0)
                else withCString "-s" $ \cSOpt -> -- stereo option
                  allocaArray 6 $ \p -> do
                    pokeArray p [cProgramName, cSOpt, cSROpt, cSRVal, cFile1, cFile2]
                    c_opus_compare_main 6 p >>= \r -> return (r == 0)

-- | Call the opus_compare_main function, which should be the main function
-- within opus_compare.c. We assume that when compiling opus_compare.c, a
-- define was used to rename the main function to "opus_compare_main".
foreign import ccall unsafe "opus_compare.c opus_compare_main"
    c_opus_compare_main
      :: Int
      -- ^ number of arguments
      -> Ptr (Ptr CChar)
      -- ^ arguments
      -> IO Int
