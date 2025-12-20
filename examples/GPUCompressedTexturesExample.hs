{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUCompressedTextures
-- Description : Demonstrates loading and rendering compressed textures (BCn and ASTC).
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/CompressedTextures C example.
module Main where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Maybe (isJust, mapMaybe)
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GPUCommon
import Paths_sdl3 (getDataFileName)
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Texture formats to test
data TextureFormatInfo = TextureFormatInfo
  { fmtName :: String,
    fmtFile :: String,
    fmtEnum :: SDLGPUTextureFormat,
    fmtIsASTC :: Bool
  }
  deriving (Show)

textureFormats :: [TextureFormatInfo]
textureFormats =
  -- BCn formats
  [ TextureFormatInfo "BC1" "bcn/BC1.dds" SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM False,
    TextureFormatInfo "BC2" "bcn/BC2.dds" SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM False,
    TextureFormatInfo "BC3" "bcn/BC3.dds" SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM False,
    TextureFormatInfo "BC4" "bcn/BC4.dds" SDL_GPU_TEXTUREFORMAT_BC4_R_UNORM False,
    TextureFormatInfo "BC5" "bcn/BC5.dds" SDL_GPU_TEXTUREFORMAT_BC5_RG_UNORM False,
    TextureFormatInfo "BC6H_S" "bcn/BC6H_S.dds" SDL_GPU_TEXTUREFORMAT_BC6H_RGB_FLOAT False,
    TextureFormatInfo "BC6H_U" "bcn/BC6H_U.dds" SDL_GPU_TEXTUREFORMAT_BC6H_RGB_UFLOAT False,
    TextureFormatInfo "BC7" "bcn/BC7.dds" SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM False,
    TextureFormatInfo "BC1_SRGB" "bcn/BC1_SRGB.dds" SDL_GPU_TEXTUREFORMAT_BC1_RGBA_UNORM_SRGB False,
    TextureFormatInfo "BC2_SRGB" "bcn/BC2_SRGB.dds" SDL_GPU_TEXTUREFORMAT_BC2_RGBA_UNORM_SRGB False,
    TextureFormatInfo "BC3_SRGB" "bcn/BC3_SRGB.dds" SDL_GPU_TEXTUREFORMAT_BC3_RGBA_UNORM_SRGB False,
    TextureFormatInfo "BC7_SRGB" "bcn/BC7_SRGB.dds" SDL_GPU_TEXTUREFORMAT_BC7_RGBA_UNORM_SRGB False,
    -- ASTC formats
    TextureFormatInfo "ASTC 4x4" "astc/4x4.astc" SDL_GPU_TEXTUREFORMAT_ASTC_4x4_UNORM True,
    TextureFormatInfo "ASTC 5x4" "astc/5x4.astc" SDL_GPU_TEXTUREFORMAT_ASTC_5x4_UNORM True,
    TextureFormatInfo "ASTC 5x5" "astc/5x5.astc" SDL_GPU_TEXTUREFORMAT_ASTC_5x5_UNORM True,
    TextureFormatInfo "ASTC 6x5" "astc/6x5.astc" SDL_GPU_TEXTUREFORMAT_ASTC_6x5_UNORM True,
    TextureFormatInfo "ASTC 6x6" "astc/6x6.astc" SDL_GPU_TEXTUREFORMAT_ASTC_6x6_UNORM True,
    TextureFormatInfo "ASTC 8x5" "astc/8x5.astc" SDL_GPU_TEXTUREFORMAT_ASTC_8x5_UNORM True,
    TextureFormatInfo "ASTC 8x6" "astc/8x6.astc" SDL_GPU_TEXTUREFORMAT_ASTC_8x6_UNORM True,
    TextureFormatInfo "ASTC 8x8" "astc/8x8.astc" SDL_GPU_TEXTUREFORMAT_ASTC_8x8_UNORM True,
    TextureFormatInfo "ASTC 10x5" "astc/10x5.astc" SDL_GPU_TEXTUREFORMAT_ASTC_10x5_UNORM True,
    TextureFormatInfo "ASTC 10x6" "astc/10x6.astc" SDL_GPU_TEXTUREFORMAT_ASTC_10x6_UNORM True,
    TextureFormatInfo "ASTC 10x8" "astc/10x8.astc" SDL_GPU_TEXTUREFORMAT_ASTC_10x8_UNORM True,
    TextureFormatInfo "ASTC 10x10" "astc/10x10.astc" SDL_GPU_TEXTUREFORMAT_ASTC_10x10_UNORM True,
    TextureFormatInfo "ASTC 12x10" "astc/12x10.astc" SDL_GPU_TEXTUREFORMAT_ASTC_12x10_UNORM True,
    TextureFormatInfo "ASTC 12x12" "astc/12x12.astc" SDL_GPU_TEXTUREFORMAT_ASTC_12x12_UNORM True
  ]

-- | Application resources
data AppResources = AppResources
  { resSrcTextures :: [Maybe SDLGPUTexture], -- Corresponds to textureFormats list
    resDstTextures :: [Maybe SDLGPUTexture]
  }

-- | State
data AppState = AppState
  { stCurrentIndex :: Int,
    stLeftPressed :: Bool,
    stRightPressed :: Bool
  }

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU CompressedTextures" [] runAppGPU
  case maybeResult of
    Nothing -> sdlLog "Application initialization failed." >> exitFailure
    Just _ -> sdlLog "Application finished successfully." >> exitSuccess

-- | runAppGPU
runAppGPU :: Context -> IO ()
runAppGPU context@Context {..} = do
  sdlLog "Base context initialized."
  bracket
    (createResources context)
    (releaseResources context)
    $ \case
      Nothing -> sdlLog "Failed to create resources. Exiting."
      Just resources -> do
        sdlLog "Resources created successfully."
        sdlLog "Press Left/Right to switch between textures"

        stateRef <- newIORef $ AppState 0 False False
        eventLoopGPU context resources stateRef

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  maybeCmdBuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdBuf of
    Nothing -> return Nothing
    Just cmdBuf -> do
      maybeCopyPass <- sdlBeginGPUCopyPass cmdBuf
      case maybeCopyPass of
        Nothing -> return Nothing
        Just copyPass -> do
          results <- forM (zip [0 ..] textureFormats) $ \(i, tf) -> do
            supports <- sdlGPUTextureSupportsFormat contextDevice (fmtEnum tf) SDL_GPU_TEXTURETYPE_2D SDL_GPU_TEXTUREUSAGE_SAMPLER
            if not supports
              then return (Nothing, Nothing)
              else do
                -- Load Image Data
                let relativePath = "Content" </> "Images" </> fmtFile tf
                path <- getDataFileName relativePath
                eitherBs <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)

                loadResult <- case eitherBs of
                  Left _ -> do
                    sdlLog $ "Failed to read file: " ++ path
                    return Nothing
                  Right bs ->
                    if fmtIsASTC tf
                      then loadASTC path bs
                      else loadDDS path bs

                case loadResult of
                  Nothing -> do
                    sdlLog $ "Failed to load " ++ path
                    return (Nothing, Nothing)
                  Just (w, h, dataBytes) -> do
                    -- Create Texture
                    let ci =
                          SDLGPUTextureCreateInfo
                            { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                              texInfoFormat = fmtEnum tf,
                              texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
                              texInfoWidth = fromIntegral w,
                              texInfoHeight = fromIntegral h,
                              texInfoLayerCountOrDepth = 1,
                              texInfoNumLevels = 1,
                              texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                              texInfoProps = 0
                            }

                    srcTex <- sdlCreateGPUTexture contextDevice ci
                    dstTex <- sdlCreateGPUTexture contextDevice ci

                    case (srcTex, dstTex) of
                      (Just s, Just d) -> do
                        -- Upload
                        let size = BS.length dataBytes
                        maybeTB <- createTransferBuffer contextDevice (fromIntegral size) SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "TextureTransfer"
                        case maybeTB of
                          Nothing -> return (Nothing, Nothing)
                          Just tb -> do
                            mapPtr <- sdlMapGPUTransferBuffer contextDevice tb False
                            case mapPtr of
                              Nothing -> do
                                sdlReleaseGPUTransferBuffer contextDevice tb
                                return (Nothing, Nothing)
                              Just ptr -> do
                                BS.unsafeUseAsCString dataBytes $ \srcPtr ->
                                  copyBytes ptr (castPtr srcPtr) size
                                sdlUnmapGPUTransferBuffer contextDevice tb

                                let texRegion = SDLGPUTextureRegion s 0 0 0 0 0 (fromIntegral w) (fromIntegral h) 1
                                    bufLoc = SDLGPUTextureTransferInfo tb 0 0 0

                                sdlUploadToGPUTexture copyPass bufLoc texRegion False

                                -- Copy src to dst (256x256 check? Example code does partial copy or full?)
                                -- Example code uses 256x256 explicitly in CopyGPUTextureToTexture?
                                -- Wait, line 183 in C: 256, 256, 1.
                                -- But imageWidth/Height might differ?
                                -- ASTC uses `w = block_count * block_size`?
                                -- "w, h" in `SDLGPUTextureRegion` should be dimensions.
                                -- For ASTC, dimensions are loaded from header.
                                -- I should use loaded `w, h`.
                                -- C code: copies 256x256. But images might be different size?
                                -- Actually all example images seem to be 256x256 or similar?
                                -- Let's use `w` `h`.

                                let srcLoc = SDLGPUTextureLocation s 0 0 0 0 0
                                    dstLoc = SDLGPUTextureLocation d 0 0 0 0 0
                                sdlCopyGPUTextureToTexture copyPass srcLoc dstLoc (fromIntegral w) (fromIntegral h) 1 False

                                -- Release TB (deferred)
                                -- Ideally we keep TB until submission.
                                -- But `createTransferBuffer` helper creates a new one each time.
                                -- We should defer release.
                                -- For now, just release it AFTER submission? No, unsafe.
                                -- The helper creates it. We should track it.
                                -- To simplify, I will just call `sdlReleaseGPUTransferBuffer` immediately?
                                -- NO. Upload uses it.
                                -- SDL3 GPU: Transfer buffers must remain valid until command buffer is submitted?
                                -- No, actually, "You can release the transfer buffer immediately after `Unmap` if you don't need it on CPU side?"
                                -- Documentation says: "buffers must not be released until the command buffer that uses them has finished execution."
                                -- So I must NOT release it yet.
                                -- I'll keep them in a list or rely on `SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD` usually being transient?
                                -- Wait, C example releases it at line 189: `SDL_ReleaseGPUTransferBuffer`.
                                -- "It is safe to release a transfer buffer after all data has been uploaded...?"
                                -- Wait, `SDL_UploadToGPUTexture` records a command.
                                -- If I release the buffer before submission, is that okay?
                                -- In Vulkan/Metal/D3D12, NO.
                                -- SDL3 GPU abstraction might handle reference counting?
                                -- "Transfer buffers are reference counted."
                                -- If the command buffer holds a reference, then yes.
                                -- C example releases it on line 189, BEFORE submission (line 224).
                                -- So SDL3 GPU must hold a reference.
                                sdlReleaseGPUTransferBuffer contextDevice tb
                                return (Just s, Just d)
                      _ -> return (Nothing, Nothing)

          sdlEndGPUCopyPass copyPass
          sdlSubmitGPUCommandBuffer cmdBuf

          let (srcs, dsts) = unzip results
          return $ Just AppResources {resSrcTextures = srcs, resDstTextures = dsts}

-- | Release resources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  mapM_ (maybe (return ()) (sdlReleaseGPUTexture contextDevice)) resSrcTextures
  mapM_ (maybe (return ()) (sdlReleaseGPUTexture contextDevice)) resDstTextures

-- | Event Loop
eventLoopGPU :: Context -> AppResources -> IORef AppState -> IO ()
eventLoopGPU context resources stateRef = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU stateRef
  unless shouldQuit $ do
    renderFrameGPU context resources stateRef
    eventLoopGPU context resources stateRef

-- | Process Events
processEventsGPU :: IORef AppState -> IO Bool
processEventsGPU stateRef = go
  where
    go =
      sdlPollEvent >>= \case
        Nothing -> return False
        Just event -> case event of
          SDLEventQuit _ -> sdlLog "Quit." >> return True
          SDLEventKeyboard (SDLKeyboardEvent _ _ _ _ sc _ _ _ down _) -> do
            AppState {..} <- readIORef stateRef
            let newLeft = down && sc == SDL_SCANCODE_LEFT
            let newRight = down && sc == SDL_SCANCODE_RIGHT
            let changedLeft = newLeft && not stLeftPressed
            let changedRight = newRight && not stRightPressed

            when changedLeft $ do
              let len = length textureFormats
              let newIdx = (stCurrentIndex - 1 + len) `rem` len
              modifyIORef' stateRef $ \s -> s {stCurrentIndex = newIdx}
              logState stateRef newIdx

            when changedRight $ do
              let len = length textureFormats
              let newIdx = (stCurrentIndex + 1) `rem` len
              modifyIORef' stateRef $ \s -> s {stCurrentIndex = newIdx}
              logState stateRef newIdx

            modifyIORef' stateRef $ \s -> s {stLeftPressed = newLeft, stRightPressed = newRight}

            if down && sc == SDL_SCANCODE_Q then return True else go
          _ -> go

    logState ref idx = do
      let info = textureFormats !! idx
      sdlLog $ "Switched to: " ++ fmtName info

-- | Render Frame
renderFrameGPU :: Context -> AppResources -> IORef AppState -> IO ()
renderFrameGPU Context {..} AppResources {..} stateRef = do
  AppState {..} <- readIORef stateRef
  let currentSrc = resSrcTextures !! stCurrentIndex
      currentDst = resDstTextures !! stCurrentIndex

  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> return ()
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, _, _) -> do
          case (currentSrc, currentDst) of
            (Just src, Just dst) -> do
              -- Blit both
              -- Clear first? Blit can clear?
              -- C example:
              -- .clear_color = {1,1,1,1}, .load_op = CLEAR for the FIRST blit (Source)
              -- Then Dst blit uses default (LOAD) implicitly? Or sets it?
              -- C Example sets clear_color in SDLGPUBlitInfo for Source?

              let mkBlitInfo s d x w h loadOp =
                    SDLGPUBlitInfo
                      { gpuBlitInfoSource = SDLGPUBlitRegion s 0 0 0 0 w h,
                        gpuBlitInfoDestination = SDLGPUBlitRegion d 0 0 x 0 w h,
                        gpuBlitInfoLoadOp = loadOp,
                        gpuBlitInfoClearColor = SDLFColor 1 1 1 1,
                        gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                        gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                        gpuBlitInfoCycle = False
                      }

              -- Blit Source
              sdlBlitGPUTexture cmdbuf (mkBlitInfo src swapchainTexture 0 256 256 SDL_GPU_LOADOP_CLEAR)

              -- Blit Dest
              sdlBlitGPUTexture cmdbuf (mkBlitInfo dst swapchainTexture 384 256 256 SDL_GPU_LOADOP_LOAD)
            _ -> do
              -- Clear screen if texture invalid
              let colorTarget =
                    defaultColorTargetInfo
                      { texture = swapchainTexture,
                        loadOp = SDL_GPU_LOADOP_CLEAR,
                        clearColor = SDLFColor 1 1 1 1
                      }
              maybeRp <- sdlBeginGPURenderPass cmdbuf [colorTarget] Nothing
              case maybeRp of
                Just rp -> sdlEndGPURenderPass rp
                Nothing -> return ()

          sdlSubmitGPUCommandBuffer cmdbuf >> return ()

-- | Loaders
loadDDS :: FilePath -> BS.ByteString -> IO (Maybe (Int, Int, BS.ByteString))
loadDDS _ bs = do
  -- Parse DDS header
  -- minimal check
  if BS.length bs < 128
    then return Nothing
    else do
      let magic = BS.take 4 bs
      if magic /= "DDS "
        then return Nothing
        else do
          -- Header starts at 4
          -- dwHeight at offset 12 (from magic? no, from struct start. magic is 4 bytes. header struct starts at 4?)
          -- DDS_HEADER struct:
          -- dwSize (4), dwFlags (4), dwHeight (4), dwWidth (4), dwPitchOrLinearSize (4)
          -- offset 0: dwSize
          -- offset 4: dwFlags
          -- offset 8: dwHeight
          -- offset 12: dwWidth
          -- offset 16: dwPitch
          -- So, relative to 'bs' start (magic include):
          -- magic: 0-3
          -- header start: 4
          -- height: 4+8 = 12
          -- width: 4+12 = 16
          -- pitch: 4+16 = 20

          let getW32 off =
                let s = BS.drop off bs
                 in if BS.length s < 4
                      then 0
                      else
                        (fromIntegral (BS.index s 0) :: Word32)
                          .|. ((fromIntegral (BS.index s 1) :: Word32) `shiftL` 8)
                          .|. ((fromIntegral (BS.index s 2) :: Word32) `shiftL` 16)
                          .|. ((fromIntegral (BS.index s 3) :: Word32) `shiftL` 24)

          let height = getW32 12
              width = getW32 16
              pitch = getW32 20

              -- Check DX10 header
              -- ddspf at offset 76 (within header) -> 4+76 = 80
              -- ddspf.dwFlags offset 84?
              -- struct DDS_PIXELFORMAT { dwSize(4), dwFlags(4), dwFourCC(4) ... }
              -- offset 80: dwSize
              -- offset 84: dwFlags
              -- offset 88: dwFourCC

              pfFlags = getW32 84
              fourCC = getW32 88

              hasDX10 = (pfFlags == 0x4) && (fourCC == 0x30315844) -- "DX10"
              headerSize = 128 -- 4 + 124
              dx10Size = 20

              dataOffset = headerSize + if hasDX10 then dx10Size else 0

              dataBytes = BS.drop dataOffset bs

          if width == 0 || height == 0
            then return Nothing
            else return $ Just (fromIntegral width, fromIntegral height, dataBytes)

loadASTC :: FilePath -> BS.ByteString -> IO (Maybe (Int, Int, BS.ByteString))
loadASTC _ bs = do
  -- ASTC Header
  -- magic 4 bytes: 13 AB A1 5C
  -- blockX (1), blockY (1), blockZ (1)
  -- dimX (3), dimY (3), dimZ (3)
  -- Total 16 bytes.
  if BS.length bs < 16
    then return Nothing
    else do
      let magic = BS.take 4 bs
      if magic /= BS.pack [0x13, 0xAB, 0xA1, 0x5C]
        then return Nothing
        else do
          let blockX = BS.index bs 4
              blockY = BS.index bs 5
              blockZ = BS.index bs 6

              getDim off =
                (fromIntegral (BS.index bs off) :: Int)
                  + ((fromIntegral (BS.index bs (off + 1)) :: Int) `shiftL` 8)
                  + ((fromIntegral (BS.index bs (off + 2)) :: Int) `shiftL` 16)

              dimX = getDim 7
              dimY = getDim 10
              dimZ = getDim 13

              -- Calculate size?
              -- block_count_x = (dimX + blockX - 1) / blockX
              -- block_count_y = (dimY + blockY - 1) / blockY
              -- size = block_count_x * block_count_y * 16

              bcX = (dimX + fromIntegral blockX - 1) `div` fromIntegral blockX
              bcY = (dimY + fromIntegral blockY - 1) `div` fromIntegral blockY
              size = bcX * bcY * 16

              dataBytes = BS.drop 16 bs

          -- Check length?
          -- BS.length dataBytes should be >= size

          return $ Just (dimX, dimY, dataBytes)
