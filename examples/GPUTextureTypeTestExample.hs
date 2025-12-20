{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Example     : GPUTextureTypeTest
-- Description : Demonstrates and tests different texture types (2D, 2D Array, 3D, Cube, Cube Array).
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Based on the SDL_gpu_examples/TextureTypeTest C example.
-- This example demonstrates:
-- - Creating textures of various types (2D, 2D Array, 3D, Cube, Cube Array)
-- - Uploading data to specific slices/layers/mips using tiling
-- - Copying data between different texture types
-- - Blitting from different texture types to the swapchain
module Main where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Word (Word32, Word8)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (copyBytes, fromBool)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GPUCommon
import SDL3
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

-- | Texture types array for display
textureTypeNames :: [String]
textureTypeNames = ["2D", "2DArray", "3D", "Cubemap", "CubemapArray"]

-- | Base mip slices for each type (from C example)
baseMipSlices :: [Word32]
baseMipSlices = [0, 1, 1, 1, 7]

-- | Second mip slices for each type (from C example)
secondMipSlices :: [Word32]
secondMipSlices = [0, 1, 0, 1, 7]

-- | Application resources
data AppResources = AppResources
  { resSrcTextures :: [SDLGPUTexture], -- 5 source textures
    resDstTextures :: [SDLGPUTexture], -- 5 dest textures
    resBaseMips :: [Ptr SDLSurface], -- 4 base mip surfaces
    resSecondMips :: [Ptr SDLSurface] -- 4 second mip surfaces
  }
  deriving (Show)

-- | State
data AppState = AppState
  { stSrcIndex :: Int,
    stDstIndex :: Int,
    stLeftPressed :: Bool,
    stRightPressed :: Bool
  }

-- | main
main :: IO ()
main = do
  sdlLog $ "Compiled SDL Version: " ++ show sdlVersion
  linkedVersion <- sdlGetVersion
  sdlLog $ "Linked SDL Version: " ++ show linkedVersion
  maybeResult <- withContext "SDL3 Haskell GPU TextureTypeTest" [] runAppGPU
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
        sdlLog "Press Left to cycle through source texture types."
        sdlLog "Press Right to cycle through destination texture types."
        sdlLog "(2D / 2D)"

        stateRef <- newIORef $ AppState 0 0 False False
        eventLoopGPU context resources stateRef

-- | createResources
createResources :: Context -> IO (Maybe AppResources)
createResources Context {..} = do
  sdlLog "--- Beginning Resource Creation ---"

  -- Load images
  -- We need baseMips[0..3] and secondMips[0..3]
  -- Files: cube0.bmp, cube0mip1.bmp, etc.
  let loadImages = do
        mips1 <- mapM (\i -> loadImage ("Content" </> "Images" </> ("cube" ++ show i ++ ".bmp"))) [0 .. 3]
        mips2 <- mapM (\i -> loadImage ("Content" </> "Images" </> ("cube" ++ show i ++ "mip1.bmp"))) [0 .. 3]
        if all isJust mips1 && all isJust mips2
          then return $ Just (map (\(Just p) -> p) mips1, map (\(Just p) -> p) mips2)
          else return Nothing

  loadImages >>= \case
    Nothing -> do
      sdlLog "Failed to load images."
      return Nothing
    Just (baseMips, secondMips) -> do
      -- Define create infos
      let baseCI =
            SDLGPUTextureCreateInfo
              { texInfoType = SDL_GPU_TEXTURETYPE_2D,
                texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
                texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER .|. SDL_GPU_TEXTUREUSAGE_COLOR_TARGET, -- Need COLOR_TARGET for Blit
                texInfoWidth = 64,
                texInfoHeight = 64,
                texInfoLayerCountOrDepth = 1,
                texInfoNumLevels = 2,
                texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
                texInfoProps = 0
              }

      -- Create 5 pairs of textures
      let createPair ty layerCount = do
            let ci = baseCI {texInfoType = ty, texInfoLayerCountOrDepth = layerCount}
            t1 <- sdlCreateGPUTexture contextDevice ci
            t2 <- sdlCreateGPUTexture contextDevice ci
            return (t1, t2)

      -- 1. 2D
      (src0, dst0) <- createPair SDL_GPU_TEXTURETYPE_2D 1
      -- 2. 2D Array
      (src1, dst1) <- createPair SDL_GPU_TEXTURETYPE_2D_ARRAY 2
      -- 3. 3D
      (src2, dst2) <- createPair SDL_GPU_TEXTURETYPE_3D 2
      -- 4. Cubemap
      (src3, dst3) <- createPair SDL_GPU_TEXTURETYPE_CUBE 6
      -- 5. Cubemap Array
      (src4, dst4) <- createPair SDL_GPU_TEXTURETYPE_CUBE_ARRAY 12

      let srcs = [src0, src1, src2, src3, src4]
          dsts = [dst0, dst1, dst2, dst3, dst4]

      if any (not . isJust) srcs || any (not . isJust) dsts
        then do
          sdlLog "Failed to create some textures."
          mapM_ (maybe (return ()) (sdlReleaseGPUTexture contextDevice)) srcs
          mapM_ (maybe (return ()) (sdlReleaseGPUTexture contextDevice)) dsts
          mapM_ sdlDestroySurface baseMips
          mapM_ sdlDestroySurface secondMips
          return Nothing
        else do
          let validSrcs = map (\(Just t) -> t) srcs
              validDsts = map (\(Just t) -> t) dsts

          -- Upload data (Initial population of SrcTextures)
          success <- uploadTextureData contextDevice validSrcs baseMips secondMips
          if success
            then do
              sdlLog "--- Resource Creation Successful ---"
              return $
                Just
                  AppResources
                    { resSrcTextures = validSrcs,
                      resDstTextures = validDsts,
                      resBaseMips = baseMips,
                      resSecondMips = secondMips
                    }
            else do
              sdlLog "Failed to upload texture data."
              return Nothing

-- | uploadTextureData
uploadTextureData :: SDLGPUDevice -> [SDLGPUTexture] -> [Ptr SDLSurface] -> [Ptr SDLSurface] -> IO Bool
uploadTextureData device srcTextures baseMips secondMips = do
  -- Calculate sizes
  surf0 <- peek (head baseMips)
  let w0 = surfaceW surf0
      h0 = surfaceH surf0
      baseDataSize = fromIntegral (w0 * h0 * 4) :: Int

  surf1 <- peek (head secondMips)
  let w1 = surfaceW surf1
      h1 = surfaceH surf1
      secondDataSize = fromIntegral (w1 * h1 * 4) :: Int

  let totalSize = fromIntegral (4 * (baseDataSize + secondDataSize)) :: Word32

  maybeTB <- createTransferBuffer device totalSize SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD "TextureTypeTransfer"
  case maybeTB of
    Nothing -> return False
    Just tb -> do
      mapSuccess <- bracket
        (sdlMapGPUTransferBuffer device tb False)
        (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer device tb)
        $ \case
          Nothing -> return False
          Just ptr -> do
            -- Fill buffer
            forM_ [0 .. 3] $ \i -> do
              let offset = i * (baseDataSize + secondDataSize)
                  destPtr = castPtr ptr `plusPtr` offset

              s0 <- peek (baseMips !! i)
              copyBytes destPtr (surfacePixels s0) baseDataSize

              s1 <- peek (secondMips !! i)
              copyBytes (destPtr `plusPtr` baseDataSize) (surfacePixels s1) secondDataSize
            return True

      if mapSuccess
        then bracket
          (sdlAcquireGPUCommandBuffer device)
          cleanupCommandBuffer
          $ \case
            Nothing -> return False
            Just cmd -> do
              mcp <- sdlBeginGPUCopyPass cmd
              case mcp of
                Nothing -> return False
                Just cp -> do
                  forM_ [0 .. 3] $ \i -> do
                    let offset = fromIntegral $ i * (baseDataSize + secondDataSize)
                        x0 = (fromIntegral i `rem` 2) * 32 :: Word32
                        y0 = (fromIntegral i `div` 2) * 32 :: Word32
                        x1 = (fromIntegral i `rem` 2) * 16 :: Word32
                        y1 = (fromIntegral i `div` 2) * 16 :: Word32

                    let baseLoc = SDLGPUTextureTransferInfo tb offset 0 0
                        secondLoc = SDLGPUTextureTransferInfo tb (offset + fromIntegral baseDataSize) 0 0

                    -- 2D (Index 0)
                    sdlUploadToGPUTexture cp baseLoc (SDLGPUTextureRegion (srcTextures !! 0) 0 0 x0 y0 0 32 32 1) False
                    sdlUploadToGPUTexture cp secondLoc (SDLGPUTextureRegion (srcTextures !! 0) 1 0 x1 y1 0 16 16 1) False

                    -- 2D Array (Index 1) - Layer 0/1 based on i? No, C code says layer=1 fixedly for upload target?
                    -- C: SrcTextures[1], 0, 1, x0...
                    sdlUploadToGPUTexture cp baseLoc (SDLGPUTextureRegion (srcTextures !! 1) 0 1 x0 y0 0 32 32 1) False
                    sdlUploadToGPUTexture cp secondLoc (SDLGPUTextureRegion (srcTextures !! 1) 1 1 x1 y1 0 16 16 1) False

                    -- 3D (Index 2) - z=1 for base, z=0 for mip1?
                    -- C: SrcTextures[2], 0, 0, x0, y0, 1 ...
                    sdlUploadToGPUTexture cp baseLoc (SDLGPUTextureRegion (srcTextures !! 2) 0 0 x0 y0 1 32 32 1) False
                    sdlUploadToGPUTexture cp secondLoc (SDLGPUTextureRegion (srcTextures !! 2) 1 0 x1 y1 0 16 16 1) False

                    -- Cubemap (Index 3) - layer=1
                    sdlUploadToGPUTexture cp baseLoc (SDLGPUTextureRegion (srcTextures !! 3) 0 1 x0 y0 0 32 32 1) False
                    sdlUploadToGPUTexture cp secondLoc (SDLGPUTextureRegion (srcTextures !! 3) 1 1 x1 y1 0 16 16 1) False

                    -- Cubemap Array (Index 4) - layer=7
                    sdlUploadToGPUTexture cp baseLoc (SDLGPUTextureRegion (srcTextures !! 4) 0 7 x0 y0 0 32 32 1) False
                    sdlUploadToGPUTexture cp secondLoc (SDLGPUTextureRegion (srcTextures !! 4) 1 7 x1 y1 0 16 16 1) False

                  sdlEndGPUCopyPass cp
                  cleanupTransferBuffer device (Just tb)
                  sdlSubmitGPUCommandBuffer cmd
        else return False

-- | releaseResources
releaseResources :: Context -> Maybe AppResources -> IO ()
releaseResources _ Nothing = return ()
releaseResources Context {..} (Just AppResources {..}) = do
  mapM_ (sdlReleaseGPUTexture contextDevice) resSrcTextures
  mapM_ (sdlReleaseGPUTexture contextDevice) resDstTextures
  mapM_ sdlDestroySurface resBaseMips
  mapM_ sdlDestroySurface resSecondMips

-- | eventLoopGPU
eventLoopGPU :: Context -> AppResources -> IORef AppState -> IO ()
eventLoopGPU context resources stateRef = do
  sdlPumpEvents
  shouldQuit <- processEventsGPU stateRef
  unless shouldQuit $ do
    renderFrameGPU context resources stateRef
    eventLoopGPU context resources stateRef

-- | processEventsGPU
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
              let newIdx = (stSrcIndex + 1) `rem` 5
              modifyIORef' stateRef $ \s -> s {stSrcIndex = newIdx}
              logState stateRef

            when changedRight $ do
              let newIdx = (stDstIndex + 1) `rem` 5
              modifyIORef' stateRef $ \s -> s {stDstIndex = newIdx}
              logState stateRef

            modifyIORef' stateRef $ \s -> s {stLeftPressed = newLeft, stRightPressed = newRight}

            if down && sc == SDL_SCANCODE_Q then return True else go
          _ -> go

    logState ref = do
      AppState {..} <- readIORef ref
      let sName = textureTypeNames !! stSrcIndex
      let dName = textureTypeNames !! stDstIndex
      sdlLog $ "(" ++ sName ++ " / " ++ dName ++ ")"

-- | renderFrameGPU
renderFrameGPU :: Context -> AppResources -> IORef AppState -> IO ()
renderFrameGPU Context {..} AppResources {..} stateRef = do
  AppState {..} <- readIORef stateRef

  maybeCmdbuf <- sdlAcquireGPUCommandBuffer contextDevice
  case maybeCmdbuf of
    Nothing -> sdlLog "Error: Failed to acquire render command buffer."
    Just cmdbuf -> do
      maybeSwapResult <- sdlWaitAndAcquireGPUSwapchainTexture cmdbuf contextWindow
      case maybeSwapResult of
        Nothing -> void $ sdlSubmitGPUCommandBuffer cmdbuf
        Just (swapchainTexture, winW, winH) -> do
          -- Clear screen
          let colorTarget =
                defaultColorTargetInfo
                  { texture = swapchainTexture,
                    loadOp = SDL_GPU_LOADOP_CLEAR,
                    clearColor = SDLFColor 0 0 0 1
                  }
          maybeRp <- sdlBeginGPURenderPass cmdbuf [colorTarget] Nothing
          case maybeRp of
            Just rp -> sdlEndGPURenderPass rp
            Nothing -> return ()

          -- Copy Source to Dest
          maybeCp <- sdlBeginGPUCopyPass cmdbuf
          case maybeCp of
            Nothing -> return ()
            Just cp -> do
              -- Copy 4 tiles
              forM_ [0 .. 3] $ \i -> do
                let x0 = (fromIntegral i `rem` 2) * 32 :: Word32
                    y0 = (fromIntegral i `div` 2) * 32 :: Word32
                    x1 = (fromIntegral i `rem` 2) * 16 :: Word32
                    y1 = (fromIntegral i `div` 2) * 16 :: Word32

                -- Determine layer/z logic
                -- Src logic
                let srcTex = resSrcTextures !! stSrcIndex
                    srcBaseLayer = if stSrcIndex == 2 then 0 else (baseMipSlices !! stSrcIndex)
                    srcBaseZ = if stSrcIndex == 2 then (baseMipSlices !! stSrcIndex) else 0
                    srcSecondLayer = if stSrcIndex == 2 then 0 else (secondMipSlices !! stSrcIndex)
                    srcSecondZ = if stSrcIndex == 2 then (secondMipSlices !! stSrcIndex) else 0

                -- Dst logic
                let dstTex = resDstTextures !! stDstIndex
                    dstBaseLayer = if stDstIndex == 2 then 0 else (baseMipSlices !! stDstIndex)
                    dstBaseZ = if stDstIndex == 2 then (baseMipSlices !! stDstIndex) else 0
                    dstSecondLayer = if stDstIndex == 2 then 0 else (secondMipSlices !! stDstIndex)
                    dstSecondZ = if stDstIndex == 2 then (secondMipSlices !! stDstIndex) else 0

                -- Copy base mip
                let srcLocBase = SDLGPUTextureLocation srcTex 0 srcBaseLayer x0 y0 srcBaseZ
                    dstLocBase = SDLGPUTextureLocation dstTex 0 dstBaseLayer x0 y0 dstBaseZ
                sdlCopyGPUTextureToTexture cp srcLocBase dstLocBase 32 32 1 False

                -- Copy second mip
                let srcLocSecond = SDLGPUTextureLocation srcTex 1 srcSecondLayer x1 y1 srcSecondZ
                    dstLocSecond = SDLGPUTextureLocation dstTex 1 dstSecondLayer x1 y1 dstSecondZ
                sdlCopyGPUTextureToTexture cp srcLocSecond dstLocSecond 16 16 1 False

              sdlEndGPUCopyPass cp

          -- Blit Helper
          let mkBlitInfo src dst =
                SDLGPUBlitInfo
                  { gpuBlitInfoSource = src,
                    gpuBlitInfoDestination = dst,
                    gpuBlitInfoLoadOp = SDL_GPU_LOADOP_LOAD,
                    gpuBlitInfoClearColor = SDLFColor 0 0 0 0,
                    gpuBlitInfoFlipMode = SDL_FLIP_NONE,
                    gpuBlitInfoFilter = SDL_GPU_FILTER_NEAREST,
                    gpuBlitInfoCycle = False
                  }

          -- Blit Source to Swapchain (Top Left)
          let srcTex = resSrcTextures !! stSrcIndex
              srcBaseLayerOrDepth = baseMipSlices !! stSrcIndex
              srcSecondLayerOrDepth = secondMipSlices !! stSrcIndex

          let blitSrcBase = SDLGPUBlitRegion srcTex 0 srcBaseLayerOrDepth 0 0 64 64
              blitDstBase = SDLGPUBlitRegion swapchainTexture 0 0 0 0 128 128
          sdlBlitGPUTexture cmdbuf (mkBlitInfo blitSrcBase blitDstBase)

          let blitSrcSecond = SDLGPUBlitRegion srcTex 1 srcSecondLayerOrDepth 0 0 32 32
              blitDstSecond = SDLGPUBlitRegion swapchainTexture 0 0 128 0 64 64
          sdlBlitGPUTexture cmdbuf (mkBlitInfo blitSrcSecond blitDstSecond)

          -- Blit Dest to Swapchain (Top Right / Far Right)
          let dstTex = resDstTextures !! stDstIndex
              dstBaseLayerOrDepth = baseMipSlices !! stDstIndex
              dstSecondLayerOrDepth = secondMipSlices !! stDstIndex

          let blitDstBaseSrc = SDLGPUBlitRegion dstTex 0 dstBaseLayerOrDepth 0 0 64 64
              blitDstBaseDst = SDLGPUBlitRegion swapchainTexture 0 0 256 0 128 128
          sdlBlitGPUTexture cmdbuf (mkBlitInfo blitDstBaseSrc blitDstBaseDst)

          let blitDstSecondSrc = SDLGPUBlitRegion dstTex 1 dstSecondLayerOrDepth 0 0 32 32
              blitDstSecondDst = SDLGPUBlitRegion swapchainTexture 0 0 384 0 64 64
          sdlBlitGPUTexture cmdbuf (mkBlitInfo blitDstSecondSrc blitDstSecondDst)

          void $ sdlSubmitGPUCommandBuffer cmdbuf
