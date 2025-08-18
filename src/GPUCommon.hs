{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GPUCommon
-- Description : Common Initialization and Utility Functions for SDL GPU Examples
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- Common setup, teardown, and shader loading logic for SDL GPU examples,
-- assuming SDL3 Haskell bindings where:
-- - sdlGetBasePath returns IO (Maybe String) and manages C memory internally.
-- - sdlIOFromFile returns IO (Maybe (Ptr SDLIOStream)).
-- - sdlLoadFile returns IO (Maybe (Ptr (), CSize)), requiring caller to free Ptr.
-- - SDL.free exists and works.
module GPUCommon where

import Control.Exception (IOException, SomeException, bracket, catch, try)
import Control.Monad (unless, when)
-- Import the function from the auto-generated module
-- For path manipulation (optional but clean)

import Data.Bits ((.&.), (.|.))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Word (Word16, Word32, Word8)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..), peek)
import Paths_sdl3 (getDataFileName)
import SDL
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))

-- | Context structure
data Context = Context
  { contextDevice :: SDLGPUDevice,
    contextWindow :: SDLWindow
  }
  deriving (Show)

-- | Storables
data PositionColorVertex = PositionColorVertex
  { pcVertexX :: {-# UNPACK #-} !CFloat,
    pcVertexY :: {-# UNPACK #-} !CFloat,
    pcVertexZ :: {-# UNPACK #-} !CFloat,
    pcVertexR :: {-# UNPACK #-} !Word8,
    pcVertexG :: {-# UNPACK #-} !Word8,
    pcVertexB :: {-# UNPACK #-} !Word8,
    pcVertexA :: {-# UNPACK #-} !Word8
  }
  deriving (Show, Eq)

instance Storable PositionColorVertex where
  sizeOf _ = 16 -- 3*float (4 bytes) + 4*word8 (1 byte) = 12 + 4 = 16
  alignment _ = alignment (undefined :: CFloat)
  peek ptr = PositionColorVertex <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12 <*> peekByteOff ptr 13 <*> peekByteOff ptr 14 <*> peekByteOff ptr 15
  poke ptr (PositionColorVertex x y z r g b a) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr 4 y
    pokeByteOff ptr 8 z
    pokeByteOff ptr 12 r
    pokeByteOff ptr 13 g
    pokeByteOff ptr 14 b
    pokeByteOff ptr 15 a

-- PositionTextureVertex
data PositionTextureVertex = PositionTextureVertex
  { ptVertexX :: {-# UNPACK #-} !CFloat, -- pos.x
    ptVertexY :: {-# UNPACK #-} !CFloat, -- pos.y
    ptVertexZ :: {-# UNPACK #-} !CFloat, -- pos.z
    ptVertexU :: {-# UNPACK #-} !CFloat, -- tex.u
    ptVertexV :: {-# UNPACK #-} !CFloat -- tex.v
  }
  deriving (Show, Eq)

instance Storable PositionTextureVertex where
  sizeOf _ = 5 * sizeOf (undefined :: CFloat) -- 5 floats = 20 bytes
  alignment _ = alignment (undefined :: CFloat)
  peek ptr =
    PositionTextureVertex
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
      <*> peekByteOff ptr 16
  poke ptr (PositionTextureVertex x y z u v) = do
    pokeByteOff ptr 0 x
    pokeByteOff ptr 4 y
    pokeByteOff ptr 8 z
    pokeByteOff ptr 12 u
    pokeByteOff ptr 16 v

-- FragMultiplyUniform (for AnimatedQuads example fragment shader)
data FragMultiplyUniform = FragMultiplyUniform
  { multR :: {-# UNPACK #-} !Float,
    multG :: {-# UNPACK #-} !Float,
    multB :: {-# UNPACK #-} !Float,
    multA :: {-# UNPACK #-} !Float
  }
  deriving (Show, Eq)

instance Storable FragMultiplyUniform where
  sizeOf _ = 4 * sizeOf (undefined :: CFloat) -- 16 bytes
  alignment _ = alignment (undefined :: CFloat)
  peek ptr =
    FragMultiplyUniform
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 4
      <*> peekByteOff ptr 8
      <*> peekByteOff ptr 12
  poke ptr (FragMultiplyUniform r g b a) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 4 g
    pokeByteOff ptr 8 b
    pokeByteOff ptr 12 a

-- | Default Structs and Helpers
defaultShaderCreateInfo :: SDLGPUShaderCreateInfo
defaultShaderCreateInfo =
  SDLGPUShaderCreateInfo
    { shaderCode = nullPtr,
      shaderCodeSize = 0,
      shaderEntryPoint = "",
      shaderFormat = SDL_GPU_SHADERFORMAT_INVALID,
      shaderStage = SDL_GPU_SHADERSTAGE_VERTEX,
      shaderNumSamplers = 0,
      shaderNumStorageTextures = 0,
      shaderNumStorageBuffers = 0,
      shaderNumUniformBuffers = 0,
      shaderProps = 0
    }

defaultGraphicsPipelineTargetInfo :: SDLGPUTextureFormat -> SDLGPUGraphicsPipelineTargetInfo
defaultGraphicsPipelineTargetInfo swapchainFormat =
  SDLGPUGraphicsPipelineTargetInfo
    { colorTargets = [defaultColorTargetDescription {targetFormat = swapchainFormat}],
      depthStencilFormat = SDL_GPU_TEXTUREFORMAT_INVALID, -- Or a common depth format if always used
      hasDepthStencil = False
    }

defaultGraphicsPipelineCreateInfo ::
  SDLGPUShader ->
  SDLGPUShader ->
  SDLGPUTextureFormat -> -- For the primary color target
  SDLGPUGraphicsPipelineCreateInfo
defaultGraphicsPipelineCreateInfo vShader fShader swapchainFormat =
  SDLGPUGraphicsPipelineCreateInfo
    { vertexShader = vShader,
      fragmentShader = fShader,
      vertexInputState = defaultVertexInputState, -- Assuming this is a good general default
      primitiveType = SDL_GPU_PRIMITIVETYPE_TRIANGLELIST,
      rasterizerState = defaultRasterizerState,
      multisampleState = defaultMultiSampleState,
      depthStencilState = defaultDepthStencilState,
      targetInfo = defaultGraphicsPipelineTargetInfo swapchainFormat,
      pipelineProps = 0
    }

defaultComputePipelineCreateInfo :: SDLGPUComputePipelineCreateInfo
defaultComputePipelineCreateInfo =
  SDLGPUComputePipelineCreateInfo
    { code = nullPtr,
      codeSize = 0,
      entryPoint = "",
      compFormat = SDL_GPU_SHADERFORMAT_INVALID,
      numSamplers = 0,
      numReadOnlyStorageTextures = 0,
      numReadOnlyStorageBuffers = 0,
      numReadWriteStorageTextures = 0,
      numReadWriteStorageBuffers = 0,
      numUniformBuffers = 0,
      threadCountX = 1,
      threadCountY = 1,
      threadCountZ = 1,
      compProps = 0
    }

defaultColorTargetInfo :: SDLGPUColorTargetInfo
defaultColorTargetInfo =
  SDLGPUColorTargetInfo
    { texture = error "Texture must be set",
      mipLevel = 0,
      layerOrDepthPlane = 0,
      clearColor = SDLFColor 0 0 0 1,
      loadOp = SDL_GPU_LOADOP_DONT_CARE,
      storeOp = SDL_GPU_STOREOP_DONT_CARE,
      resolveTexture = Nothing,
      resolveMipLevel = 0,
      resolveLayer = 0,
      targetCycle = False,
      targetCycleResolve = False
    }

defaultSamplerCreateInfo :: SDLGPUFilter -> SDLGPUSamplerCreateInfo
defaultSamplerCreateInfo filt =
  SDLGPUSamplerCreateInfo
    filt
    filt
    SDL_GPU_SAMPLERMIPMAPMODE_NEAREST
    SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
    SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
    SDL_GPU_SAMPLERADDRESSMODE_CLAMP_TO_EDGE
    0.0
    1.0
    SDL_GPU_COMPAREOP_NEVER
    0.0
    0.0
    False
    False
    0

defaultColorTargetBlendState :: SDLGPUColorTargetBlendState
defaultColorTargetBlendState = SDLGPUColorTargetBlendState {writeMask = 0x0F, enableBlend = False, blendOp = SDL_GPU_BLENDOP_ADD, srcColorFactor = SDL_GPU_BLENDFACTOR_ONE, dstColorFactor = SDL_GPU_BLENDFACTOR_ZERO, alphaOp = SDL_GPU_BLENDOP_ADD, srcAlphaFactor = SDL_GPU_BLENDFACTOR_ONE, dstAlphaFactor = SDL_GPU_BLENDFACTOR_ZERO, enableColorWrite = True}

defaultColorTargetDescription :: SDLGPUColorTargetDescription
defaultColorTargetDescription =
  SDLGPUColorTargetDescription
    { targetFormat = SDL_GPU_TEXTUREFORMAT_B8G8R8A8_UNORM,
      targetBlendState = defaultColorTargetBlendState
    }

defaultRasterizerState :: SDLGPURasterizerState
defaultRasterizerState = SDLGPURasterizerState {fillMode = SDL_GPU_FILLMODE_FILL, cullMode = SDL_GPU_CULLMODE_NONE, frontFace = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE, enableDepthBias = False, depthBiasConstantFactor = 0.0, depthBiasClamp = 0.0, depthBiasSlopeFactor = 0.0, enableDepthClip = False}

defaultMultiSampleState :: SDLGPUMultisampleState
defaultMultiSampleState = SDLGPUMultisampleState {sampleCount = SDL_GPU_SAMPLECOUNT_1, sampleMask = 0, enableAlphaToCoverage = False, enableMask = False}

defaultStencilOpState :: SDLGPUStencilOpState
defaultStencilOpState = SDLGPUStencilOpState {stencilFailOp = SDL_GPU_STENCILOP_KEEP, stencilPassOp = SDL_GPU_STENCILOP_KEEP, stencilDepthFailOp = SDL_GPU_STENCILOP_KEEP, stencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS}

defaultDepthStencilState :: SDLGPUDepthStencilState
defaultDepthStencilState = SDLGPUDepthStencilState {enableDepthTest = False, enableDepthWrite = False, depthStencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS, enableStencilTest = False, backStencilState = defaultStencilOpState, frontStencilState = defaultStencilOpState, stencilCompareMask = 0xFF, stencilWriteMask = 0xFF}

defaultVertexInputState :: SDLGPUVertexInputState
defaultVertexInputState = SDLGPUVertexInputState {inputVertexBuffers = [], inputVertexAttribs = []}

defaultTextureCreateInfo :: Int -> Int -> SDLGPUTextureCreateInfo
defaultTextureCreateInfo w h =
  SDLGPUTextureCreateInfo
    SDL_GPU_TEXTURETYPE_2D
    SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM
    SDL_GPU_TEXTUREUSAGE_SAMPLER
    (fromIntegral w)
    (fromIntegral h)
    1
    1
    SDL_GPU_SAMPLECOUNT_1
    0

defaultTextureRegion :: SDLGPUTexture -> Int -> Int -> SDLGPUTextureRegion
defaultTextureRegion tex w h = SDLGPUTextureRegion tex 0 0 0 0 0 (fromIntegral w) (fromIntegral h) 1

defaultBlitRegion :: SDLGPUTexture -> Int -> Int -> SDLGPUBlitRegion
defaultBlitRegion tex w h = SDLGPUBlitRegion tex 0 0 0 0 (fromIntegral w) (fromIntegral h)

-- | commonInit
commonInit :: String -> [SDLWindowFlags] -> IO (Maybe Context)
commonInit exampleName windowFlags = do
  initSuccess <- sdlInit [SDL_INIT_VIDEO, SDL_INIT_EVENTS]
  unless initSuccess $ do
    sdlLog "Failed to initialize SDL!"
    sdlGetError >>= sdlLog . ("SDL Error: " ++)
    exitFailure

  let desiredFormats = SDL_GPU_SHADERFORMAT_SPIRV .|. SDL_GPU_SHADERFORMAT_DXIL .|. SDL_GPU_SHADERFORMAT_MSL
  let debugMode = False
  maybeDevice <- sdlCreateGPUDevice desiredFormats debugMode Nothing
  case maybeDevice of
    Nothing -> do
      sdlLog "Failed to create GPU Device!"
      sdlGetError >>= sdlLog . ("SDL Error: " ++)
      sdlQuit
      return Nothing
    Just device -> do
      deviceName <- sdlGetGPUDeviceDriver device >>= return . fromMaybe "Unknown Driver"
      sdlLog $ "GPU Device created successfully with driver: " ++ deviceName

      maybeWindow <- sdlCreateWindow exampleName 640 480 windowFlags
      case maybeWindow of
        Nothing -> do
          sdlLog "Failed to create window!"
          sdlGetError >>= sdlLog . ("SDL Error: " ++)
          sdlDestroyGPUDevice device
          sdlQuit
          return Nothing
        Just window -> do
          claimed <- sdlClaimWindowForGPUDevice device window
          if claimed
            then do
              sdlLog "Window claimed for GPU device."
              return $ Just (Context device window)
            else do
              sdlLog "Failed to claim window for GPU device!"
              sdlGetError >>= sdlLog . ("SDL Error: " ++)
              sdlDestroyWindow window
              sdlDestroyGPUDevice device
              return Nothing

-- | commonQuit
-- Takes a guaranteed valid Context
commonQuit :: Context -> IO ()
commonQuit Context {..} = do
  -- Direct record destructuring
  sdlLog "Starting common quit..."

  -- Wait for GPU Idle (Optional but recommended)
  sdlLog "Waiting for GPU idle before destroying device..."
  success <- sdlWaitForGPUIdle contextDevice
  unless success $ do
    err <- sdlGetError
    sdlLog $ "Warning: Failed to wait for GPU idle: " ++ err

  -- Release window from GPU device (No Maybe checks needed)
  sdlLog $ "Releasing window " ++ show contextWindow ++ " from device " ++ show contextDevice
  sdlReleaseWindowFromGPUDevice contextDevice contextWindow

  -- Destroy GPU device (No Maybe checks needed)
  sdlLog $ "Destroying GPU device " ++ show contextDevice
  sdlDestroyGPUDevice contextDevice

  -- Destroy window (No Maybe checks needed)
  sdlLog $ "Destroying window " ++ show contextWindow
  sdlDestroyWindow contextWindow

  -- Quit SDL Subsystems
  sdlLog "Quitting SDL subsystems..."
  sdlQuit

  sdlLog "Common quit finished."

-- | withContext (remains the same)
withContext :: String -> [SDLWindowFlags] -> (Context -> IO a) -> IO (Maybe a)
withContext exampleName windowFlags appAction =
  bracket
    (commonInit exampleName windowFlags)
    (maybe (return ()) commonQuit)
    (mapM appAction)

-- | Loads shader code from a file located via Cabal's data-files mechanism.
loadShader :: SDLGPUDevice -> FilePath -> SDLGPUShaderStage -> SDLGPUShaderCreateInfo -> IO (Maybe SDLGPUShader)
loadShader device baseFilename stage baseCreateInfo = do
  supportedFormats <- sdlGetGPUShaderFormats device

  let formatsToTry =
        [ (SDL_GPU_SHADERFORMAT_SPIRV, "SPIRV", "spv", "main"),
          (SDL_GPU_SHADERFORMAT_MSL, "MSL", "msl", "main0"),
          (SDL_GPU_SHADERFORMAT_DXIL, "DXIL", "dxil", "main")
        ]

  -- Helper to construct the relative path for getDataFileName
  let constructRelativePath fmtDir ext = "Content" </> "Shaders" </> "Compiled" </> fmtDir </> baseFilename ++ "." ++ ext
  cwd <- getCurrentDirectory
  putStrLn cwd
  -- Find first supported shader format where the *data file* exists
  maybeFound <-
    findM
      ( \(fmt, fmtDir, ext, _) ->
          if supportedFormats .&. fmt /= SDL_GPU_SHADERFORMAT_INVALID
            then do
              -- Get the absolute path from Cabal's data dir
              let relativePath = constructRelativePath fmtDir ext
              absolutePath <- getDataFileName relativePath -- Use Paths_sdl3 function
              -- Check if the file exists at that absolute path
              System.Directory.doesFileExist absolutePath
            else return False
      )
      formatsToTry

  case maybeFound of
    Nothing -> do
      sdlLog $ "Could not find suitable shader file for: " ++ baseFilename ++ " using getDataFileName."
      -- Log checked relative paths for debugging
      mapM_ (\(_, fd, ex, _) -> sdlLog $ "  Checked relative path: " ++ constructRelativePath fd ex) formatsToTry
      return Nothing -- Shader not found/supported
    Just (format, fmtDir, ext, entryPoint) -> do
      let relativePath = constructRelativePath fmtDir ext
      absolutePath <- getDataFileName relativePath
      sdlLog $ "Attempting to load shader from data file: " ++ absolutePath

      -- Use bracket to safely load file data using the absolute path
      bracket
        (sdlLoadFile absolutePath) -- Load using the absolute path
        ( \case
            Just (ptr, _) -> free ptr
            Nothing -> return ()
        )
        ( \case
            Nothing -> do
              sdlLog $ "SDL_LoadFile failed for: " ++ absolutePath
              return Nothing
            Just (codePtr, codeSize) -> do
              sdlLog $ "Successfully loaded " ++ show codeSize ++ " bytes."
              -- Populate shader creation info struct (override placeholders)
              let createInfo =
                    baseCreateInfo
                      { shaderCode = castPtr codePtr,
                        shaderCodeSize = codeSize,
                        shaderEntryPoint = entryPoint,
                        shaderFormat = format,
                        shaderStage = stage
                        -- Other fields like shaderNumSamplers remain from baseCreateInfo
                      }
              maybeShader <- sdlCreateGPUShader device createInfo
              when (isNothing maybeShader) $ do
                sdlLog $ "sdlCreateGPUShader failed for: " ++ absolutePath
                sdlGetError >>= sdlLog . ("SDL Error: " ++)
              return maybeShader
        )

createComputePipelineFromShader ::
  SDLGPUDevice ->
  -- | Base filename (e.g., "FillTexture.comp")
  FilePath ->
  -- | Base create info with resource counts, thread counts
  SDLGPUComputePipelineCreateInfo ->
  IO (Maybe SDLGPUComputePipeline)
createComputePipelineFromShader device baseFilename baseCreateInfo = do
  supportedFormats <- sdlGetGPUShaderFormats device
  let formatsToTry =
        [ (SDL_GPU_SHADERFORMAT_SPIRV, "SPIRV", "spv", "main"),
          (SDL_GPU_SHADERFORMAT_MSL, "MSL", "msl", "main0"),
          (SDL_GPU_SHADERFORMAT_DXIL, "DXIL", "dxil", "main")
        ]
  let constructRelativePath fmtDir ext = "Content" </> "Shaders" </> "Compiled" </> fmtDir </> baseFilename ++ "." ++ ext

  maybeFound <-
    findM
      ( \(fmt, fmtDir, ext, _) ->
          if supportedFormats .&. fmt /= SDL_GPU_SHADERFORMAT_INVALID
            then getDataFileName (constructRelativePath fmtDir ext) >>= System.Directory.doesFileExist
            else return False
      )
      formatsToTry

  case maybeFound of
    Nothing -> do
      sdlLog $ "Could not find suitable compute shader file for: " ++ baseFilename
      return Nothing
    Just (shaderFmt, fmtDir, ext, entryP) -> do
      let relativePath = constructRelativePath fmtDir ext
      absolutePath <- getDataFileName relativePath
      sdlLog $ "Attempting to load compute shader from: " ++ absolutePath
      bracket
        (sdlLoadFile absolutePath)
        (\loadResult -> case loadResult of Just (ptr, _) -> free ptr; Nothing -> return ())
        ( \loadResult -> case loadResult of
            Nothing -> do
              sdlLog $ "SDL_LoadFile failed for compute shader: " ++ absolutePath
              return Nothing
            Just (codePtr, loadedSize) -> do
              sdlLog $ "Successfully loaded " ++ show loadedSize ++ " bytes for compute shader."
              let finalCI =
                    baseCreateInfo
                      { code = castPtr codePtr,
                        codeSize = loadedSize, -- Ensure types match
                        entryPoint = entryP,
                        compFormat = shaderFmt
                      }
              sdlCreateGPUComputePipeline device finalCI
        )

-- | Loads an image using SDL_LoadBMP and converts it to ABGR8888 format if necessary.
-- Mimics the C example's LoadImage helper.
-- The caller is responsible for calling SDL_DestroySurface on the returned pointer
-- when it's no longer needed. This function handles destroying the *intermediate*
-- original surface if a conversion occurs.
loadImage ::
  -- | Relative path within the data directory (e.g., "Content/Images/ravioli.bmp")
  FilePath ->
  -- | Returns pointer to the final surface (ABGR8888) or Nothing on failure.
  IO (Maybe (Ptr SDLSurface))
loadImage relativeImagePath = do
  sdlLog $ "Attempting to load image: " ++ relativeImagePath
  fullPath <-
    getDataFileName relativeImagePath `catch` \(e :: IOException) -> do
      sdlLog $ "Error constructing image file path for '" ++ relativeImagePath ++ "': " ++ show e
      return ""

  -- Check if getDataFileName failed
  when (null fullPath) $ sdlLog "Failed to get absolute path using getDataFileName."
  if null fullPath
    then return Nothing
    else do
      -- Load the initial BMP surface
      maybeOriginalSurfPtr <- sdlLoadBMP fullPath
      case maybeOriginalSurfPtr of
        Nothing -> do
          sdlLog $ "SDL_LoadBMP failed for: " ++ fullPath
          sdlGetError >>= sdlLog . ("SDL Error: " ++)
          return Nothing
        Just originalSurfPtr -> do
          sdlLog $ "Loaded original surface: " ++ show originalSurfPtr ++ " from " ++ fullPath

          -- Peek the original surface's format
          originalData <- peek originalSurfPtr
          let originalFormat = surfaceFormat originalData
          let targetFormat = SDL_PIXELFORMAT_ABGR8888 -- Target format

          -- Check if conversion is needed
          if originalFormat == targetFormat
            then do
              sdlLog $ "Surface already in target format (" ++ show targetFormat ++ "). Returning original pointer."
              -- No conversion needed, return the pointer we loaded. Caller now owns it.
              return $ Just originalSurfPtr
            else do
              sdlLog $ "Original format (" ++ show originalFormat ++ ") differs from target (" ++ show targetFormat ++ "). Converting..."

              -- Attempt conversion
              eitherConverted <-
                try (sdlConvertSurface originalSurfPtr targetFormat) ::
                  IO (Either SomeException (Maybe (Ptr SDLSurface)))

              -- IMPORTANT: Destroy the original surface *after* the conversion attempt,
              -- regardless of whether it succeeded or failed. We no longer need it.
              -- Once this is wrapped properly we could just let GC handle it.
              sdlLog $ "Destroying original intermediate surface: " ++ show originalSurfPtr
              sdlDestroySurface originalSurfPtr

              -- Handle the result of the conversion attempt
              case eitherConverted of
                Left ex -> do
                  sdlLog $ "!!! Exception during SDL_ConvertSurface: " ++ show ex
                  return Nothing -- Conversion failed due to exception
                Right Nothing -> do
                  sdlLog "!!! SDL_ConvertSurface returned NULL (failed)."
                  sdlGetError >>= sdlLog . ("SDL Error: " ++)
                  return Nothing -- Conversion failed, returned null
                Right (Just convertedSurfPtr) -> do
                  sdlLog $ "Successfully converted to new surface: " ++ show convertedSurfPtr
                  -- Conversion succeeded, return the new pointer. Caller now owns it.
                  return $ Just convertedSurfPtr

-- | Generic Helpers

-- createGPUBuffer
createGPUBuffer :: SDLGPUDevice -> SDLGPUBufferUsageFlags -> Word32 -> String -> IO (Maybe SDLGPUBuffer)
createGPUBuffer dev usage size name = do
  sdlLog $ "Creating " ++ name ++ " (Size: " ++ show size ++ " bytes)..."
  let ci = SDLGPUBufferCreateInfo {bufferUsage = usage, bufferSize = size, bufferProps = 0}
  maybeBuf <- sdlCreateGPUBuffer dev ci
  when (isNothing maybeBuf) $ do
    err <- sdlGetError
    sdlLog $ "!!! Failed to create " ++ name ++ ": " ++ err
  return maybeBuf

-- cleanupCopyPass
cleanupCopyPass :: Maybe SDLGPUCopyPass -> IO ()
cleanupCopyPass = maybe (return ()) sdlEndGPUCopyPass

-- cleanupCommandBuffer
-- Command buffers are never actually released by the user, this is mostly for logging
cleanupCommandBuffer :: Maybe SDLGPUCommandBuffer -> IO ()
cleanupCommandBuffer Nothing = return ()
cleanupCommandBuffer (Just cmdbuf) =
  sdlLog $ "Upload Command Buffer bracket cleanup for: " ++ show cmdbuf

-- createTransferBuffer
createTransferBuffer ::
  SDLGPUDevice ->
  Word32 ->
  SDLGPUTransferBufferUsage ->
  String ->
  IO (Maybe SDLGPUTransferBuffer)
createTransferBuffer dev size usage name = do
  let logName = if null name then "Transfer Buffer" else name
  sdlLog $ "Creating " ++ logName ++ " (Size: " ++ show size ++ " bytes, Usage: " ++ show usage ++ ")..."
  let tbCreateInfo = SDLGPUTransferBufferCreateInfo usage size 0
  maybeTb <- sdlCreateGPUTransferBuffer dev tbCreateInfo
  when (isNothing maybeTb) $ do
    err <- sdlGetError
    sdlLog $ "!!! Failed to create " ++ logName ++ ": " ++ err
  return maybeTb

-- cleanupTransferBuffer
cleanupTransferBuffer :: SDLGPUDevice -> Maybe SDLGPUTransferBuffer -> IO ()
cleanupTransferBuffer _ Nothing = return ()
cleanupTransferBuffer dev (Just tb) = do
  sdlLog $ "Releasing Transfer Buffer: " ++ show tb
  sdlReleaseGPUTransferBuffer dev tb

-- cleanupMaybeRenderPass
cleanupMaybeRenderPass :: Maybe SDLGPURenderPass -> IO ()
cleanupMaybeRenderPass Nothing = return ()
cleanupMaybeRenderPass (Just rp) = sdlEndGPURenderPass rp

-- createGPUTeture
createGPUTexture :: SDLGPUDevice -> Int -> Int -> IO (Maybe SDLGPUTexture)
createGPUTexture dev w h = do
  sdlLog $ "Creating Texture (Size: " ++ show w ++ "x" ++ show h ++ ")..."
  let ci =
        SDLGPUTextureCreateInfo
          { texInfoType = SDL_GPU_TEXTURETYPE_2D,
            texInfoFormat = SDL_GPU_TEXTUREFORMAT_R8G8B8A8_UNORM,
            texInfoUsage = SDL_GPU_TEXTUREUSAGE_SAMPLER,
            texInfoWidth = fromIntegral w,
            texInfoHeight = fromIntegral h,
            texInfoLayerCountOrDepth = 1,
            texInfoNumLevels = 1,
            texInfoSampleCount = SDL_GPU_SAMPLECOUNT_1,
            texInfoProps = 0
          }
  maybeTex <- sdlCreateGPUTexture dev ci
  when (isNothing maybeTex) $ do
    err <- sdlGetError
    sdlLog $ "!!! Failed to create texture: " ++ err
  return maybeTex

-- Helper for mapping and copying raw data to a GPU buffer using a transferbuffer.
-- mapAndCopyBufferData
mapAndCopyBufferData :: SDLGPUDevice -> SDLGPUTransferBuffer -> [PositionTextureVertex] -> [Word16] -> Word32 -> Word32 -> IO Bool
mapAndCopyBufferData dev tb vertexData indexData vOffset iOffset = do
  sdlLog $ "Mapping Buffer Transfer Buffer: " ++ show tb
  bracket
    (sdlMapGPUTransferBuffer dev tb False)
    (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer dev tb)
    $ \case
      Nothing -> sdlLog "!!! Failed to map buffer transfer buffer." >> return False
      Just mappedPtr -> do
        sdlLog $ "Buffer Transfer Buffer mapped. Copying vertex data to offset " ++ show vOffset
        pokeArray (castPtr mappedPtr `plusPtr` fromIntegral vOffset) vertexData

        sdlLog $ "Copying index data to offset " ++ show iOffset
        pokeArray (castPtr mappedPtr `plusPtr` fromIntegral iOffset) indexData
        sdlLog "Buffer data copied."
        return True

-- Helper for mapping and copying texture data to GPU
mapAndCopyTextureData :: SDLGPUDevice -> SDLGPUTransferBuffer -> Ptr SDLSurface -> Int -> Int -> Int -> IO Bool
mapAndCopyTextureData dev tb surfacePtr w h bytesPerPixel = do
  sdlLog $ "Mapping Texture Transfer Buffer: " ++ show tb
  bracket
    (sdlMapGPUTransferBuffer dev tb False)
    (\mptr -> when (isJust mptr) $ sdlUnmapGPUTransferBuffer dev tb)
    $ \case
      Nothing -> sdlLog "!!! Failed to map texture transfer buffer." >> return False
      Just mappedPtr -> do
        surfaceData <- peek surfacePtr :: IO SDLSurface
        let pixelsPtr = surfacePixels surfaceData

        if pixelsPtr == nullPtr
          then do
            sdlLog "!!! Surface pixels pointer is NULL!"
            return False
          else do
            let dataSize = w * h * bytesPerPixel
            sdlLog $ "Texture Transfer Buffer mapped. Copying " ++ show dataSize ++ " bytes of pixel data."
            copyBytes (castPtr mappedPtr) pixelsPtr dataSize
            sdlLog "Texture data copied."
            return True

-- Calculates the total size of a buffer object.
calculateBufferDataSize :: forall a. (Storable a) => [a] -> String -> IO (Int, CSize, Word32)
calculateBufferDataSize dataList name = do
  let elementSize = sizeOf (undefined :: a)
  let numElements = length dataList
  let totalBytes = numElements * elementSize
  let totalCSize = fromIntegral totalBytes :: CSize
  let totalSizeWord32 = fromIntegral totalBytes :: Word32
  when (totalBytes == 0) $
    sdlLog $
      "!!! WARNING: " ++ name ++ " data is empty!"
  return (elementSize, totalCSize, totalSizeWord32)

-- | findM
findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x : xs) = do
  b <- p x
  if b then return (Just x) else findM p xs

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag =
  sdlLog $
    "  - " ++ case flag of
      SDL_INIT_AUDIO -> "Audio"
      SDL_INIT_VIDEO -> "Video"
      SDL_INIT_JOYSTICK -> "Joystick"
      SDL_INIT_HAPTIC -> "Haptic"
      SDL_INIT_GAMEPAD -> "Gamepad"
      SDL_INIT_EVENTS -> "Events"
      SDL_INIT_SENSOR -> "Sensor"
      SDL_INIT_CAMERA -> "Camera"
      _ -> "Unknown subsystem"

-- | ----------------- FFI Helpers --------------------

-- Define a type alias for the pixel data
type HDRImagePixelData = Ptr CFloat

foreign import ccall unsafe "hs_stbi_loadf_wrapper"
  c_stbi_loadf ::
    CString -> -- fullPath
    Ptr CInt -> -- pWidth
    Ptr CInt -> -- pHeight
    Ptr CInt -> -- pChannelsInFile
    CInt -> -- desiredChannels
    IO HDRImagePixelData

-- | Loads an HDR image from a file using stb_image.
-- The FilePath argument should be relative to your Cabal data directory's "Content/Images/" subfolder
-- (e.g., "memorial.hdr" if the full relative path is "Content/Images/memorial.hdr").
-- Returns Nothing on failure, or Just (pixelDataPtr, width, height, channelsInFile) on success.
-- The caller is responsible for freeing the pixelDataPtr using SDL's `free`.
loadHDRImageFromFile ::
  -- | Filename relative to "Content/Images/"
  FilePath ->
  -- | Desired channels (0=original, 3=RGB, 4=RGBA)
  Int ->
  IO (Maybe (HDRImagePixelData, Int, Int, Int))
loadHDRImageFromFile baseImageFilename desiredChannels = do
  -- Construct the full relative path as expected by getDataFileName
  let relativePath = "Content" </> "Images" </> baseImageFilename

  eitherAbsolutePath <- try (getDataFileName relativePath) :: IO (Either IOException FilePath)

  case eitherAbsolutePath of
    Left ex -> do
      sdlLog $ "Error resolving HDR image path for '" ++ relativePath ++ "' using getDataFileName: " ++ show ex
      sdlLog $ "Ensure the file is listed in your .cabal's data-files and accessible."
      return Nothing
    Right absolutePath -> do
      sdlLog $ "Attempting to load HDR image from resolved absolute path: " ++ absolutePath

      -- Optional: Check if the file exists at the absolute path before calling C
      fileExists <- System.Directory.doesFileExist absolutePath
      unless fileExists $ do
        sdlLog $ "Resolved HDR image path does not exist: " ++ absolutePath
      -- Fall through to stbi_loadf, which will also fail and return null,
      -- or return Nothing here directly:
      -- return Nothing

      -- Call the C wrapper with the absolute path
      withCString absolutePath $ \cAbsolutePath ->
        alloca $ \wPtr ->
          alloca $ \hPtr ->
            alloca $ \cPtr -> do
              pixelData <- c_stbi_loadf cAbsolutePath wPtr hPtr cPtr (fromIntegral desiredChannels)
              if pixelData == nullPtr
                then do
                  sdlLog $ "stbi_loadf failed for: " ++ absolutePath
                  -- You could try to get stbi_failure_reason() here if you added an FFI for it
                  return Nothing
                else do
                  width <- fromIntegral <$> peek wPtr
                  height <- fromIntegral <$> peek hPtr
                  channelsInFile <- fromIntegral <$> peek cPtr
                  sdlLog $ "Successfully loaded HDR image: " ++ absolutePath
                  sdlLog $ "Width: " ++ show width ++ " Height: " ++ show height ++ " Channels: " ++ show channelsInFile
                  return $ Just (pixelData, width, height, channelsInFile)
