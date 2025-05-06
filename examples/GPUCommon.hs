{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : GPUCommon
Description : Common Initialization and Utility Functions for SDL GPU Examples
Copyright   : (c) Kyle Lukaszek, 2025
License     : BSD3

Common setup, teardown, and shader loading logic for SDL GPU examples,
assuming SDL3 Haskell bindings where:
- sdlGetBasePath returns IO (Maybe String) and manages C memory internally.
- sdlIOFromFile returns IO (Maybe (Ptr SDLIOStream)).
- sdlLoadFile returns IO (Maybe (Ptr (), CSize)), requiring caller to free Ptr.
- SDL.free exists and works.
-}
module GPUCommon where

import SDL

import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.C.String (peekCString)
import Foreign.Storable (peek, Storable(..))

import Control.Monad (unless, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket, catch, try, Exception, IOException, SomeException)

import System.Exit (exitFailure)
import Paths_sdl3 (getDataFileName) -- Import the function from the auto-generated module
import System.FilePath ((</>))      -- For path manipulation (optional but clean)
import System.Directory (doesFileExist, getCurrentDirectory)

import Data.Maybe (fromMaybe, isNothing, isJust, fromJust)
import Data.Bits ((.|.), (.&.))
import Data.List (find)
import Data.Word (Word8)
import qualified Data.ByteString as BS

-- | Context structure
data Context = Context
    { contextDevice :: SDLGPUDevice
    , contextWindow :: SDLWindow
    } deriving (Show)

data PositionColorVertex = PositionColorVertex
    { pcVertexX :: {-# UNPACK #-} !CFloat
    , pcVertexY :: {-# UNPACK #-} !CFloat
    , pcVertexZ :: {-# UNPACK #-} !CFloat
    , pcVertexR :: {-# UNPACK #-} !Word8
    , pcVertexG :: {-# UNPACK #-} !Word8
    , pcVertexB :: {-# UNPACK #-} !Word8
    , pcVertexA :: {-# UNPACK #-} !Word8
    } deriving (Show, Eq)

instance Storable PositionColorVertex where
    sizeOf _ = 16 -- 3*float (4 bytes) + 4*word8 (1 byte) = 12 + 4 = 16
    alignment _ = alignment (undefined :: CFloat)
    peek ptr = PositionColorVertex <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8 <*> peekByteOff ptr 12 <*> peekByteOff ptr 13 <*> peekByteOff ptr 14 <*> peekByteOff ptr 15
    poke ptr (PositionColorVertex x y z r g b a) = do
        pokeByteOff ptr 0 x; pokeByteOff ptr 4 y; pokeByteOff ptr 8 z;
        pokeByteOff ptr 12 r; pokeByteOff ptr 13 g; pokeByteOff ptr 14 b; pokeByteOff ptr 15 a

data PositionTextureVertex = PositionTextureVertex
    { ptVertexX :: {-# UNPACK #-} !CFloat -- pos.x
    , ptVertexY :: {-# UNPACK #-} !CFloat -- pos.y
    , ptVertexZ :: {-# UNPACK #-} !CFloat -- pos.z
    , ptVertexU :: {-# UNPACK #-} !CFloat -- tex.u
    , ptVertexV :: {-# UNPACK #-} !CFloat -- tex.v
    } deriving (Show, Eq)

instance Storable PositionTextureVertex where
    sizeOf _ = 5 * sizeOf (undefined :: CFloat) -- 5 floats = 20 bytes
    alignment _ = alignment (undefined :: CFloat)
    peek ptr = PositionTextureVertex
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

-- Default Structs
defaultShaderCreateInfo :: SDLGPUShaderCreateInfo
defaultShaderCreateInfo = SDLGPUShaderCreateInfo { shaderCode = nullPtr, shaderCodeSize = 0, shaderEntryPoint = "", shaderFormat = SDL_GPU_SHADERFORMAT_INVALID, shaderStage = SDL_GPU_SHADERSTAGE_VERTEX, shaderNumSamplers = 0, shaderNumStorageTextures = 0, shaderNumStorageBuffers = 0, shaderNumUniformBuffers = 0, shaderProps = 0 }
defaultColorTargetInfo :: SDLGPUColorTargetInfo
defaultColorTargetInfo = SDLGPUColorTargetInfo
    { texture           = error "Texture must be set"
    , mipLevel          = 0
    , layerOrDepthPlane = 0
    , clearColor        = SDLFColor 0 0 0 1
    , loadOp            = SDL_GPU_LOADOP_DONT_CARE
    , storeOp           = SDL_GPU_STOREOP_DONT_CARE
    , resolveTexture    = Nothing
    , resolveMipLevel   = 0
    , resolveLayer      = 0
    , targetCycle             = False
    , targetCycleResolve      = False
    }
defaultColorTargetBlendState :: SDLGPUColorTargetBlendState
defaultColorTargetBlendState = SDLGPUColorTargetBlendState { writeMask = 0x0F, enableBlend = False, blendOp = SDL_GPU_BLENDOP_ADD, srcColorFactor = SDL_GPU_BLENDFACTOR_ONE, dstColorFactor = SDL_GPU_BLENDFACTOR_ZERO, alphaOp = SDL_GPU_BLENDOP_ADD, srcAlphaFactor = SDL_GPU_BLENDFACTOR_ONE, dstAlphaFactor = SDL_GPU_BLENDFACTOR_ZERO, enableColorWrite = True }
defaultRasterizerState :: SDLGPURasterizerState
defaultRasterizerState = SDLGPURasterizerState { fillMode = SDL_GPU_FILLMODE_FILL, cullMode = SDL_GPU_CULLMODE_NONE, frontFace = SDL_GPU_FRONTFACE_COUNTER_CLOCKWISE, enableDepthBias = False, depthBiasConstantFactor = 0.0, depthBiasClamp = 0.0, depthBiasSlopeFactor = 0.0, enableDepthClip = False }
defaultMultiSampleState :: SDLGPUMultisampleState
defaultMultiSampleState = SDLGPUMultisampleState { sampleCount = SDL_GPU_SAMPLECOUNT_1, sampleMask = 0, enableAlphaToCoverage = False, enableMask = False }
defaultStencilOpState :: SDLGPUStencilOpState
defaultStencilOpState = SDLGPUStencilOpState { stencilFailOp = SDL_GPU_STENCILOP_KEEP, stencilPassOp = SDL_GPU_STENCILOP_KEEP, stencilDepthFailOp = SDL_GPU_STENCILOP_KEEP, stencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS }
defaultDepthStencilState :: SDLGPUDepthStencilState
defaultDepthStencilState = SDLGPUDepthStencilState { enableDepthTest = False, enableDepthWrite = False, depthStencilCompareOp = SDL_GPU_COMPAREOP_ALWAYS, enableStencilTest = False, backStencilState = defaultStencilOpState, frontStencilState = defaultStencilOpState, stencilCompareMask = 0xFF, stencilWriteMask = 0xFF }
defaultVertexInputState :: SDLGPUVertexInputState
defaultVertexInputState = SDLGPUVertexInputState { inputVertexBuffers = [], inputVertexAttribs = [] }        

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
commonQuit Context{..} = do -- Direct record destructuring
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
    bracket (commonInit exampleName windowFlags)
            (maybe (return ()) commonQuit)
            (mapM appAction)

-- | Loads shader code from a file located via Cabal's data-files mechanism.
loadShader :: SDLGPUDevice -> FilePath -> SDLGPUShaderStage -> SDLGPUShaderCreateInfo -> IO (Maybe SDLGPUShader)
loadShader device baseFilename stage baseCreateInfo = do
    supportedFormats <- sdlGetGPUShaderFormats device

    let formatsToTry =
            [ (SDL_GPU_SHADERFORMAT_SPIRV,  "SPIRV",  "spv", "main")
            , (SDL_GPU_SHADERFORMAT_MSL,    "MSL",    "msl", "main0")
            , (SDL_GPU_SHADERFORMAT_DXIL,   "DXIL",   "dxil","main")
            ]

    -- Helper to construct the relative path for getDataFileName
    let constructRelativePath fmtDir ext = "Content" </> "Shaders" </> "Compiled" </> fmtDir </> baseFilename ++ "." ++ ext
    cwd <- getCurrentDirectory
    putStrLn cwd
    -- Find first supported shader format where the *data file* exists
    maybeFound <- findM (\(fmt, fmtDir, ext, _) ->
                            if supportedFormats .&. fmt /= SDL_GPU_SHADERFORMAT_INVALID
                            then do
                                -- Get the absolute path from Cabal's data dir
                                let relativePath = constructRelativePath fmtDir ext
                                absolutePath <- getDataFileName relativePath -- Use Paths_sdl3 function
                                -- Check if the file exists at that absolute path
                                System.Directory.doesFileExist absolutePath
                            else return False
                        ) formatsToTry

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
            bracket (sdlLoadFile absolutePath) -- Load using the absolute path
                    (\case
                        Just (ptr, _) -> free ptr
                        Nothing -> return ()
                    )
                    (\case
                        Nothing -> do
                            sdlLog $ "SDL_LoadFile failed for: " ++ absolutePath
                            return Nothing
                        Just (codePtr, codeSize) -> do
                            sdlLog $ "Successfully loaded " ++ show codeSize ++ " bytes."
                            -- Populate shader creation info struct (override placeholders)
                            let createInfo = baseCreateInfo
                                    { shaderCode = castPtr codePtr
                                    , shaderCodeSize = codeSize
                                    , shaderEntryPoint = entryPoint
                                    , shaderFormat = format
                                    , shaderStage = stage
                                    -- Other fields like shaderNumSamplers remain from baseCreateInfo
                                    }
                            maybeShader <- sdlCreateGPUShader device createInfo
                            when (isNothing maybeShader) $ do
                                sdlLog $ "sdlCreateGPUShader failed for: " ++ absolutePath
                                sdlGetError >>= sdlLog . ("SDL Error: " ++)
                            return maybeShader
                    )

-- | Loads an image using SDL_LoadBMP and converts it to ABGR8888 format if necessary.
-- Mimics the C example's LoadImage helper.
-- The caller is responsible for calling SDL_DestroySurface on the returned pointer
-- when it's no longer needed. This function handles destroying the *intermediate*
-- original surface if a conversion occurs.
loadImage :: FilePath -- ^ Relative path within the data directory (e.g., "Content/Images/ravioli.bmp")
          -> IO (Maybe (Ptr SDLSurface)) -- ^ Returns pointer to the final surface (ABGR8888) or Nothing on failure.
loadImage relativeImagePath = do
    sdlLog $ "Attempting to load image: " ++ relativeImagePath
    fullPath <- getDataFileName relativeImagePath `catch` \(e :: IOException) -> do
        sdlLog $ "Error constructing image file path for '" ++ relativeImagePath ++ "': " ++ show e
        return ""

    -- Check if getDataFileName failed
    when (null fullPath) $ sdlLog "Failed to get absolute path using getDataFileName."
    if null fullPath then return Nothing else do

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
                if originalFormat == targetFormat then do
                    sdlLog $ "Surface already in target format (" ++ show targetFormat ++ "). Returning original pointer."
                    -- No conversion needed, return the pointer we loaded. Caller now owns it.
                    return $ Just originalSurfPtr
                else do
                    sdlLog $ "Original format (" ++ show originalFormat ++ ") differs from target (" ++ show targetFormat ++ "). Converting..."

                    -- Attempt conversion
                    eitherConverted <- try (sdlConvertSurface originalSurfPtr targetFormat)
                                        :: IO (Either SomeException (Maybe (Ptr SDLSurface)))

                    -- IMPORTANT: Destroy the original surface *after* the conversion attempt,
                    -- regardless of whether it succeeded or failed. We no longer need it.
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

-- | findM
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
  b <- p x
  if b then return (Just x) else findM p xs

-- Helper function to print subsystem names
printSubsystem :: SDLInitFlags -> IO ()
printSubsystem flag = sdlLog $ "  - " ++ case flag of
  SDL_INIT_AUDIO    -> "Audio"
  SDL_INIT_VIDEO    -> "Video"
  SDL_INIT_JOYSTICK -> "Joystick"
  SDL_INIT_HAPTIC   -> "Haptic"
  SDL_INIT_GAMEPAD  -> "Gamepad"
  SDL_INIT_EVENTS   -> "Events"
  SDL_INIT_SENSOR   -> "Sensor"
  SDL_INIT_CAMERA   -> "Camera"
  _            -> "Unknown subsystem"
