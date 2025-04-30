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
import Control.Exception (bracket)

import System.Exit (exitFailure)
import Paths_sdl3 (getDataFileName) -- Import the function from the auto-generated module
import System.FilePath ((</>))      -- For path manipulation (optional but clean)
import System.Directory (doesFileExist) -- Need this directly now

import Data.Maybe (fromMaybe, isNothing, isJust, fromJust)
import Data.Bits ((.|.), (.&.))
import Data.List (find)
import qualified Data.ByteString as BS

-- | Context structure
data Context = Context
    { contextDevice :: SDLGPUDevice
    , contextWindow :: SDLWindow
    } deriving (Show)

-- | commonInit
commonInit :: String -> [SDLWindowFlags] -> IO (Maybe Context)
commonInit exampleName windowFlags = do
    initSuccess <- sdlInit [InitVideo, InitEvents]
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

-- | findM
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
  b <- p x
  if b then return (Just x) else findM p xs
