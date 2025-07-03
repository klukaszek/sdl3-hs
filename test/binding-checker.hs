{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (IOException, catch)
import Control.Monad (filterM, forM_, unless, when)
import Data.Char (toLower, toUpper)
import Data.List (find, isInfixOf, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (replaceExtension, takeBaseName, (</>))
import System.IO (hIsTerminalDevice, stdin)
import System.Process (readProcess)
import Text.Regex.Posix ((=~))

-- | Represents a C function declaration
data CFunction = CFunction
  { cfName :: String,
    cfReturnType :: String,
    cfParams :: [String],
    cfFullDecl :: String
  }
  deriving (Show, Eq)

-- | Represents a Haskell foreign import
data HsFunction = HsFunction
  { hfName :: String,
    hfCName :: String,
    hfType :: String,
    hfFullDecl :: String
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  isTerminal <- hIsTerminalDevice stdin

  case (args, isTerminal) of
    ([], True) -> checkAllBindings
    ([], False) -> checkPipedHeaders
    (["--help"], _) -> printHelp
    ([headerPath], _) -> checkSingleHeader headerPath
    _ -> putStrLn "Usage: binding-checker [SDL_header.h] or binding-checker --help"

printHelp :: IO ()
printHelp = do
  putStrLn "SDL3 Binding Checker"
  putStrLn "==================="
  putStrLn ""
  putStrLn "This tool compares SDL3 C header files with Haskell bindings to detect"
  putStrLn "missing or mismatched function declarations."
  putStrLn ""
  putStrLn "RECOMMENDED: Use the smart helper script for better experience:"
  putStrLn "  ./check-sdl-bindings --help        Show smart helper options"
  putStrLn "  ./check-sdl-bindings -i             Interactive header selection"
  putStrLn "  ./check-sdl-bindings -c GPU         Find headers with GPU functions"
  putStrLn "  ./check-sdl-bindings audio video    Check specific header patterns"
  putStrLn ""
  putStrLn "Direct Usage:"
  putStrLn "  binding-checker                    Check all SDL3 headers"
  putStrLn "  binding-checker SDL_header.h       Check specific header"
  putStrLn "  find /path -name '*.h' | binding-checker  Check piped headers"
  putStrLn "  binding-checker --help             Show this help"
  putStrLn ""
  putStrLn "Modern Tool Examples:"
  putStrLn "  fd 'SDL_.*\\.h$' /usr/local/include/SDL3 | binding-checker"
  putStrLn "  rg --files-with-matches 'SDLCALL.*Audio' /usr/local/include/SDL3/ | binding-checker"
  putStrLn "  find /usr/local/include/SDL3 -name 'SDL_*.h' | fzf --multi | binding-checker"
  putStrLn ""
  putStrLn "Output:"
  putStrLn "  - Creates 'broken/' directory with .txt files for each broken binding"
  putStrLn "  - Each file contains missing/mismatched functions"

checkAllBindings :: IO ()
checkAllBindings = do
  putStrLn "Checking all SDL3 bindings..."

  -- Create broken directory
  createDirectoryIfMissing False "broken"

  -- Find SDL3 headers
  headers <- findSDL3Headers

  if null headers
    then putStrLn "No SDL3 headers found. Please install SDL3 development files."
    else do
      putStrLn $ "Found " ++ show (length headers) ++ " SDL3 headers"

      totalBroken <- sum <$> mapM checkHeader headers

      putStrLn $ "\nSummary: " ++ show totalBroken ++ " total broken bindings found"
      when (totalBroken > 0) $
        putStrLn "Check the 'broken/' directory for details"

checkPipedHeaders :: IO ()
checkPipedHeaders = do
  putStrLn "Reading header paths from stdin..."

  -- Create broken directory
  createDirectoryIfMissing False "broken"

  -- Read header paths from stdin
  content <- getContents
  let headers = filter (not . null) $ map trim $ lines content
      sdlHeaders = filter isSDL3Header headers

  if null sdlHeaders
    then putStrLn "No SDL3 headers found in piped input."
    else do
      putStrLn $ "Found " ++ show (length sdlHeaders) ++ " SDL3 headers from piped input"

      totalBroken <- sum <$> mapM checkHeader sdlHeaders

      putStrLn $ "\nSummary: " ++ show totalBroken ++ " total broken bindings found"
      when (totalBroken > 0) $
        putStrLn "Check the 'broken/' directory for details"
  where
    isSDL3Header :: String -> Bool
    isSDL3Header path = "SDL" `isPrefixOf` takeBaseName path && ".h" `isInfixOf` path

checkSingleHeader :: String -> IO ()
checkSingleHeader headerPath = do
  exists <- doesFileExist headerPath
  if not exists
    then putStrLn $ "Header file not found: " ++ headerPath
    else do
      createDirectoryIfMissing False "broken"
      brokenCount <- checkHeader headerPath
      if brokenCount > 0
        then putStrLn $ "Found " ++ show brokenCount ++ " broken bindings"
        else putStrLn "No broken bindings found"

findSDL3Headers :: IO [String]
findSDL3Headers = do
  -- Try common SDL3 installation paths
  let searchPaths =
        [ "/usr/local/include/SDL3",
          "/usr/include/SDL3",
          "/opt/homebrew/include/SDL3"
        ]

  headers <- concat <$> mapM findHeadersInPath searchPaths
  return $ filter isSDL3Header headers
  where
    findHeadersInPath path = do
      contents <- (listDirectory path) `catch` (\(_ :: IOException) -> return [])
      return $ map (path </>) $ filter (\f -> ".h" `isInfixOf` f && "SDL" `isPrefixOf` f) contents

    isSDL3Header path = "SDL" `isPrefixOf` takeBaseName path

checkHeader :: String -> IO Int
checkHeader headerPath = do
  putStrLn $ "Checking " ++ takeBaseName headerPath ++ "..."

  -- Extract C functions from header
  cFunctions <- extractCFunctions headerPath

  -- Find corresponding Haskell binding file
  bindingFile <- findBindingFile headerPath

  case bindingFile of
    Nothing -> do
      putStrLn $ "  No binding file found for " ++ takeBaseName headerPath
      return 0
    Just haskellFile -> do
      -- Extract Haskell foreign imports
      hsFunctions <- extractHsFunctions haskellFile

      -- Compare and find broken bindings
      let broken = findBrokenBindings cFunctions hsFunctions

      if null broken
        then putStrLn $ "  ✓ All bindings OK"
        else do
          putStrLn $ "  ✗ Found " ++ show (length broken) ++ " broken bindings"
          writeBrokenReport headerPath broken

      return $ length broken

extractCFunctions :: String -> IO [CFunction]
extractCFunctions headerPath = do
  content <- readFile headerPath
  let lines' = lines content
      functionLines = filter isFunctionDecl lines'
  return $ mapMaybe parseCFunction functionLines
  where
    isFunctionDecl line =
      "SDL_DECLSPEC" `isInfixOf` line
        && "SDLCALL" `isInfixOf` line
        && not ("typedef" `isInfixOf` line)

parseCFunction :: String -> Maybe CFunction
parseCFunction line
  | "SDL_DECLSPEC" `isInfixOf` line && "SDLCALL" `isInfixOf` line =
      case parseCDeclaration line of
        Just (retType, name, params) ->
          Just $ CFunction name retType params line
        Nothing -> Nothing
  | otherwise = Nothing
  where
    parseCDeclaration :: String -> Maybe (String, String, [String])
    parseCDeclaration l =
      case findFunctionParts l of
        Just (beforeName, nameAndRest) ->
          case break (== '(') nameAndRest of
            (name, '(' : paramsPart) ->
              case break (== ')') paramsPart of
                (paramsStr, ')' : _) ->
                  let retType = extractReturnType beforeName
                      params = parseParams paramsStr
                   in Just (retType, trim name, params)
                _ -> Nothing
            _ -> Nothing
        Nothing -> Nothing

    findFunctionParts :: String -> Maybe (String, String)
    findFunctionParts l =
      case findSDLCall l of
        Just afterSDLCall -> Just (takeWhile (/= 'S') l, afterSDLCall)
        Nothing -> Nothing

    findSDLCall :: String -> Maybe String
    findSDLCall l = findSDLCALLInString l
      where
        findSDLCALLInString :: String -> Maybe String
        findSDLCALLInString [] = Nothing
        findSDLCALLInString s@(c : cs)
          | "SDLCALL" `isPrefixOf` s = Just (drop 7 s)
          | otherwise = findSDLCALLInString cs

    extractReturnType :: String -> String
    extractReturnType s =
      let afterDeclspec = case dropWhile (/= 'S') s of
            declspec | "SDL_DECLSPEC" `isPrefixOf` declspec -> drop 12 declspec -- "SDL_DECLSPEC" length is 12
            _ -> s
       in trim afterDeclspec

    parseParams :: String -> [String]
    parseParams paramStr =
      if trim paramStr == "void" || null (trim paramStr)
        then []
        else map trim $ splitOn ',' paramStr

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s = case break (== c) s of
  (chunk, []) -> [chunk]
  (chunk, _ : rest) -> chunk : splitOn c rest

findBindingFile :: String -> IO (Maybe String)
findBindingFile headerPath = do
  let baseName = takeBaseName headerPath
      -- Remove "SDL_" prefix if present
      moduleName = case stripPrefix "SDL_" baseName of
        Just name -> name
        Nothing -> baseName

      -- Capitalize first letter for module name and handle underscores
      capitalizedName = case moduleName of
        (c : cs) -> toUpper c : cs
        [] -> ""

      -- Also try a version with underscores removed and proper camelCase
      camelCaseName = toCamelCase moduleName

      possiblePaths =
        [ "src/SDL/" ++ capitalizedName ++ ".hsc",
          "src/SDL/" ++ capitalizedName ++ ".hs",
          "src/SDL/" ++ camelCaseName ++ ".hsc",
          "src/SDL/" ++ camelCaseName ++ ".hs",
          "src/SDL/" ++ baseName ++ ".hsc",
          "src/SDL/" ++ baseName ++ ".hs"
        ]

  existing <- filterM doesFileExist possiblePaths
  case existing of
    (path : _) -> return $ Just path
    [] -> return Nothing
  where
    -- Convert snake_case to CamelCase
    toCamelCase :: String -> String
    toCamelCase s = concatMap capitalizeWord (splitOn '_' s)

    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (c : cs) = toUpper c : cs

extractHsFunctions :: String -> IO [HsFunction]
extractHsFunctions haskellFile = do
  content <- readFile haskellFile
  let foreignBlocks = extractForeignImportBlocks content
  return $ mapMaybe parseHsFunction foreignBlocks
  where
    -- Extract multi-line foreign import blocks
    extractForeignImportBlocks :: String -> [String]
    extractForeignImportBlocks content =
      let lines' = lines content
          numberedLines = zip [1 ..] lines'
          foreignStarts = [n | (n, line) <- numberedLines, "foreign import ccall" `isInfixOf` line]
       in map (getForeignBlock lines') foreignStarts

    getForeignBlock :: [String] -> Int -> String
    getForeignBlock lines' startLine =
      let relevantLines = drop (startLine - 1) lines'
          block = takeWhile (\line -> not (null (trim line)) || "foreign import ccall" `isInfixOf` line) relevantLines
       in unwords $ map trim $ filter (not . null . trim) block

parseHsFunction :: String -> Maybe HsFunction
parseHsFunction block
  | "foreign import ccall" `isInfixOf` block =
      case parseHsDeclaration block of
        Just (cName, hsName) -> Just $ HsFunction hsName cName "" block
        Nothing -> Nothing
  | otherwise = Nothing
  where
    parseHsDeclaration :: String -> Maybe (String, String)
    parseHsDeclaration l =
      case findQuotedString l of
        Just cName ->
          case findHsName l of
            Just hsName -> Just (cName, hsName)
            Nothing -> Nothing
        Nothing -> Nothing

    findQuotedString :: String -> Maybe String
    findQuotedString l =
      case dropWhile (/= '"') l of
        ('"' : rest) ->
          case break (== '"') rest of
            (quoted, '"' : _) -> Just quoted
            _ -> Nothing
        _ -> Nothing

    findHsName :: String -> Maybe String
    findHsName l =
      -- Look for the function name after the quoted string and before ::
      let afterQuote = dropWhile (/= '"') l
          afterSecondQuote = case afterQuote of
            ('"' : rest) -> case dropWhile (/= '"') rest of
              ('"' : final) -> final
              _ -> ""
            _ -> ""
          trimmed = dropWhile (`elem` (" \t\n\r" :: String)) afterSecondQuote
          name = takeWhile (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= ':') trimmed
       in if null name then Nothing else Just name

findBrokenBindings :: [CFunction] -> [HsFunction] -> [String]
findBrokenBindings cFunctions hsFunctions =
  let cNames = map cfName cFunctions
      hsNames = map hfCName hsFunctions
      missing = filter (`notElem` hsNames) cNames
   in map formatBrokenBinding missing
  where
    formatBrokenBinding name =
      case find (\cf -> cfName cf == name) cFunctions of
        Just cf -> name ++ " " ++ unwords (cfParams cf)
        Nothing -> name

writeBrokenReport :: String -> [String] -> IO ()
writeBrokenReport headerPath broken = do
  let baseName = takeBaseName headerPath
      reportFile = "broken" </> baseName ++ ".txt"

  writeFile reportFile $
    unlines $
      [ "Broken bindings for " ++ baseName,
        "Generated by binding-checker",
        ""
      ]
        ++ broken

-- Utility functions
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` (" \t\n\r" :: String))
