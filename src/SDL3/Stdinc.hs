{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- SDL/Stdinc.hs

-- |
-- Module      : SDL.Stdinc
-- Description : Bindings to SDL stdinc functionality
-- Copyright   : (c) Kyle Lukaszek, 2025
-- License     : BSD3
--
-- This module provides Haskell bindings to the SDL stdinc functionality.
module SDL3.Stdinc
  ( -- * Types
    Sint8,
    Uint8,
    Sint16,
    Uint16,
    Sint32,
    Uint32,
    Sint64,
    Uint64,
    SDLTime,
    SDLBool,
    SDLCall,

    -- * Constants
    sdlSizeTMax,
    sdlMaxSint8,
    sdlMinSint8,
    sdlMaxUint8,
    sdlMinUint8,
    sdlMaxSint16,
    sdlMinSint16,
    sdlMaxUint16,
    sdlMinUint16,
    sdlMaxSint32,
    sdlMinSint32,
    sdlMaxUint32,
    sdlMinUint32,
    sdlMaxSint64,
    sdlMinSint64,
    sdlMaxUint64,
    sdlMinUint64,
    sdlMaxTime,
    sdlMinTime,
    sdlFltEpsilon,
    sdlInvalidUnicodeCodepoint,
    sdlTrue,
    sdlFalse,

    -- * Memory Management
    malloc,
    calloc,
    realloc,
    free,
    getMemoryFunctions,
    getOriginalMemoryFunctions,
    setMemoryFunctions,
    alignedAlloc,
    alignedFree,
    getNumAllocations,

    -- * Memory Operations
    memcpy,
    memmove,
    memset,
    memset4,
    memcmp,

    -- * String Operations
    strlen,
    strnlen,
    strlcpy,
    utf8strlcpy,
    strlcat,
    strdup,
    strndup,
    strrev,
    strupr,
    strlwr,
    strchr,
    strrchr,
    strstr,
    strnstr,
    strcasestr,
    strtokR,
    utf8strlen,
    utf8strnlen,
    itoa,
    uitoa,
    ltoa,
    ultoa,
    lltoa,
    ulltoa,
    atoi,
    atof,
    strtol,
    strtoul,
    strtoll,
    strtoull,
    strtod,
    strcmp,
    strncmp,
    strcasecmp,
    strncasecmp,
    strpbrk,
    sscanf,
    vsscanf,
    snprintf,
    vsnprintf,
    asprintf,
    vasprintf,

    -- * Wide String Operations
    wcslen,
    wcsnlen,
    wcslcpy,
    wcslcat,
    wcsdup,
    wcsstr,
    wcsnstr,
    wcscmp,
    wcsncmp,
    wcscasecmp,
    wcsncasecmp,
    wcstol,

    -- * Random Number Functions
    srand,
    rand,
    randf,
    randBits,
    randR,
    randfR,
    randBitsR,

    -- * Mathematical Functions
    abs,
    acos,
    acosf,
    asin,
    asinf,
    atan,
    atanf,
    atan2,
    atan2f,
    ceil,
    ceilf,
    copysign,
    copysignf,
    cos,
    cosf,
    exp,
    expf,
    fabs,
    fabsf,
    floor,
    floorf,
    trunc,
    truncf,
    fmod,
    fmodf,
    isinf,
    isinff,
    isnan,
    isnanf,
    log,
    logf,
    log10,
    log10f,
    modf,
    modff,
    pow,
    powf,
    round,
    roundf,
    lround,
    lroundf,
    scalbn,
    scalbnf,
    sin,
    sinf,
    sqrt,
    sqrtf,
    tan,
    tanf,

    -- * Character Classification
    isalpha,
    isalnum,
    isblank,
    iscntrl,
    isdigit,
    isxdigit,
    ispunct,
    isspace,
    isupper,
    islower,
    isprint,
    isgraph,
    toupper,
    tolower,

    -- * Data Integrity
    crc16,
    crc32,
    murmur332,

    -- * Unicode Utilities
    stepUTF8,
    stepBackUTF8,
    ucs4ToUTF8,

    -- * Environment Variables
    Environment,
    getEnvironment,
    createEnvironment,
    getEnvironmentVariable,
    getEnvironmentVariables,
    setEnvironmentVariable,
    unsetEnvironmentVariable,
    destroyEnvironment,
    getenv,
    getenvUnsafe,
    setenvUnsafe,
    unsetenvUnsafe,

    -- * Comparison Functions
    qsort,
    bsearch,
    qsortR,
    bsearchR,

    -- * Character Set Conversion
    IconvT,
    iconvOpen,
    iconvClose,
    iconv,
    iconvString,
    iconvUtf8Locale,
    iconvUtf8Ucs2,
    iconvUtf8Ucs4,
    iconvWcharUtf8,

    -- * Helper Functions and Macros
    sdlPiD,
    sdlPiF,
  )
where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (abs, acos, asin, atan, atan2, ceiling, cos, exp, floor, log, max, min, round, sin, sqrt, tan, truncate)

-- | 8-bit signed integer
type Sint8 = Int8

-- | 8-bit unsigned integer
type Uint8 = Word8

-- | 16-bit signed integer
type Sint16 = Int16

-- | 16-bit unsigned integer
type Uint16 = Word16

-- | 32-bit signed integer
type Sint32 = Int32

-- | 32-bit unsigned integer
type Uint32 = Word32

-- | 64-bit signed integer
type Sint64 = Int64

-- | 64-bit unsigned integer
type Uint64 = Word64

-- | SDL Time representation (signed 64-bit integer representing nanoseconds)
type SDLTime = Sint64

-- | SDL Boolean type
type SDLBool = CInt

-- | A generic function pointer, used for platform-specific thread functions
type SDLCall = Ptr ()

-- Constants
sdlSizeTMax :: Integer
sdlSizeTMax = 2 ^ (8 * sizeOf (undefined :: CSize)) - 1

sdlMaxSint8 :: Sint8
sdlMaxSint8 = 127

sdlMinSint8 :: Sint8
sdlMinSint8 = -128

sdlMaxUint8 :: Uint8
sdlMaxUint8 = 255

sdlMinUint8 :: Uint8
sdlMinUint8 = 0

sdlMaxSint16 :: Sint16
sdlMaxSint16 = 32767

sdlMinSint16 :: Sint16
sdlMinSint16 = -32768

sdlMaxUint16 :: Uint16
sdlMaxUint16 = 65535

sdlMinUint16 :: Uint16
sdlMinUint16 = 0

sdlMaxSint32 :: Sint32
sdlMaxSint32 = 2147483647

sdlMinSint32 :: Sint32
sdlMinSint32 = -2147483648

sdlMaxUint32 :: Uint32
sdlMaxUint32 = 4294967295

sdlMinUint32 :: Uint32
sdlMinUint32 = 0

sdlMaxSint64 :: Sint64
sdlMaxSint64 = 9223372036854775807

sdlMinSint64 :: Sint64
sdlMinSint64 = -9223372036854775808

sdlMaxUint64 :: Uint64
sdlMaxUint64 = 18446744073709551615

sdlMinUint64 :: Uint64
sdlMinUint64 = 0

sdlMaxTime :: SDLTime
sdlMaxTime = sdlMaxSint64

sdlMinTime :: SDLTime
sdlMinTime = sdlMinSint64

sdlFltEpsilon :: CFloat
sdlFltEpsilon = 1.1920928955078125e-07

sdlInvalidUnicodeCodepoint :: Word32
sdlInvalidUnicodeCodepoint = 0xFFFD

-- Mathematical constants
sdlPiD :: CDouble
sdlPiD = 3.141592653589793238462643383279502884

sdlPiF :: CFloat
sdlPiF = 3.141592653589793238462643383279502884

-- True / False
sdlTrue :: SDLBool
sdlTrue = 1

sdlFalse :: SDLBool
sdlFalse = 0

-- Memory Management
foreign import ccall unsafe "SDL_malloc" malloc :: CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_calloc" calloc :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_realloc" realloc :: Ptr a -> CSize -> IO (Ptr b)

foreign import ccall unsafe "SDL_free" free :: Ptr a -> IO ()

type MallocFunc = CSize -> IO (Ptr ())

type CallocFunc = CSize -> CSize -> IO (Ptr ())

type ReallocFunc = Ptr () -> CSize -> IO (Ptr ())

type FreeFunc = Ptr () -> IO ()

foreign import ccall unsafe "SDL_GetMemoryFunctions"
  getMemoryFunctions :: Ptr (FunPtr MallocFunc) -> Ptr (FunPtr CallocFunc) -> Ptr (FunPtr ReallocFunc) -> Ptr (FunPtr FreeFunc) -> IO ()

foreign import ccall unsafe "SDL_GetOriginalMemoryFunctions"
  getOriginalMemoryFunctions :: Ptr (FunPtr MallocFunc) -> Ptr (FunPtr CallocFunc) -> Ptr (FunPtr ReallocFunc) -> Ptr (FunPtr FreeFunc) -> IO ()

foreign import ccall unsafe "SDL_SetMemoryFunctions"
  setMemoryFunctions :: FunPtr MallocFunc -> FunPtr CallocFunc -> FunPtr ReallocFunc -> FunPtr FreeFunc -> IO SDLBool

foreign import ccall unsafe "SDL_aligned_alloc" alignedAlloc :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_aligned_free" alignedFree :: Ptr a -> IO ()

foreign import ccall unsafe "SDL_GetNumAllocations" getNumAllocations :: IO CInt

-- Memory Operations
foreign import ccall unsafe "SDL_memcpy" memcpy :: Ptr a -> Ptr b -> CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_memmove" memmove :: Ptr a -> Ptr b -> CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_memset" memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_memset4" memset4 :: Ptr a -> Uint32 -> CSize -> IO (Ptr a)

foreign import ccall unsafe "SDL_memcmp" memcmp :: Ptr a -> Ptr b -> CSize -> IO CInt

-- Environment
data Environment

foreign import ccall unsafe "SDL_GetEnvironment" getEnvironment :: IO (Ptr Environment)

foreign import ccall unsafe "SDL_CreateEnvironment" createEnvironment :: SDLBool -> IO (Ptr Environment)

foreign import ccall unsafe "SDL_GetEnvironmentVariable" getEnvironmentVariable :: Ptr Environment -> CString -> IO CString

foreign import ccall unsafe "SDL_GetEnvironmentVariables" getEnvironmentVariables :: Ptr Environment -> IO (Ptr CString)

foreign import ccall unsafe "SDL_SetEnvironmentVariable" setEnvironmentVariable :: Ptr Environment -> CString -> CString -> SDLBool -> IO SDLBool

foreign import ccall unsafe "SDL_UnsetEnvironmentVariable" unsetEnvironmentVariable :: Ptr Environment -> CString -> IO SDLBool

foreign import ccall unsafe "SDL_DestroyEnvironment" destroyEnvironment :: Ptr Environment -> IO ()

foreign import ccall unsafe "SDL_getenv" getenv :: CString -> IO CString

foreign import ccall unsafe "SDL_getenv_unsafe" getenvUnsafe :: CString -> IO CString

foreign import ccall unsafe "SDL_setenv_unsafe" setenvUnsafe :: CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "SDL_unsetenv_unsafe" unsetenvUnsafe :: CString -> IO CInt

-- Comparison Functions
type CompareCallback a b = Ptr a -> Ptr b -> IO CInt

type CompareCallbackR a b = Ptr () -> Ptr a -> Ptr b -> IO CInt

foreign import ccall unsafe "SDL_qsort"
  qsort :: Ptr a -> CSize -> CSize -> FunPtr (CompareCallback a a) -> IO ()

foreign import ccall unsafe "SDL_bsearch"
  bsearch :: Ptr a -> Ptr b -> CSize -> CSize -> FunPtr (CompareCallback a b) -> IO (Ptr ())

foreign import ccall unsafe "SDL_qsort_r"
  qsortR :: Ptr a -> CSize -> CSize -> FunPtr (CompareCallbackR a a) -> Ptr () -> IO ()

foreign import ccall unsafe "SDL_bsearch_r"
  bsearchR :: Ptr a -> Ptr b -> CSize -> CSize -> FunPtr (CompareCallbackR a b) -> Ptr () -> IO (Ptr ())

-- String Operations
foreign import ccall unsafe "SDL_strlen" strlen :: CString -> IO CSize

foreign import ccall unsafe "SDL_strnlen" strnlen :: CString -> CSize -> IO CSize

foreign import ccall unsafe "SDL_strlcpy" strlcpy :: CString -> CString -> CSize -> IO CSize

foreign import ccall unsafe "SDL_utf8strlcpy" utf8strlcpy :: CString -> CString -> CSize -> IO CSize

foreign import ccall unsafe "SDL_strlcat" strlcat :: CString -> CString -> CSize -> IO CSize

foreign import ccall unsafe "SDL_strdup" strdup :: CString -> IO CString

foreign import ccall unsafe "SDL_strndup" strndup :: CString -> CSize -> IO CString

foreign import ccall unsafe "SDL_strrev" strrev :: CString -> IO CString

foreign import ccall unsafe "SDL_strupr" strupr :: CString -> IO CString

foreign import ccall unsafe "SDL_strlwr" strlwr :: CString -> IO CString

foreign import ccall unsafe "SDL_strchr" strchr :: CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_strrchr" strrchr :: CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_strstr" strstr :: CString -> CString -> IO CString

foreign import ccall unsafe "SDL_strnstr" strnstr :: CString -> CString -> CSize -> IO CString

foreign import ccall unsafe "SDL_strcasestr" strcasestr :: CString -> CString -> IO CString

foreign import ccall unsafe "SDL_strtok_r" strtokR :: CString -> CString -> Ptr CString -> IO CString

foreign import ccall unsafe "SDL_utf8strlen" utf8strlen :: CString -> IO CSize

foreign import ccall unsafe "SDL_utf8strnlen" utf8strnlen :: CString -> CSize -> IO CSize

-- String Conversion
foreign import ccall unsafe "SDL_itoa" itoa :: CInt -> CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_uitoa" uitoa :: CUInt -> CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_ltoa" ltoa :: CLong -> CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_ultoa" ultoa :: CULong -> CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_lltoa" lltoa :: CLLong -> CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_ulltoa" ulltoa :: CULLong -> CString -> CInt -> IO CString

foreign import ccall unsafe "SDL_atoi" atoi :: CString -> IO CInt

foreign import ccall unsafe "SDL_atof" atof :: CString -> IO CDouble

foreign import ccall unsafe "SDL_strtol" strtol :: CString -> Ptr CString -> CInt -> IO CLong

foreign import ccall unsafe "SDL_strtoul" strtoul :: CString -> Ptr CString -> CInt -> IO CULong

foreign import ccall unsafe "SDL_strtoll" strtoll :: CString -> Ptr CString -> CInt -> IO CLLong

foreign import ccall unsafe "SDL_strtoull" strtoull :: CString -> Ptr CString -> CInt -> IO CULLong

foreign import ccall unsafe "SDL_strtod" strtod :: CString -> Ptr CString -> IO CDouble

-- String Comparison
foreign import ccall unsafe "SDL_strcmp" strcmp :: CString -> CString -> IO CInt

foreign import ccall unsafe "SDL_strncmp" strncmp :: CString -> CString -> CSize -> IO CInt

foreign import ccall unsafe "SDL_strcasecmp" strcasecmp :: CString -> CString -> IO CInt

foreign import ccall unsafe "SDL_strncasecmp" strncasecmp :: CString -> CString -> CSize -> IO CInt

foreign import ccall unsafe "SDL_strpbrk" strpbrk :: CString -> CString -> IO CString

-- Wide String Operations
foreign import ccall unsafe "SDL_wcslen" wcslen :: Ptr CWchar -> IO CSize

foreign import ccall unsafe "SDL_wcsnlen" wcsnlen :: Ptr CWchar -> CSize -> IO CSize

foreign import ccall unsafe "SDL_wcslcpy" wcslcpy :: Ptr CWchar -> Ptr CWchar -> CSize -> IO CSize

foreign import ccall unsafe "SDL_wcslcat" wcslcat :: Ptr CWchar -> Ptr CWchar -> CSize -> IO CSize

foreign import ccall unsafe "SDL_wcsdup" wcsdup :: Ptr CWchar -> IO (Ptr CWchar)

foreign import ccall unsafe "SDL_wcsstr" wcsstr :: Ptr CWchar -> Ptr CWchar -> IO (Ptr CWchar)

foreign import ccall unsafe "SDL_wcsnstr" wcsnstr :: Ptr CWchar -> Ptr CWchar -> CSize -> IO (Ptr CWchar)

foreign import ccall unsafe "SDL_wcscmp" wcscmp :: Ptr CWchar -> Ptr CWchar -> IO CInt

foreign import ccall unsafe "SDL_wcsncmp" wcsncmp :: Ptr CWchar -> Ptr CWchar -> CSize -> IO CInt

foreign import ccall unsafe "SDL_wcscasecmp" wcscasecmp :: Ptr CWchar -> Ptr CWchar -> IO CInt

foreign import ccall unsafe "SDL_wcsncasecmp" wcsncasecmp :: Ptr CWchar -> Ptr CWchar -> CSize -> IO CInt

foreign import ccall unsafe "SDL_wcstol" wcstol :: Ptr CWchar -> Ptr (Ptr CWchar) -> CInt -> IO CLong

-- Wide-char formatting wrappers for Haskell FFI
foreign import ccall unsafe "wrapper_SDL_swprintf"
  sdlSwprintf :: Ptr CWchar -> CSize -> Ptr CWchar -> IO CInt

foreign import ccall unsafe "wrapper_SDL_vswprintf"
  sdlVswprintf :: Ptr CWchar -> CSize -> Ptr CWchar -> Ptr () -> IO CInt

-- Formatting
foreign import ccall unsafe "SDL_sscanf" sscanf :: CString -> CString -> IO CInt

foreign import ccall unsafe "SDL_vsscanf" vsscanf :: CString -> CString -> Ptr () -> IO CInt

foreign import ccall unsafe "SDL_snprintf" snprintf :: CString -> CSize -> CString -> IO CInt

foreign import ccall unsafe "SDL_vsnprintf" vsnprintf :: CString -> CSize -> CString -> Ptr () -> IO CInt

foreign import ccall unsafe "SDL_asprintf" asprintf :: Ptr CString -> CString -> IO CInt

foreign import ccall unsafe "SDL_vasprintf" vasprintf :: Ptr CString -> CString -> Ptr () -> IO CInt

-- Random Number Generation
foreign import ccall unsafe "SDL_srand" srand :: Uint64 -> IO ()

foreign import ccall unsafe "SDL_rand" rand :: Sint32 -> IO Sint32

foreign import ccall unsafe "SDL_randf" randf :: IO CFloat

foreign import ccall unsafe "SDL_rand_bits" randBits :: IO Uint32

foreign import ccall unsafe "SDL_rand_r" randR :: Ptr Uint64 -> Sint32 -> IO Sint32

foreign import ccall unsafe "SDL_randf_r" randfR :: Ptr Uint64 -> IO CFloat

foreign import ccall unsafe "SDL_rand_bits_r" randBitsR :: Ptr Uint64 -> IO Uint32

-- Mathematical Functions
foreign import ccall unsafe "SDL_abs" abs :: CInt -> IO CInt

foreign import ccall unsafe "SDL_acos" acos :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_acosf" acosf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_asin" asin :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_asinf" asinf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_atan" atan :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_atanf" atanf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_atan2" atan2 :: CDouble -> CDouble -> IO CDouble

foreign import ccall unsafe "SDL_atan2f" atan2f :: CFloat -> CFloat -> IO CFloat

foreign import ccall unsafe "SDL_ceil" ceil :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_ceilf" ceilf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_copysign" copysign :: CDouble -> CDouble -> IO CDouble

foreign import ccall unsafe "SDL_copysignf" copysignf :: CFloat -> CFloat -> IO CFloat

foreign import ccall unsafe "SDL_cos" cos :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_cosf" cosf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_exp" exp :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_expf" expf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_fabs" fabs :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_fabsf" fabsf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_floor" floor :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_floorf" floorf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_trunc" trunc :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_truncf" truncf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_fmod" fmod :: CDouble -> CDouble -> IO CDouble

foreign import ccall unsafe "SDL_fmodf" fmodf :: CFloat -> CFloat -> IO CFloat

foreign import ccall unsafe "SDL_isinf" isinf :: CDouble -> IO CInt

foreign import ccall unsafe "SDL_isinff" isinff :: CFloat -> IO CInt

foreign import ccall unsafe "SDL_isnan" isnan :: CDouble -> IO CInt

foreign import ccall unsafe "SDL_isnanf" isnanf :: CFloat -> IO CInt

foreign import ccall unsafe "SDL_log" log :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_logf" logf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_log10" log10 :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_log10f" log10f :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_modf" modf :: CDouble -> Ptr CDouble -> IO CDouble

foreign import ccall unsafe "SDL_modff" modff :: CFloat -> Ptr CFloat -> IO CFloat

foreign import ccall unsafe "SDL_pow" pow :: CDouble -> CDouble -> IO CDouble

foreign import ccall unsafe "SDL_powf" powf :: CFloat -> CFloat -> IO CFloat

foreign import ccall unsafe "SDL_round" round :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_roundf" roundf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_lround" lround :: CDouble -> IO CLong

foreign import ccall unsafe "SDL_lroundf" lroundf :: CFloat -> IO CLong

foreign import ccall unsafe "SDL_scalbn" scalbn :: CDouble -> CInt -> IO CDouble

foreign import ccall unsafe "SDL_scalbnf" scalbnf :: CFloat -> CInt -> IO CFloat

foreign import ccall unsafe "SDL_sin" sin :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_sinf" sinf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_sqrt" sqrt :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_sqrtf" sqrtf :: CFloat -> IO CFloat

foreign import ccall unsafe "SDL_tan" tan :: CDouble -> IO CDouble

foreign import ccall unsafe "SDL_tanf" tanf :: CFloat -> IO CFloat

-- Character Classification
foreign import ccall unsafe "SDL_isalpha" isalpha :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isalnum" isalnum :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isblank" isblank :: CInt -> IO CInt

foreign import ccall unsafe "SDL_iscntrl" iscntrl :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isdigit" isdigit :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isxdigit" isxdigit :: CInt -> IO CInt

foreign import ccall unsafe "SDL_ispunct" ispunct :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isspace" isspace :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isupper" isupper :: CInt -> IO CInt

foreign import ccall unsafe "SDL_islower" islower :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isprint" isprint :: CInt -> IO CInt

foreign import ccall unsafe "SDL_isgraph" isgraph :: CInt -> IO CInt

foreign import ccall unsafe "SDL_toupper" toupper :: CInt -> IO CInt

foreign import ccall unsafe "SDL_tolower" tolower :: CInt -> IO CInt

-- CRC Functions
foreign import ccall unsafe "SDL_crc16" crc16 :: Uint16 -> Ptr () -> CSize -> IO Uint16

foreign import ccall unsafe "SDL_crc32" crc32 :: Uint32 -> Ptr () -> CSize -> IO Uint32

foreign import ccall unsafe "SDL_murmur3_32" murmur332 :: Ptr () -> CSize -> Uint32 -> IO Uint32

-- Unicode Utilities
foreign import ccall unsafe "SDL_StepUTF8" stepUTF8 :: Ptr (Ptr CChar) -> Ptr CSize -> IO Uint32

foreign import ccall unsafe "SDL_StepBackUTF8" stepBackUTF8 :: Ptr CChar -> Ptr (Ptr CChar) -> IO Uint32

foreign import ccall unsafe "SDL_UCS4ToUTF8" ucs4ToUTF8 :: Uint32 -> Ptr CChar -> IO (Ptr CChar)

-- Character Set Conversion
data IconvData

type IconvT = Ptr IconvData

foreign import ccall unsafe "SDL_iconv_open" iconvOpen :: CString -> CString -> IO IconvT

foreign import ccall unsafe "SDL_iconv_close" iconvClose :: IconvT -> IO CInt

foreign import ccall unsafe "SDL_iconv" iconv :: IconvT -> Ptr (Ptr CChar) -> Ptr CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CSize

foreign import ccall unsafe "SDL_iconv_string" iconvString :: CString -> CString -> CString -> CSize -> IO CString

-- Helper functions for common conversion cases
iconvUtf8Locale :: CString -> IO CString
iconvUtf8Locale str = do
  len <- strlen str
  withCAString "" $ \emptyStr ->
    withCAString "UTF-8" $ \utf8 ->
      iconvString emptyStr utf8 str (len + 1)

iconvUtf8Ucs2 :: CString -> IO (Ptr Uint16)
iconvUtf8Ucs2 str = do
  len <- strlen str
  withCAString "UCS-2" $ \ucs2 ->
    withCAString "UTF-8" $ \utf8 ->
      do
        result <- iconvString ucs2 utf8 str (len + 1)
        return (castPtr result)

iconvUtf8Ucs4 :: CString -> IO (Ptr Uint32)
iconvUtf8Ucs4 str = do
  len <- strlen str
  withCAString "UCS-4" $ \ucs4 ->
    withCAString "UTF-8" $ \utf8 ->
      do
        result <- iconvString ucs4 utf8 str (len + 1)
        return (castPtr result)

iconvWcharUtf8 :: Ptr CWchar -> IO CString
iconvWcharUtf8 str = do
  len <- wcslen str
  withCAString "UTF-8" $ \utf8 ->
    withCAString "WCHAR_T" $ \wcharT ->
      iconvString utf8 wcharT (castPtr str) ((len + 1) * fromIntegral (sizeOf (undefined :: CWchar)))
