{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module R3 where

#include <r3.h>
import Data.Word
import Foreign.C
import Foreign.Ptr

import Foreign.Marshal.Array (withArray)

data ClearBit
    = ClearBitColor
    | ClearBitDepth
    deriving (Show, Eq)

data Vert
    = Position
    | Color
    | Normal
    | TexCoord
    deriving (Show, Eq) 

foreign import ccall unsafe "r3_init" r3_init :: CString -> CInt -> CInt -> IO CUChar
foreign import ccall unsafe "r3_clear" r3_clear :: Ptr Float -> CInt -> IO ()
foreign import ccall unsafe "r3_render" render :: IO ()
foreign import ccall unsafe "r3_quit" quit :: IO ()
foreign import ccall unsafe "r3_viewport" viewport :: IO ()
foreign import ccall unsafe "r3_enable_tests" enable_tests :: IO ()

init :: String -> Int -> Int -> IO Bool
init title w h = withCString title $ \s -> do
    uc <- r3_init s (fromIntegral w) (fromIntegral h)
    return (uc /= 0)

clear :: IO ()
clear = withArray [0.2, 0.2, 0.3] $ \ptr -> do
    r3_clear ptr r3_CLEAR_BIT_COLOR

r3_CLEAR_BIT_DEPTH = 0x100
r3_CLEAR_BIT_STENCIL = 0x400
r3_CLEAR_BIT_COLOR = 0x4000
