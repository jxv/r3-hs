{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module R3
    ( ClearBit(..)
    , Vert(..)
    , TextureId
    , Mesh
    , init
    , clear
    , render
    , quit
    , viewport
    , enableTests
    , loadTgaBgrTexture
    , loadTgaRgbTexture
    ) where

#include <r3.h>
import Prelude hiding (init)
import Data.Word
import Foreign.C
import Foreign.Ptr

import Foreign.Marshal.Array (withArray)

data ClearBit
    = ClearBitColor
    | ClearBitDepth
    deriving (Show, Eq)

r3_CLEAR_BIT_DEPTH = 0x100
r3_CLEAR_BIT_STENCIL = 0x400
r3_CLEAR_BIT_COLOR = 0x4000

data Vert
    = Position
    | Color
    | Normal
    | TexCoord
    deriving (Show, Eq) 

r3_VERT_POSIION = 0
r3_VERT_COLOR = 1
r3_VERT_NORMAL = 2
r3_VERT_TEXCOORD = 3

newtype TextureId = TextureId { unTextureId :: Word32 }
    deriving (Show, Eq, Read, Enum)

data Mesh = Mesh {
    meshVert :: Vert,
    meshVBO :: Word32,
    meshIBO :: Word32,
    meshNumIndices :: CInt
}

foreign import ccall unsafe "r3_init" r3_init :: CString -> CInt -> CInt -> IO CUChar
foreign import ccall unsafe "r3_clear" r3_clear :: Ptr Float -> CInt -> IO ()
foreign import ccall unsafe "r3_render" render :: IO ()
foreign import ccall unsafe "r3_quit" quit :: IO ()
foreign import ccall unsafe "r3_viewport" viewport :: IO ()
foreign import ccall unsafe "r3_enable_tests" enableTests :: IO ()
foreign import ccall unsafe "r3_load_tga_bgr_texture" r3_load_tga_bgr_texture :: CString -> IO Word32
foreign import ccall unsafe "r3_load_tga_rgb_texture" r3_load_tga_rgb_texture :: CString -> IO Word32
foreign import ccall unsafe "r3_load_tga_bgr" r3_load_tga_bgr :: CString -> Ptr CInt -> Ptr CInt -> Ptr Word8
foreign import ccall unsafe "r3_load_tga_rgb" r3_load_tga_rgb :: CString -> Ptr CInt -> Ptr CInt -> Ptr Word8

init :: String -> Int -> Int -> IO Bool
init title w h = withCString title $ \s -> do
    uc <- r3_init s (fromIntegral w) (fromIntegral h)
    return (uc /= 0)

clear :: Float -> Float -> Float -> IO ()
clear r g b = withArray [r,g,b] $ \ptr -> do
    r3_clear ptr r3_CLEAR_BIT_COLOR

loadTgaBgrTexture :: String -> IO (Maybe TextureId)
loadTgaBgrTexture path = withCString path $ \s -> do
    texId <- r3_load_tga_bgr_texture s
    return $ if texId == 0
        then Just $ TextureId texId
        else Nothing

loadTgaRgbTexture :: String -> IO (Maybe TextureId)
loadTgaRgbTexture path = withCString path $ \s -> do
    texId <- r3_load_tga_rgb_texture s
    return $ if texId == 0
        then Just $ TextureId texId
        else Nothing

--cubeMesh :: Mesh
--cubeMesh = undefined
