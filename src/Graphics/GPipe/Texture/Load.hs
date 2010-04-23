-----------------------------------------------------------------------------
--
-- Module      :  Graphics.GPipe.Texture.Load
-- Copyright   :  Tobias Bexelius
-- License     :  BSD3
--
-- Maintainer  :  Tobias Bexelius
-- Stability   :  Experimental
-- Portability :
--
-- |
-- This module provides the means to load all kinds of textures from file.
-- It's based on the stb-image package, and inherits its strengths and limitations.
-- Specifically, it handles a subset of the JPEG, PNG, TGA, BMP and PSD file formats, but is not
-- entirely thread safe. See <http://hackage.haskell.org/package/stb-image> for more information.
--
-----------------------------------------------------------------------------

module Graphics.GPipe.Texture.Load (
    LoadableTexture(..),
    LoadableTexture3D(..),
) where

import Graphics.GPipe
import Codec.Image.STB
import Data.Bitmap.IO (withBitmap)
import Data.Either
import Foreign.Ptr (plusPtr)

-- | Provides the general way of loading any kind of textures. A 3D texture is assumed to be an array of square images
-- tiled vertically in the image file. Cube textures are assumed to be composed of 6 equally sized images
-- tiled vertically. No additional mipmaps are loaded.
--
-- Filesystem errors or bad texture dimensions (e.g. loading a cube texture from a file where the height
-- is not a multiple of 6) are thrown as 'IOError's.
class Texture t => LoadableTexture t where
    loadTexture :: TextureFormat t
                -> FilePath
                -> IO t

-- | Provides an alternative way of loading 'Texture3D's that are arrays of non-square images tiled vertically. No additional mipmaps are loaded.
--
-- Filesystem errors or bad texture dimensions (i.e. the height of the image is not a multiple of the supplied depth)
-- are thrown as 'IOError's.
class LoadableTexture3D f where
    loadTexture3D :: Int -- ^ The depth of the resulting 'Texture3D'
                  -> f
                  -> FilePath
                  -> IO (Texture3D f)

loadTexture' comp io path = do image <- loadImage' path comp
                               either
                                  (ioError . userError)
                                  (flip withBitmap io)
                                  image

texture3DFromImage cpufmt fmt path s@(w,h) comp 0 ptr =
    case quotRem h w of
        (q, 0) -> newTexture cpufmt fmt (w:.w:.q:.()) [ptr]
        _      -> ioError $ userError ("loadTexture: Bad 3D image size " ++ show s ++ " in " ++ show path)
texture3DFromImage _ _ path _ _ _ _ = ioError $ userError ("loadTexture: Row padding is not supported, in " ++ show path)

texture2DFromImage cpufmt fmt path (w,h) comp 0 ptr = newTexture cpufmt fmt (w:.h:.()) [ptr]
texture2DFromImage _ _ path _ _ _ _ = ioError $ userError ("loadTexture: Row padding is not supported, in " ++ show path)

texture1DFromImage cpufmt fmt path (w,h) comp 0 ptr = newTexture cpufmt fmt (w*h) [ptr]
texture1DFromImage _ _ path _ _ _ _ = ioError $ userError ("loadTexture: Row padding is not supported, in " ++ show path)

textureCubeFromImage cpufmt fmt path s@(w,h) comp 0 ptr =
    case quotRem h 6 of
        (q, 0) -> newTexture cpufmt fmt (w:.q:.()) [ptr `plusPtr` (off*w*q) | off <- [0..5]]
        _      -> ioError $ userError ("loadTexture: Bad cube image size " ++ show s ++ " in " ++ show path)
textureCubeFromImage _ _ path _ _ _ _ = ioError $ userError ("loadTexture: Row padding is not supported, in " ++ show path)

texture3DFromImage' d cpufmt fmt path s@(w,h) comp 0 ptr =
    case quotRem h d of
        (q, 0) -> newTexture cpufmt fmt (w:.q:.d:.()) [ptr]
        _      -> ioError $ userError ("loadTexture: Bad 3D image size " ++ show s ++ " in " ++ show path)
texture3DFromImage' _ _ _ path _ _ _ _ = ioError $ userError ("loadTexture: Row padding is not supported, in " ++ show path)


instance LoadableTexture (Texture3D AlphaFormat) where
    loadTexture fmt path = loadTexture' 1 (texture3DFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (Texture3D LuminanceFormat) where
    loadTexture fmt path = loadTexture' 1 (texture3DFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (Texture3D LuminanceAlphaFormat) where
    loadTexture fmt path = loadTexture' 2 (texture3DFromImage (PerComp2 UnsignedByteFormat) fmt path) path
instance LoadableTexture (Texture3D RGBFormat) where
    loadTexture fmt path = loadTexture' 3 (texture3DFromImage (PerComp3 UnsignedByteFormat) fmt path) path
instance LoadableTexture (Texture3D RGBAFormat) where
    loadTexture fmt path = loadTexture' 4 (texture3DFromImage (PerComp4 UnsignedByteFormat) fmt path) path

instance LoadableTexture (Texture2D AlphaFormat) where
    loadTexture fmt path = loadTexture' 1 (texture2DFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (Texture2D LuminanceFormat) where
    loadTexture fmt path = loadTexture' 1 (texture2DFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (Texture2D LuminanceAlphaFormat) where
    loadTexture fmt path = loadTexture' 2 (texture2DFromImage (PerComp2 UnsignedByteFormat) fmt path) path
instance LoadableTexture (Texture2D RGBFormat) where
    loadTexture fmt path = loadTexture' 3 (texture2DFromImage (PerComp3 UnsignedByteFormat) fmt path) path
instance LoadableTexture (Texture2D RGBAFormat) where
    loadTexture fmt path = loadTexture' 4 (texture2DFromImage (PerComp4 UnsignedByteFormat) fmt path) path

instance LoadableTexture (Texture1D AlphaFormat) where
    loadTexture fmt path = loadTexture' 1 (texture1DFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (Texture1D LuminanceFormat) where
    loadTexture fmt path = loadTexture' 1 (texture1DFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (Texture1D LuminanceAlphaFormat) where
    loadTexture fmt path = loadTexture' 2 (texture1DFromImage (PerComp2 UnsignedByteFormat) fmt path) path
instance LoadableTexture (Texture1D RGBFormat) where
    loadTexture fmt path = loadTexture' 3 (texture1DFromImage (PerComp3 UnsignedByteFormat) fmt path) path
instance LoadableTexture (Texture1D RGBAFormat) where
    loadTexture fmt path = loadTexture' 4 (texture1DFromImage (PerComp4 UnsignedByteFormat) fmt path) path

instance LoadableTexture (TextureCube AlphaFormat) where
    loadTexture fmt path = loadTexture' 1 (textureCubeFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (TextureCube LuminanceFormat) where
    loadTexture fmt path = loadTexture' 1 (textureCubeFromImage UnsignedByteFormat fmt path) path
instance LoadableTexture (TextureCube LuminanceAlphaFormat) where
    loadTexture fmt path = loadTexture' 2 (textureCubeFromImage (PerComp2 UnsignedByteFormat) fmt path) path
instance LoadableTexture (TextureCube RGBFormat) where
    loadTexture fmt path = loadTexture' 3 (textureCubeFromImage (PerComp3 UnsignedByteFormat) fmt path) path
instance LoadableTexture (TextureCube RGBAFormat) where
    loadTexture fmt path = loadTexture' 4 (textureCubeFromImage (PerComp4 UnsignedByteFormat) fmt path) path

instance LoadableTexture3D AlphaFormat where
    loadTexture3D d fmt path = loadTexture' 1 (texture3DFromImage' d UnsignedByteFormat fmt path) path
instance LoadableTexture3D LuminanceFormat where
    loadTexture3D d fmt path = loadTexture' 1 (texture3DFromImage' d UnsignedByteFormat fmt path) path
instance LoadableTexture3D LuminanceAlphaFormat where
    loadTexture3D d fmt path = loadTexture' 2 (texture3DFromImage' d (PerComp2 UnsignedByteFormat) fmt path) path
instance LoadableTexture3D RGBFormat where
    loadTexture3D d fmt path = loadTexture' 3 (texture3DFromImage' d (PerComp3 UnsignedByteFormat) fmt path) path
instance LoadableTexture3D RGBAFormat where
    loadTexture3D d fmt path = loadTexture' 4 (texture3DFromImage' d (PerComp4 UnsignedByteFormat) fmt path) path
