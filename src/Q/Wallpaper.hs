module Q.Wallpaper where

import Codec.Picture

data Dimensions = Dimensions {
  width :: Int,
  height :: Int,
  aspect :: Double
}

data Position = Position {
  dimensions :: Dimensions,
  pixelX :: Int,
  pixelY :: Int,
  u :: Double,
  v :: Double,
  x :: Double,
  y :: Double
}

mkDimensions :: Int -> Int -> Dimensions
mkDimensions width height = Dimensions { width, height, aspect }
  where
    aspect :: Double
    aspect = (fromIntegral width) / (fromIntegral height)

mkPosition :: Dimensions -> Int -> Int -> Position
mkPosition dimensions pixelX pixelY = Position { dimensions, pixelX, pixelY, u, v, x, y }
  where
    width' = width dimensions
    height' = height dimensions
    u :: Double
    u = (fromIntegral pixelX) / (fromIntegral width')
    v :: Double
    v = (fromIntegral pixelY) / (fromIntegral height')
    innerRadius :: Int
    innerRadius = div (min width' height') 2
    x :: Double
    x = (fromIntegral $ pixelX - (div width' 2)) / (fromIntegral innerRadius)
    y :: Double
    y = (fromIntegral $ pixelY - (div height' 2)) / (fromIntegral innerRadius)

color :: RealFrac a => a -> a -> a -> PixelRGB8
color r g b = PixelRGB8 (toWord r) (toWord g) (toWord b)
  where
    toWord = truncate . (* 255) . (max 0) . (min 1)

wallpaper :: Position -> PixelRGB8
wallpaper p = color (u p) 0 (v p)

wallpaperImage :: Image PixelRGB8
wallpaperImage = generateImage pixel width height
  where
    width :: Int
    width = 1920
    height :: Int
    height = 1080
    dimensions :: Dimensions
    dimensions = mkDimensions width height
    pixel :: Int -> Int -> PixelRGB8
    pixel x y = wallpaper $ mkPosition dimensions x y

generateWallpaper :: IO ()
generateWallpaper = writePng "/tmp/wallpaper.png" wallpaperImage