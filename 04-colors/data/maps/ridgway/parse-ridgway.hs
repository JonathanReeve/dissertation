{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Codec.Picture
import Codec.Picture.Extra
import Data.Either
import Lucid
import PyF
import qualified Data.Vector.Storable as V
import GHC.Word
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Generic

testImage = "ridgway-58.jpg"

readImageFile :: FilePath -> IO (Image PixelRGB16)
readImageFile imageFile = do
  eitherImg <- readImage imageFile
  let img = case eitherImg of
              Left err -> error err
              Right img -> img
  let rgbImg = convertRGB16 img
  return rgbImg

main :: IO ()
main = do
  -- Parse command-line argument, and read the filename given
  -- by the first argument.
  fileName <- getRecord "Color word annotator."

  rgbImg <- readImageFile fileName

  let croppedChips = concatMap cropChips (cropRows rgbImg)
  let dominantColors = map (imageData . dominantColor) croppedChips

  renderToFile "color-test.html" $ scaffold dominantColors
  TIO.putStr $ T.intercalate "\n" $ map formatColor dominantColors
  -- saveImgs croppedChips
  -- saveImgs croppedImages

  -- print $ imageData (dominantColor rgbImg)

formatColor :: V.Vector Word8 -> T.Text
formatColor color = [fmt|rgb({colorList})|] where
                        colorList = T.intercalate "," $ map (T.pack . show) (V.toList color)

-- | Just make an HTML page that shows these things
scaffold :: [V.Vector Word8] -> Html ()
scaffold colors = do
  html_ $ body_ $ do
    mapM_ (\chip -> img_ [src_ chip])
      [ "chip-1.jpg", "chip-2.jpg", "chip-3.jpg" ]
    mapM_ colorBlock colors

-- | Make an HTML block that just shows the color
colorBlock :: V.Vector Word8 -> Html ()
colorBlock color =
    div_ [ style_ css ] "something" where
      css = T.concat ["width: 100; height: 100; background-color: rgb(", colorList, ")"]
      colorList = T.intercalate "," $ map (T.pack . show) (V.toList color)

-- | Enumerate and save a list of images.
saveImgs :: [Image PixelRGB16] -> IO ()
saveImgs imgs = mapM_ saveImg (zip [1..] imgs) where
  saveImg (i, img) = saveJpgImage 100 fn (ImageRGB16 img) where
    fn = "chip-" ++ show i ++ ".jpg"

-- | Convert an image into a list of cropped images,
-- all horizontal slices.
cropRows :: Pixel a => Image a -> [Image a]
cropRows img = map (cropIt img) yVals
  where yVals = [530, 750, 980, 1200, 1420, 1650, 1880]
        cropIt img y = crop 200 y 1300 220 img

-- | Crop out chips of size 240x100 from the horizontal slices
cropChips :: Pixel a => Image a -> [Image a]
cropChips img = map (cropIt img) [200, 560, 970]
  where cropIt img x = crop x 20 230 100 img

-- | Just a convenience wrapper around crop, which takes a tuple
-- instead of four integers.
cropToBox :: Pixel a => (Int, Int, Int, Int) -> Image a -> Image a
cropToBox (xstart, xend, ystart, yend) img =
  crop xstart xend ystart yend img

dominantColor :: Image PixelRGB16 -> Palette
dominantColor img = palette where
  (_, palette) = palettize paletteOptions img8 where
  paletteOptions = PaletteOptions MedianMeanCut True 1
  img8 = convertRGB8 (ImageRGB16 img)

