module Main where

import Graphics.Image (Bilinear (Bilinear), Border (Edge), VU (VU), readImageRGB, writeImage)
import qualified Graphics.Image as GI
import Options.Applicative

data Options = Options
  { optCommand :: Command
  }

data Command
  = Resize ResizeOptions
  | ExportThumbnail ExportThumbnailOptions

data ResizeOptions = ResizeOptions
  { inputFile :: String,
    ouputFile :: String,
    width :: Int,
    height :: Int
  }

data ExportThumbnailOptions = ExportThumbnailOptions

options :: Parser Options
options =
  Options
    <$> subparser
      ( command "resize" (info resizeCommand (progDesc "resize image"))
          <> command "exportThumbnail" (info exportThumbnailCommand (progDesc "exportThumbnail"))
      )

resizeCommand :: Parser Command
resizeCommand =
  let resizeOptions =
        ResizeOptions
          <$> argument str (metavar "INPUT_FILE")
          <*> argument str (metavar "OUTPUT_FILE")
          <*> option
            auto
            ( long "width"
                <> short 'w'
                <> help "image width"
                <> showDefault
                <> value 400
                <> metavar "INT"
            )
          <*> option
            auto
            ( long "height"
                <> short 'h'
                <> help "image height"
                <> showDefault
                <> value 200
                <> metavar "INT"
            )
   in Resize <$> resizeOptions

exportThumbnailCommand :: Parser Command
exportThumbnailCommand = pure $ ExportThumbnail ExportThumbnailOptions

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "exec image processing commands"
            <> header "hanimg - exec image processing commands"
        )

run :: Options -> IO ()
run (Options (Resize resizeOptions)) = resize resizeOptions
run (Options (ExportThumbnail exportThumbnailOptions)) = exportThumbnail

resize :: ResizeOptions -> IO ()
resize (ResizeOptions inputFile outputFile width height) = do
  putStrLn "exec Resize command"
  inputImage <- readImageRGB VU inputFile
  let resizedImage = GI.resize Bilinear Edge (height, width) inputImage
  writeImage outputFile resizedImage

exportThumbnail :: IO ()
exportThumbnail = putStrLn "exec ExportThumbnail command"