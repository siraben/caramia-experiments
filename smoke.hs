{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main ( main ) where

import Control.Exception
import Data.Bits
import qualified Data.Text as T
import Foreign.C.String
import Foreign.C.Types
import Graphics.Caramia.Prelude hiding ( init )
import Graphics.Caramia
--import Graphics.UI.SDL
import SDL.Video
import Linear.Matrix
import System.Mem
import SDL.Raw.Enum
import Data.Maybe
import SDL
import SDL.Raw.Video

main :: IO ()
main = do
    putStrLn $ "This will run the smoke test. The smoke test merely tests " <>
               "that " <>
               "things don't crash and burn when trying to heavily draw " <>
               "something."
    putStrLn $ "However with some drivers it can drive the computer to " <>
               "become unresponsive and difficult to recover without a " <>
               "reboot."
    putStrLn "Press enter to start the smoke test."
    _ <- getLine
    putStrLn "Running the smoke test..."
    bracket_ (SDL.initialize [InitVideo])
             quit
             program

program :: IO ()
program = 
    withCString "smoke-test" $ \cstr -> do
        _ <- glSetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 3
        _ <- glSetAttribute SDL_GL_CONTEXT_MINOR_VERSION 3
        _ <- glSetAttribute SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_CORE
        _ <- glSetAttribute  SDL_GL_CONTEXT_FLAGS SDL_GL_CONTEXT_DEBUG_FLAG
        --window <- createWindow "smoke-test" (defaultWindow{windowPosition = Wherever, windowInitialSize = V2 500 500, windowGraphicsContext = defaultOpenGL })
        window <- SDL.Raw.Video.createWindow cstr SDL_WINDOWPOS_UNDEFINED SDL_WINDOWPOS_UNDEFINED
                                    500 500
                                    (SDL_WINDOW_OPENGL .|.
                                     SDL_WINDOW_SHOWN)
        _ <- SDL.Raw.Video.glCreateContext window
        giveContext $ do
            -- Make some buffers.
            for_ [1..100 :: Integer] $ \idx -> do
                buf <- newBuffer defaultBufferCreation { size = 1024
                                                       , accessFlags =
                                                           WriteAccess }
                withMapping 0 500 WriteAccess buf $ \_ -> return ()
                runPendingFinalizers
                putStrLn $ "Buffer creation: " <> show idx <> "/100"

            -- Make some buffers but with much larger size and ReadAccess
            for_ [1..100 :: Integer] $ \idx -> do
                buf <- newBuffer defaultBufferCreation { size = 102400
                                                       , accessFlags =
                                                           ReadAccess }
                withMapping 23 50000 ReadAccess buf $ \_ -> return ()
                Graphics.Caramia.copy buf 100 buf 500 400
                runPendingFinalizers

                -- Compile a gazillion shaders
                sh1 <- newShader fragmentShaderSrc Fragment
                pipeline <- newPipeline [sh1] mempty
                loc <- getUniformLocation "tutturuu" pipeline
                setUniform (transpose identity :: M44 CFloat)
                           (fromJust loc)
                           pipeline
                setUniform (identity :: M44 CDouble)
                           (fromJust loc)
                           pipeline
                setUniform (identity :: M33 Float)
                           (fromJust loc)
                           pipeline
                setUniform (transpose identity :: M33 Double)
                           (fromJust loc)
                           pipeline

                tex <- newTexture
                    textureSpecification
                    { topology = TexCube { widthCube = 299 }
                    , mipmapLevels = 5
                    , imageFormat = SRGB8_ALPHA8 }

                uploadToTexture
                    (uploading2D buf 28 28 FWord32 UBGRA)
                    {
                        cubeSide = NegativeX
                    }
                    tex

                -- Make some stupid VAOs
                vao <- newVAO
                vao2 <- newVAO
                sourceVertexData buf
                                 ((defaultSourcingType (5 :: Word16))
                                  { offset = 123
                                  , components = 3
                                  , stride = 19
                                  , attributeIndex = 3
                                  , integerMapping = True })
                                 vao2
                sourceVertexData buf
                                 ((defaultSourcingType (5 :: Word16))
                                  { offset = 123
                                  , components = 3
                                  , stride = 19
                                  , instancingDivisor = 19
                                  , attributeIndex = 3
                                  , Graphics.Caramia.normalize = True
                                  , integerMapping = False })
                                 vao
                draw drawCommand {
                     primitiveType = LineLoop
                   , primitivesVAO = vao2
                   , numIndices = 8
                   , numInstances = 213
                   , sourceData = PrimitivesWithIndices {
                       indexBuffer = buf
                     , indexOffset = 17
                     , indexType = IWord16 } }
                     defaultDrawParams {
                     pipeline = pipeline
                     }

                fbuf <- newFramebuffer [ (ColorAttachment 3
                                       , frontTextureTarget tex) ]
                draw drawCommand {
                     primitiveType = LineLoop
                   , primitivesVAO = vao2
                   , numIndices = 8
                   , numInstances = 999
                   , sourceData = Primitives {
                       firstIndex = 3 } }
                     defaultDrawParams {
                     pipeline = pipeline
                   , targetFramebuffer = fbuf
                     }
                putStrLn $ "Drawing: " <> show idx <> "/100"

            performGC
            runPendingFinalizers
            return ()

fragmentShaderSrc :: T.Text
fragmentShaderSrc = "" <>
    "#version 330\n" <>
    "uniform mat3 tutturuu;\n" <>
    "layout (location = 0) out vec4 colorOut;\n" <>
    "void main() {\n" <>
    "    colorOut = vec4(tutturuu[0][0]);\n" <>
    "}\n"
