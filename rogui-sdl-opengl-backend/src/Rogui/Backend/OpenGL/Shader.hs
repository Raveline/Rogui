{-# LANGUAGE QuasiQuotes #-}

module Rogui.Backend.OpenGL.Shader
  ( initShaderProgram,
    vertexShaderSrc,
    fragmentShaderSrc,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Foreign (alloca, allocaArray, peekArray, withArray)
import Foreign.Storable (peek)
import Graphics.GL
import Text.RawString.QQ

-- | Compile and link the sprite shader program.
initShaderProgram :: MonadIO m => m GLuint
initShaderProgram = liftIO $ do
  vertShader <- compileShader GL_VERTEX_SHADER vertexShaderSrc
  fragShader <- compileShader GL_FRAGMENT_SHADER fragmentShaderSrc
  program <- glCreateProgram
  glAttachShader program vertShader
  glAttachShader program fragShader
  glLinkProgram program
  checkProgramLink program
  glDeleteShader vertShader
  glDeleteShader fragShader
  pure program

compileShader :: GLenum -> BS.ByteString -> IO GLuint
compileShader shaderType src = do
  shader <- glCreateShader shaderType
  BS.useAsCStringLen src $ \(srcPtr, srcLen) ->
    withArray [srcPtr] $ \srcPtrArr ->
      withArray [fromIntegral srcLen] $ \lenArr ->
        glShaderSource shader 1 srcPtrArr lenArr
  glCompileShader shader
  checkShaderCompile shader
  pure shader

checkShaderCompile :: GLuint -> IO ()
checkShaderCompile shader =
  alloca $ \statusPtr -> do
    glGetShaderiv shader GL_COMPILE_STATUS statusPtr
    status <- peek statusPtr
    when (status == 0) $ do
      alloca $ \lenPtr -> do
        glGetShaderiv shader GL_INFO_LOG_LENGTH lenPtr
        logLen <- peek lenPtr
        allocaArray (fromIntegral logLen) $ \logPtr -> do
          glGetShaderInfoLog shader logLen lenPtr logPtr
          logMsg <- peekArray (fromIntegral logLen) logPtr
          error $ "Shader compile failed: " <> map (toEnum . fromIntegral) logMsg

checkProgramLink :: GLuint -> IO ()
checkProgramLink program =
  alloca $ \statusPtr -> do
    glGetProgramiv program GL_LINK_STATUS statusPtr
    status <- peek statusPtr
    when (status == 0) $ do
      alloca $ \lenPtr -> do
        glGetProgramiv program GL_INFO_LOG_LENGTH lenPtr
        logLen <- peek lenPtr
        allocaArray (fromIntegral logLen) $ \logPtr -> do
          glGetProgramInfoLog program logLen lenPtr logPtr
          logMsg <- peekArray (fromIntegral logLen) logPtr
          error $ "Shader link failed: " <> map (toEnum . fromIntegral) logMsg

vertexShaderSrc :: BS.ByteString
vertexShaderSrc = [r|#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aUV;
layout (location = 2) in vec4 aFrontColour;
layout (location = 3) in vec4 aBackColour;

uniform mat4 uProjection;

out vec2 vUV;
out vec4 vFrontColour;
out vec4 vBackColour;

void main() {
    gl_Position = uProjection * vec4(aPos, 0.0, 1.0);
    vUV = aUV;
    vFrontColour = aFrontColour;
    vBackColour = aBackColour;
}
|]

fragmentShaderSrc :: BS.ByteString
fragmentShaderSrc = [r|#version 330 core
in vec2 vUV;
in vec4 vFrontColour;
in vec4 vBackColour;

uniform sampler2D uTileset;
uniform bool uUseTileset;

out vec4 fragColor;

void main() {
    if (!uUseTileset) {
        fragColor = vFrontColour;
        return;
    }

    vec4 texel = texture(uTileset, vUV);

    if (vBackColour.a > 0.0) {
        fragColor = vBackColour;
        vec4 fg = texel * vFrontColour;
        fragColor = vec4(
            mix(fragColor.rgb, fg.rgb, fg.a),
            max(fragColor.a, fg.a)
        );
    } else {
        fragColor = texel * vFrontColour;
    }
}
|]
