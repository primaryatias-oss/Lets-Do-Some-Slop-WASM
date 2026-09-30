{-# LANGUAGE ForeignFunctionInterface #-}
-- | WebAssembly reactor exports (GHC wasm backend): the flat C-style ABI the host expects.
module Main (main) where

import Core
import Data.Int (Int32)

foreign export ccall "init" cInit :: IO ()
cInit :: IO ()
cInit = initCore

foreign export ccall "load_level" cLoad :: IO ()
cLoad :: IO ()
cLoad = loadLevel

foreign export ccall "advance" cAdvance :: Double -> IO Int32
cAdvance :: Double -> IO Int32
cAdvance dt = fromIntegral <$> advanceCore dt

foreign export ccall "mem_get" cGet :: Int32 -> IO Double
cGet :: Int32 -> IO Double
cGet i = memGet (fromIntegral i)

foreign export ccall "mem_set" cSet :: Int32 -> Double -> IO ()
cSet :: Int32 -> Double -> IO ()
cSet i v = memSet (fromIntegral i) v

foreign export ccall "mem_ptr" cPtr :: IO Int32
cPtr :: IO Int32
cPtr = return (fromIntegral memPtrAddr)

main :: IO ()
main = return ()
