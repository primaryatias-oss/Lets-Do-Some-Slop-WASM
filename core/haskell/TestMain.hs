-- Native open-loop driver: same scenario + trace format as tools/ol_node.js.
module Main (main) where

import Abi
import Control.Monad
import Core
import Data.IORef
import GHC.Float (castDoubleToWord64)
import System.Environment
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let lvl = read (args !! 0) :: Int
      frames = read (args !! 1) :: Int
      tp = if length args > 2 then Just (read (args !! 2) :: Double) else Nothing
      dir = if length args > 3 then args !! 3 else "core/test"
  txt <- readFile (dir ++ "/level" ++ show lvl ++ ".txt")
  ref <- newIORef (words txt)
  let nx = do { (t:ts) <- readIORef ref; writeIORef ref ts; return t }
      nf = read <$> nx :: IO Double
      ni = read <$> nx :: IO Int
  initCore
  _ <- nx; wd <- ni; memSet g_lw (fromIntegral wd)
  _ <- nx; sx <- nf; sy <- nf; memSet g_spawnx sx; memSet g_spawny sy
  _ <- nx; gx <- nf; gy <- nf; gon <- ni; when (gon /= 0) $ do { memSet g_goalon 1; memSet g_goalx gx; memSet g_goaly gy }
  _ <- nx; dx <- nf; dy0 <- nf; dy1 <- nf; don <- ni
  when (don /= 0) $ do { memSet g_dooron 1; memSet g_doorx dx; memSet g_doory0 dy0; memSet g_doory1 dy1 }
  _ <- nx; bx <- nf; by <- nf; btr <- nf; bon <- ni
  when (bon /= 0) $ do { memSet g_bspecx bx; memSet g_bspecy by; memSet g_btrig btr }
  _ <- nx; sd <- nf; memSet g_rng sd
  _ <- nx; nt <- ni
  replicateM_ nt $ do { x <- ni; y <- ni; v <- nf; memSet (tile_base + y * wd + x) v }
  _ <- nx; ns <- ni
  forM_ [0 .. ns - 1] $ \i -> do
    k <- nf; x <- nf; y <- nf
    memSet (spec_base + i * 3) k; memSet (spec_base + i * 3 + 1) x; memSet (spec_base + i * 3 + 2) y
  memSet g_nspec (fromIntegral ns)
  memSet g_mode (fromIntegral mode_play); memSet g_vieww 20
  loadLevel
  case tp of { Just x -> do { memSet p_x x; memSet p_y 3 }; Nothing -> return () }
  sRef <- newIORef (7.0 :: Double)
  evTotal <- newIORef (0.0 :: Double)
  let lcg = do
        s <- readIORef sRef
        let v = s * 1664525.0 + 1013904223.0
            v' = v - fromIntegral (floor (v / 4294967296.0) :: Int) * 4294967296.0
        writeIORef sRef v'
        return (v' / 4294967296.0)
      bits :: Double -> String
      bits x = printf "%016x" (fromIntegral (castDoubleToWord64 x) :: Integer)
  forM_ [0 .. frames - 1] $ \fr -> do
    r1 <- lcg; let ax = if r1 < 0.2 then -1 else if r1 < 0.9 then 1 else 0 :: Double
    r2 <- lcg; let jp = r2 < 0.05
    r3 <- lcg; let jh = if r3 < 0.5 then 1 else 0 :: Double
    r4 <- lcg; let dp = r4 < 0.01
    r5 <- lcg; let dn = if r5 < 0.03 then 1 else 0 :: Double
    memSet in_ax ax; memSet in_jumpheld jh; memSet in_downheld dn
    when jp $ memSet in_jumppress 1
    when dp $ memSet in_dashpress 1
    _ <- advanceCore (1.0 / 60.0)
    en <- memGet g_evn
    modifyIORef evTotal (+ en)
    hp <- memGet p_hp
    when (hp < 2) $ memSet p_hp 3
    when (fr `mod` 15 == 0) $ do
      h <- foldM (\acc i -> do { v <- memGet i; return (acc + v * fromIntegral (1 + i `mod` 7)) }) 0.0 [0 .. spec_base - 1]
      vals <- mapM memGet [p_x, p_y, p_vx, p_vy, p_hp, g_score, g_coins, g_kills, g_mode, g_nsh, g_rng]
      et <- readIORef evTotal
      putStrLn (unwords (show fr : map bits (vals ++ [et, h])))
