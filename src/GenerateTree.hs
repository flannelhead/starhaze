module Main where

import qualified Data.ByteString.Lazy as B
import           System.Environment   (getArgs)

import           StarMap
import           Util

-- Generate and store the k-d star tree from a star catalog

main :: IO ()
main = do
    args <- getArgs
    case args of
        [infile, outfile, threshold] -> do
            outfile' <- normalizePath outfile
            infile' <- normalizePath infile
            eitherMap <- readMapFromFile infile'
            case eitherMap of
                Right stars -> do
                    putStrLn "Generating the star tree..."
                    treeBs <- timeAction "Building the tree"
                        (return $! treeToByteString .
                         buildStarTree $
                         filter (\(_, (mag, _)) -> mag < read threshold) stars)
                    promptOverwriteFile outfile' $ B.fromStrict treeBs
                    putStrLn $ "Tree saved to " ++ outfile' ++ "."
                Left  err   ->  putStrLn err
        _ -> putStrLn "Usage: generate-tree <INFILE> <OUTFILE> <THRESHOLD>"
