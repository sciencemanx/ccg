module Main where

import Lib (disass)
import System.Environment (getProgName)
import System.Posix.Env.ByteString (getArgs)
import System.Exit (die)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (unpack)

utf2str :: ByteString -> String
utf2str = unpack . decodeUtf8

usage :: () -> IO a
usage () = do
    progname <- getProgName
    die ("Usage: " ++ progname ++ " <filename>")

main :: IO ()
main = do
    args <- getArgs
    filename <- if length args == 1 then return (args !! 0) else usage ()
    disass $ utf2str filename
