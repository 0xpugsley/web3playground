{-# LANGUAGE OverloadedStrings #-}

module Main where

import ArbitrumGuildTokenBundle
import Crypto.Ethereum (PrivateKey)
import Data.ByteArray.HexString (hexString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Bytechar
import Data.Default (def)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import Lens.Micro ((.~))
import Network.Ethereum hiding (name)
import Network.Ethereum.Account
import Network.Ethereum.Api.Personal (importRawKey, Passphrase)
import Network.Web3
import Network.Web3.Provider (Provider (..), Web3Error (ParserFail), runWeb3')
import System.Console.GetOpt (ArgDescr (OptArg, ReqArg), ArgOrder (Permute), OptDescr (..), getOpt, usageInfo)
import System.Environment (getArgs)
import Text.Printf (printf)

data Flag
  = File String
  | OtherFile String
  deriving (Show)

options :: [OptDescr Flag]
options =
  [ Option ['f'] [] (ReqArg File "FILE") "input FILE",
    Option ['g'] [] (OptArg someFile "FILE") "input FILE"
  ]

someFile :: Maybe String -> Flag
someFile = OtherFile . fromMaybe "stdin"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: ic [OPTION...] files..."

getKfilePath :: [Flag] -> IO String
getKfilePath f = do
  let fileOpt = find cond f
        where
          cond x = case x of
            File _ -> True
            _ -> False
  case fileOpt of
    Just (File fl) -> return fl
    _ -> error "no k file"

arbitrumRPC = "https://arb-mainnet.g.alchemy.com/v2/qCGjBH0p6_NEzJmfXHEqHEiCYpPDHpUZ" -- TODO from config

bundleContract = "0x13c2910c06f6f8648Cb01d4f20AD8FA1d3868a17" -- for some reason the whole abi cannot be read

-- signBundleTransaction :: IO (Either Web3Error String)
-- signBundleTransaction = do
--   runWeb3' (HttpProvider arbitrumRPC) $
--     withAccount () $
--       withParam (to .~ bundleContract) $ do
--         show <$> uSDC

readContract :: String -> IO (Either Web3Error String)
readContract key = do
  let hex = hexString $ Bytechar.pack key
  h <- case hex of
    Right s -> return s
    Left _ -> error "parse fail"
  -- let account = importRawKey h (pack "DUPA")
  -- runWeb3' (HttpProvider arbitrumRPC) $
  runWeb3' (HttpProvider arbitrumRPC) $
    withAccount () $
      withParam (to .~ bundleContract) $ do
        show <$> uSDC

main :: IO ()
main = do
  args <- getArgs
  (f, _) <- compilerOpts args
  file <- getKfilePath f
  rF <- readFile file
  let one = head $ lines rF
  -- print one
  result <- readContract one
  case result of
    Left err -> error (show err)
    Right info -> putStrLn info

-- result <- runWeb3' (HttpProvider "https://mainnet.infura.io/v3/1b519f85bfe8452f88eb83043e449d63") $
--   withAccount () $
--     withParam (to .~ "0x514910771AF9Ca656af840dff83E8264EcF986CA") $ do
--       n <- name
--       s <- symbol
--       d <- decimals
--       return $
--         printf
--           "Token %s with symbol %s and decimals %d"
--           (unpack n)
--           (unpack s)
--           (fromIntegral d :: Int)
-- case result of
--   Left err -> error (show err)
--   Right info -> putStrLn info
