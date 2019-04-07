{-# LANGUAGE OverloadedStrings     #-}

module Main where
  
import qualified System.Environment as Env
import qualified Data.Either as Either
import qualified System.IO.Error as Error
import           Control.Monad (replicateM, join)
import qualified GHC.Conc
import qualified Data.Char as Char

import           Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString.Char8 as Parsec
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Driver as PG
import qualified Database.PostgreSQL.Protocol.Types as PGT
import qualified Database.PostgreSQL.Protocol.Store.Encode as PGSE
import qualified Database.PostgreSQL.Protocol.Codecs.Encoders as PGCE
import qualified Database.PostgreSQL.Protocol.Codecs.PgTypes as PGCT
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Status as Status
import qualified System.Random.MWC as MWC

type Pool = Pool.Pool PG.Connection

main :: IO ()
main = do
  (dbHost, dbName, dbUser, dbPass)
    <-  (,,,)
    <$> Env.getEnv "DB_HOST"
    <*> Env.getEnv "DB_NAME"
    <*> Env.getEnv "DB_USER"
    <*> Env.getEnv "DB_PASS"
  numCaps <- GHC.Conc.getNumCapabilities
  app numCaps $ PG.defaultConnectionSettings
    { PG.settingsHost     = BSC.pack dbHost
    , PG.settingsDatabase = BSC.pack dbName
    , PG.settingsUser     = BSC.pack dbUser
    , PG.settingsPassword = BSC.pack dbPass
    }

app :: Int -> PG.ConnectionSettings -> IO ()
app numCaps dbConfig = do
  putStrLn "Config is:"
  print dbConfig
  putStrLn "Initializing PRNG seed..."
  gen <- MWC.create
  putStrLn "Initializing database connection pool..."
  dbPool <- mkPool numCaps dbConfig
  putStrLn "Warp core online"
  Warp.runEnv 7041 $ server gen dbPool

server :: MWC.GenIO -> Pool -> Wai.Application
server gen dbPool req respond = do
  let qParams = Wai.queryString req
  let mCount = parseCount =<< join (lookup "queries" qParams)
  case (Wai.requestMethod req, Wai.pathInfo req) of
    ("GET", ["updates"])
      -> respond =<< updateWorlds gen dbPool mCount
    _ -> respond routeNotFound
    
contentText :: Header.ResponseHeaders
contentText = [(Header.hContentType, "text/plain")]

respondText :: Status.Status -> LBS.ByteString -> Wai.Response
respondText code = Wai.responseLBS code contentText

-------------------------------------------------------------------------------
-- * DB utils

connect :: PG.ConnectionSettings -> IO PG.Connection
connect pgc = simplifyError =<< PG.connect pgc
  where
    simplifyError = Either.either (Error.ioError . Error.userError . show) pure

mkPool :: Int -> PG.ConnectionSettings -> IO Pool
mkPool numStripes c = Pool.createPool (connect c) PG.close numStripes 0.5 512

encodeInt :: Int -> (PGCT.Oids, PGSE.Encode)
encodeInt qId = (PGCT.int2, PGCE.int2 $ fromIntegral qId)

mkQuery :: ByteString -> [(PGCT.Oids, PGSE.Encode)] -> PG.Query
mkQuery q es = PG.Query q ps PGT.Binary PGT.Binary PG.NeverCache
  where
    mkP (oid, e) = (PGCT.oidType oid, Just e)
    ps = fmap mkP es

-------------------------------------------------------------------------------
-- * Route implementations

routeNotFound :: Wai.Response
routeNotFound = respondText Status.status400 "Bad route"

updateWorlds :: MWC.GenIO -> Pool -> Maybe Count -> IO Wai.Response
updateWorlds gen dbPool mCount = do
  wIds <- replicateM count $ randomId
  wNumbers <- replicateM count $ randomId
  go dbPool $ zip wIds wNumbers
  where
    randomId = MWC.uniformR (1, 10000) gen
    count = getCount mCount
    respondDbError = respondText Status.status500 . LBSC.pack . show
    respondOk = respondText Status.status200 "OK"
    s = "UPDATE World SET randomNumber = $1 WHERE id = $2"
    mkQ (wId, wNum) = mkQuery s [encodeInt wId, encodeInt wNum]
    logger = BSC.putStrLn . BSC.pack
    go pool wIdAndNums = Pool.withResource pool $ \conn -> do
      let qs = fmap mkQ wIdAndNums
      PG.sendBatchAndSync conn qs
      eRows <- replicateM (length qs) $ PG.readNextData conn
      _ <- PG.waitReadyForQuery conn
      let (errs, _) = Either.partitionEithers eRows
      case errs of
        [] -> pure respondOk
        _ -> do
          logger $ "Count: " <> (show count)
          logger $ "ERROR: " <> (show errs)
          pure $ respondDbError errs
{-# INLINE updateWorlds #-}

-------------------------------------------------------------------------------
-- * Count    

newtype Count = Count Int

parseCount :: ByteString -> Maybe Count
parseCount = fmap Count . Either.either (const Nothing) pure . Parsec.parseOnly parseInt

getCount :: Maybe Count -> Int
getCount Nothing = 1
getCount (Just (Count c)) = max 1 (min c 500)

-- https://stackoverflow.com/a/24171263
parseInt :: Parsec.Parser Int
parseInt = do
  digits <- Parsec.many1 parseIntDigit
  let n = foldl (\x d -> 10*x + (Char.digitToInt d)) 0 digits
  seq n (return n)

parseIntDigit :: Parsec.Parser Char
parseIntDigit = digit
  where
    digit = Parsec.satisfy isDigit
    isDigit c = c >= '0' && c <= '9'
