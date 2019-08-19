module GitClient where
import GitObj
import Repo
import Data.Aeson
import Control.Monad.Except
import Control.Monad.Reader
import Network.HTTP.Simple
import GHC.Exception.Type
import Config
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
type Resp r = ReaderT Config (ExceptT String IO) r

gitReq :: (FromJSON r) => String -> (Request->Request) -> Resp r
gitReq req fn = do
  token <- asks token
  req <- liftEither $ either (throwError.show) return $ 
    parseRequest $ req
  resp <- liftIO $  httpJSONEither $ setRequestHeader (CI.mk $ C8.pack "Authorization") [C8.pack $ "token"++token] $ fn req
  either (throwError.show) return $ getResponseBody resp

getJSON :: (FromJSON r) => GitObj -> FromRepo -> Int -> Resp r
getJSON what (FromRepo repo) num = 
  gitReq ("https://api.github.com/"++repo++"/"++show what++"/"++ show what++"/"++ show num) id
  
putJSON :: (ToJSON r,FromJSON r1)=>GitObj -> ToRepo -> r -> Resp r1
putJSON  what (ToRepo repo) val = 
  gitReq ("POST https://api.github.com/"++repo++"/"++show what++"/"++ show what) (setRequestBodyJSON val)

patchJSON :: (ToJSON r,FromJSON r1) => GitObj -> ToRepo -> Int -> r -> Resp r1
patchJSON what (ToRepo repo) num val =
  gitReq ("PATCH https://api.github.com/"++repo++"/"++show what++"/"++ show what ++ show num) (setRequestBodyJSON val)

