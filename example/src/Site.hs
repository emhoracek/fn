{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Site where

import           Control.Lens
import           Control.Logging
import           Data.Default                      (def)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Pool
import           Data.Serialize.Text               ()
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Read                    as T
import qualified Data.Vault.Lazy                   as Vault
import qualified Database.PostgreSQL.Simple        as PG
import qualified Database.Redis                    as R
import           Heist
import           Network.Wai
import           Network.Wai.Session               (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import qualified Text.XmlHtml                      as X
import           Web.ClientSession                 (randomKey)
import           Web.Fn
import           Web.Fn.Extra.Heist

data Ctxt = Ctxt { _req   :: Request
                 , _heist :: FnHeistState Ctxt
                 , _db    :: Pool PG.Connection
                 , _redis :: R.Connection
                 , _sess  :: Vault.Key (Session IO Text Text)
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

instance HeistContext Ctxt where
  heistLens = heist

exampleSplices :: Splices (FnSplice Ctxt)
exampleSplices =
  tag "current-url" (attr "n" &= attrOpt "prefix") $ \ctxt _ rep pref ->
    return $
    replicate
      rep
      (X.TextNode (fromMaybe "" pref <> (T.decodeUtf8 . rawPathInfo $ ctxt ^. req)))

initializer :: IO Ctxt
initializer =
  do hs' <- heistInit
              ["templates"]
              exampleSplices
     let hs = case hs' of
                Left ers -> errorL' ("Heist failed to load templates: \n" <> T.intercalate "\n" (map T.pack ers))
                Right hs'' -> hs''
     pgpool <- createPool (PG.connect (PG.ConnectInfo "localhost"
                                                      5432
                                                      "fn_user"
                                                      "111"
                                                      "fn_db"))
                          PG.close 1 60 20
     rconn <- R.connect R.defaultConnectInfo
     session <- Vault.newKey
     return (Ctxt defaultRequest hs pgpool rconn session)

app :: IO (Application, IO ())
app =
  do -- NOTE(dbp 2015-10-25): in real applications, you would want to only
     -- call randomKey when you had never before - the first part of the
     -- tuple is a ByteString you would use for future initializations.
     -- If you call randomKey each time, every time you restart you would
     -- invalidate pre-existing sessions. Also, if you have different
     -- keys on different instances of the application, sessions created
     -- on one wouldn't be valid on the other (so store the ByteString in
     -- Redis or something).
     (_, k) <- randomKey
     let store = clientsessionStore k
     ctxt <- initializer
     return (withSession store "_session" def (ctxt ^. sess) (toWAI ctxt site)
            ,destroyAllResources (ctxt ^. db))

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [end ==> indexHandler
             ,path "param" /? param "id" ==> paramHandler
             ,path "template" ==> templateHandler
             ,path "db" /? param "number" ==> dbHandler
             ,path "segment" // segment ==> segmentHandler
             ,path "redis" // segment /? paramOpt "set" ==> redisHandler
             ,path "session" ==> sessionHandler
             ,anything ==> heistServe
             ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler _ =
  okText ("Try /param?id=123, /template, /db?number=123, /segment/foo,"
            <> " /redis/key, /redis/key?set=new, or /session")

paramHandler :: Ctxt -> Int -> IO (Maybe Response)
paramHandler _ i =
  okText (T.pack (show i))

templateHandler :: Ctxt -> IO (Maybe Response)
templateHandler ctxt =
  do t <- render ctxt "template"
     case t of
       Nothing -> okText "Could not find template. Did you start application from example directory?"
       Just _ -> return t

dbHandler :: Ctxt -> Int -> IO (Maybe Response)
dbHandler ctxt n =
  do r <- withResource (ctxt ^. db) $ \c -> PG.query c "select ?" (PG.Only n)
     okText (T.pack (show (r :: [[Int]])))

segmentHandler :: Ctxt -> Text -> IO (Maybe Response)
segmentHandler _ seg = okText seg

redisHandler :: Ctxt -> Text -> Either ParamError [Text] -> IO (Maybe Response)
redisHandler ctxt key new =
  do res <- R.runRedis (ctxt ^. redis) $
              do let k = T.encodeUtf8 key
                 case new of
                   Left _ -> R.get k
                   Right new' -> R.getset k (T.encodeUtf8 (head new'))
     case res of
       Left err ->
         errText (T.pack (show err))
       Right value ->
         okText (T.pack (show value))

sessionHandler :: Ctxt -> IO (Maybe Response)
sessionHandler ctxt =
  do let Just (getsess, putsess) = Vault.lookup (ctxt ^. sess)
                                                (vault (ctxt ^. req))
     current <- fromMaybe "0" <$> getsess "visits"
     let cur = case T.decimal current of
                 Left _ -> error "Bad value in session"
                 Right (n,_) -> n
     putsess "visits" (T.pack (show (cur + 1 :: Int)))
     okText (T.pack (show cur))

heistServe :: Ctxt -> IO (Maybe Response)
heistServe ctxt =
  let template = T.intercalate "/" (pathInfo $ _req ctxt) in
  render ctxt template
