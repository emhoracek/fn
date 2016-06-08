{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Site where

import           Control.Lens
import           Control.Monad                     (mplus)
import           Data.Default                      (def)
import qualified Data.Map                          as M
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
import           Larceny
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Session               (Session, withSession)
import           Network.Wai.Session.ClientSession (clientsessionStore)
import           Web.ClientSession                 (randomKey)
import           Web.Fn

data Ctxt = Ctxt { _req   :: FnRequest
                 , _subs  :: Ctxt -> Substitutions
                 , _lib   :: Library
                 , _db    :: Pool PG.Connection
                 , _redis :: R.Connection
                 , _sess  :: Vault.Key (Session IO Text Text)
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

exampleSubs :: Ctxt -> Substitutions
exampleSubs ctxt = do
  fills [ ("current-url", useAttrs ((a "n" % a "prefix") (currentUrlFill ctxt)))
        , ("hello", text "hello") ]

currentUrlFill :: Ctxt -> Int -> Text -> Text -> IO Text
currentUrlFill ctxt rep pref _tpl =
  let u = T.decodeUtf8 . rawPathInfo $ ctxt ^. req . _1
      pref' = Just pref in
  return $ T.concat $ replicate rep (fromMaybe "" pref' <> u)

larcenyServe :: Ctxt ->
                IO (Maybe Response)
larcenyServe ctxt =
  let p = pathInfo . fst $ getRequest ctxt in
  mplus <$> renderLarceny ctxt (T.intercalate "/" p)
        <*> renderLarceny ctxt (T.intercalate "/" (p ++ ["index"]))

renderLarceny :: Ctxt ->
                 Text ->
                 IO (Maybe Response)
renderLarceny ctxt name =
  do let tpl = (ctxt ^. lib) M.! [name]
     t <- runTemplate tpl [name] ((ctxt ^. subs) ctxt) (ctxt ^. lib)
     okHtml t

initializer :: IO Ctxt
initializer =
  do tpls <- Larceny.loadTemplates "templates"
     pgpool <- createPool (PG.connect (PG.ConnectInfo "localhost"
                                                      5432
                                                      "fn_user"
                                                      "111"
                                                      "fn_db"))
                          PG.close 1 60 20
     rconn <- R.connect R.defaultConnectInfo
     session <- Vault.newKey
     return (Ctxt defaultFnRequest exampleSubs tpls pgpool rconn session)

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
             ,path "param_many" /? paramMany "id" ==> paramManyHandler
             ,path "template" ==> templateHandler
             ,path "db" /? param "number" ==> dbHandler
             ,path "segment" // segment ==> segmentHandler
             ,path "redis" // segment /? paramOpt "set" ==> redisHandler
             ,path "session" ==> sessionHandler
             ,path "file" ==> fileHandler
             ,anything ==> larcenyServe
             ,anything ==> staticServe "static"
             ]
    `fallthrough` notFoundText "Page not found."

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler _ =
  okText ("Try /param?id=123, /template, /db?number=123, /segment/foo,"
       <> " /redis/key, /redis/key?set=new, /session, or /haskell.png")

paramHandler :: Ctxt -> Int  -> IO (Maybe Response)
paramHandler _ i =
  okText (T.pack (show i))

paramManyHandler :: Ctxt -> [Int] -> IO (Maybe Response)
paramManyHandler _ is =
  okText (T.pack (show is))

templateHandler :: Ctxt -> IO (Maybe Response)
templateHandler ctxt =
  do t <- renderLarceny ctxt "template"
     case t of
       Nothing -> okText "Could not find template. Did you start application from example directory?"
       Just _ -> return t

dbHandler :: Ctxt -> Int ->  IO (Maybe Response)
dbHandler ctxt n =
  do r <- withResource (ctxt ^. db) $ \c -> PG.query c "select ?" (PG.Only n)
     okText (T.pack (show (r :: [[Int]])))

segmentHandler :: Ctxt -> Text ->  IO (Maybe Response)
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
                                                (vault (ctxt ^. req . _1))
     current <- fromMaybe "0" <$> getsess "visits"
     let cur = case T.decimal current of
                 Left _ -> error "Bad value in session"
                 Right (n,_) -> n
     putsess "visits" (T.pack (show (cur + 1 :: Int)))
     okText (T.pack (show cur))

fileHandler :: Ctxt -> IO (Maybe Response)
fileHandler ctxt = route ctxt [method GET ==> const (renderLarceny ctxt "file")
                              ,method POST /? file "f" ==> fileH]
  where fileH _ (File name ct _) =
          okText ("Got file named " <> name <> " of type " <> ct)
