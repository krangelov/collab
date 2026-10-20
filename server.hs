import Network.HTTP
import OpenSSL
import Text.JSON
import Control.Concurrent.MVar
import Control.Monad(forM_)
import Data.List

main = do
    stateRef <- newMVar (emptyDocument,[])
    simpleServer (Just 4455) Nothing (serverMain stateRef)

serverMain :: MVar (Document,[MVar [Update]]) -> Request -> IO Response
serverMain stateRef rq = do
  case decodeRequest rq of
    Ok fn     -> fn
    Error msg -> return
                   (Response
                      { rspCode = 500
                      , rspReason = "Fail"
                      , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                      , rspBody = msg
                      })
  where
    decodeRequest rq
      | null (rqBody rq) = return fetchHTML
      | otherwise = do
          json <- decode (rqBody rq)
          typ  <- valFromObj "type" json
          case typ of
            "getDocument" -> return getDocument
            "pushUpdates" -> do version <- valFromObj "version" json
                                updates <- valFromObj "updates" json
                                return (pushUpdates version updates)
            "pullUpdates" -> do version <- valFromObj "version" json
                                return (pullUpdates version)

    fetchHTML = do
      s <- readFile "collab.html"
      return
        (Response
           { rspCode = 200
           , rspReason = "OK"
           , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
           , rspBody = s
           })

    getDocument = do
      (Document text updates,_) <- readMVar stateRef
      let json = toJSObject [("version",  showJSON (length updates))
                            ,("document", showJSON text)
                            ]
      return
        (Response
           { rspCode = 200
           , rspReason = "OK"
           , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
           , rspBody = encode json
           })

    pushUpdates version updates = do
      (doc,pending) <- takeMVar stateRef
      let (doc',updates',new_updates) = apply version updates doc
      if null updates'
        then return ()
        else forM_ pending $ \var -> do
               putMVar var updates'
      putMVar stateRef (doc',[])
      return
        (Response
           { rspCode = 200
           , rspReason = "OK"
           , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
           , rspBody = encode new_updates
           })

    pullUpdates version = do
      (doc@(Document text updates),pending) <- takeMVar stateRef
      if version < length updates
        then do putMVar stateRef (doc,pending)
                return
                  (Response
                     { rspCode = 200
                     , rspReason = "OK"
                     , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                     , rspBody = encode (drop version updates)
                     })
        else do var <- newEmptyMVar
                putMVar stateRef (doc,var:pending)
                updates <- takeMVar var
                return
                  (Response
                     { rspCode = 200
                     , rspReason = "OK"
                     , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                     , rspBody = encode updates
                     })


type ClientID = String
data Change   = Skip {-# UNPACK #-} !Int | Change {-# UNPACK #-} !Int String deriving Show
data Update   = Update ClientID [Change]
data Document = Document String [Update]

instance JSON Update where
    readJSON (JSObject jsobj) = do
      clientID <- valFromObj "clientID" jsobj
      changes  <- valFromObj "changes"  jsobj
      return (Update clientID changes)
    readJSON _ = fail "Every update must be an object"
    
    showJSON (Update clientID changes) =
      makeObj [("clientID", showJSON clientID)
              ,("changes",  showJSON changes)
              ]

instance JSON Change where
    readJSON (JSRational _ i)              = return (Skip (round i))
    readJSON (JSArray (JSRational _ i:ss)) = return (Change (round i) (intercalate "\n" [fromJSString s | JSString s <- ss]))
    readJSON json = fail "Invalid Change representation"

    showJSON (Skip i)      = showJSON i
    showJSON (Change i s)  = showJSON (showJSON i : [showJSON l | l <- split s])
      where
        split cs =
          case break (=='\n') cs of
            (xs,'\n':cs) -> xs : split cs
            (xs,[])      -> [xs]

emptyDocument = Document "" []

apply :: Int -> [Update] -> Document -> (Document,[Update],[Update])
apply version new_updates (Document content updates) =
  let updates0 = drop version updates
      (new_updates',updates0') = transform new_updates updates0
  in (Document (foldl update content new_updates')
               (updates++new_updates')
     ,new_updates'
     ,updates0'
     )
  where
    update content (Update _ changes) =
      let (xs,ys) = foldl change ("",content) changes
      in reverse xs ys

    change (xs,ys) (Skip i)     = skip xs ys i
    change (xs,ys) (Change i s) = (reverse s xs,drop i ys)

    skip xs    ys  0 = (xs,ys)
    skip xs    []  i = (xs,[])
    skip xs (y:ys) i = skip (y:xs) ys (i-1)

    reverse []     ys = ys
    reverse (x:xs) ys = reverse xs (x:ys)

transform :: [Update] -> [Update] -> ([Update],[Update])
transform  = mapAccumL (\us1 u2 -> swap (mapAccumL (flip transform1) u2 us1))
  where
    swap (x,y) = (y,x)

transform1 :: Update -> Update -> (Update,Update)
transform1 (Update cid1 as) (Update cid2 bs) =
  case loop as bs of
    (as,bs) -> (Update cid1 as, Update cid2 bs)
  where
    loop as            []            = (as,[])
    loop []            bs            = ([],bs)
    loop (Skip n : as) (Skip m : bs) =
      case compare n m of
        LT -> case loop as (Skip (m-n) : bs) of
                (as,bs) -> (skip n as,skip n bs)
        EQ -> case loop as bs of
                (as,bs) -> (skip n as,skip m bs)
        GT -> case loop (Skip (n-m) : as) bs of
                (as,bs) -> (skip m as,skip m bs)
    loop (Skip n : as) (Change m s2 : bs) =
      case compare n m of
        LT -> case loop as (Change (m-n) s2 : bs) of
                (as,bs) -> (skip (length s2) as,change n [] bs)
        EQ -> case loop as bs of
                (as,bs) -> (skip (length s2) as,change m s2 bs)
        GT -> case loop (Skip (n-m) : as) bs of
                (as,bs) -> (skip (length s2) as,change m s2 bs)
    loop (Change n s1 : as) (Skip m : bs) =
      case compare n m of
        LT -> case loop as (Skip (m-n) : bs) of
                (as,bs) -> (change n s1 as,skip (length s1) bs)
        EQ -> case loop as bs of
                (as,bs) -> (change n s1 as,skip (length s1) bs)
        GT -> case loop (Change (n-m) s1 : as) bs of
                (as,bs) -> (change m [] as,skip (length s1) bs)
    loop (Change n s1 : as) (Change m s2 : bs) =
      case compare n m of
        LT -> case loop as (Change (m-n) s2 : bs) of
                (as,bs) -> (change 0 s1 as,skip (length s1) bs)
        EQ -> case loop as bs of
                (as,bs) -> (skip (length s2) (change 0 s1 as),skip (length s1) (change 0 s2 bs))
        GT -> case loop (Change (n-m) s1 : as) bs of
                (as,bs) -> (skip (length s2) as,change 0 s2 bs)

    skip n (Skip m : as) = Skip (n+m) : as
    skip n           as  = Skip n     : as

    change n s (Change m s1 : as) = Change (n+m) (s++s1) : as
    change n s                as  = Change n     s       : as
