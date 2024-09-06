import Network.HTTP
import OpenSSL
import Text.JSON
import Control.Concurrent.MVar
import Control.Monad(forM_)

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
      s <- readFile "colab.html"
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
      let doc'@(Document text updates') = applyUpdates updates doc
      let x = version + 1 ::Int 
      if null updates
        then return ()
        else forM_ pending $ \var -> do
               putMVar var (drop version updates)
      putMVar stateRef (doc',[])
      return
        (Response
           { rspCode = 200
           , rspReason = "OK"
           , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
           , rspBody = encode True
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
data Change   = Skip Int | Change Int String
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
    readJSON (JSRational _ i)                       = return (Skip (round i))
    readJSON (JSArray [JSRational _ i])             = return (Change (round i) "")
    readJSON (JSArray [JSRational _ i, JSString s]) = return (Change (round i) (fromJSString s))
    readJSON _ = fail "Invalid Change representation"

    showJSON (Skip i)      = showJSON i
    showJSON (Change i "") = showJSON [i]
    showJSON (Change i s)  = showJSON [showJSON i, showJSON s]

emptyDocument = Document "" []

applyUpdates :: [Update] -> Document -> Document
applyUpdates new_updates (Document content updates) = 
  Document (foldl applyUpdate content new_updates)
           (updates++new_updates)
  where
    applyUpdate content (Update _ changes) = 
      let (_,_,content') = foldl applyChange (0,0,content) changes
      in content'

    applyChange (posA,posB,content) (Skip i)     = (posA+i,posB+i,content)
    applyChange (posA,posB,content) (Change i s) = (posA+i,posB+length s,take posB content ++ s ++ drop (posB + i) content)
