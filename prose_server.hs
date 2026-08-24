import Control.Concurrent.MVar
import Control.Monad (forM_)
import Network.HTTP
import OpenSSL
import Text.JSON

main = do
    stateRef <- newMVar (emptyDocument,[])
    simpleServer (Just 4455) Nothing (serverMain stateRef)

serverMain :: MVar (Document,[MVar [Step]]) -> Request -> IO Response
serverMain stateRef rq =
  case decodeRequest rq of
    Ok fn     -> fn
    Error msg -> return
                   (Response
                      { rspCode = 400
                      , rspReason = "Bad Request"
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
            "pushUpdates" -> do version  <- valFromObj "version"  json
                                clientID <- valFromObj "clientID" json
                                steps    <- valFromObj "steps"    json
                                doc      <- valFromObj "doc"      json
                                return (pushUpdates version clientID steps doc)
            "pullUpdates" -> do version <- valFromObj "version" json
                                return (pullUpdates version)
            _             -> fail ("Unknown request type: " ++ typ)

    fetchHTML = do
      s <- readFile "prose.html"
      htmlResponse s

    getDocument = do
      (Document doc steps,_) <- readMVar stateRef
      jsonResponse
        (makeObj [("version", showJSON (length steps))
                 ,("doc",     documentJSON doc)
                 ])

    pushUpdates version clientID newSteps newDoc = do
      (doc@(Document _ steps),pending) <- takeMVar stateRef
      if version == length steps
        then do let acceptedSteps = [Step clientID step | step <- newSteps]
                    nextDoc = Document (Just newDoc) (steps ++ acceptedSteps)
                if null acceptedSteps
                  then return ()
                  else forM_ pending $ \var ->
                         putMVar var acceptedSteps
                putMVar stateRef (nextDoc,[])
                jsonResponse
                  (makeObj [("accepted", showJSON True)
                           ,("version",  showJSON (length steps + length acceptedSteps))
                           ,("steps",    showJSON [step | Step _ step <- acceptedSteps])
                           ,("clientIDs", showJSON [acceptedClientID | Step acceptedClientID _ <- acceptedSteps])
                           ])
        else do putMVar stateRef (doc,pending)
                jsonResponse
                  (makeObj [("accepted", showJSON False)
                           ,("version",  showJSON (length steps))
                           ,("steps",    showJSON ([] :: [JSValue]))
                           ,("clientIDs", showJSON ([] :: [ClientID]))
                           ])

    pullUpdates version = do
      (doc@(Document _ steps),pending) <- takeMVar stateRef
      case compare version (length steps) of
        LT -> do putMVar stateRef (doc,pending)
                 stepsResponse (drop version steps)
        EQ -> do var <- newEmptyMVar
                 putMVar stateRef (doc,var:pending)
                 newSteps <- takeMVar var
                 stepsResponse newSteps
        GT -> do putMVar stateRef (doc,pending)
                 stepsResponse []

htmlResponse :: String -> IO Response
htmlResponse body =
  return
    (Response
       { rspCode = 200
       , rspReason = "OK"
       , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
       , rspBody = body
       })

jsonResponse :: JSValue -> IO Response
jsonResponse body =
  return
    (Response
       { rspCode = 200
       , rspReason = "OK"
       , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
       , rspBody = encode body
       })

stepsResponse :: [Step] -> IO Response
stepsResponse steps =
  jsonResponse
    (makeObj [("steps",     showJSON [step | Step _ step <- steps])
             ,("clientIDs", showJSON [clientID | Step clientID _ <- steps])
             ])

documentJSON :: Maybe JSValue -> JSValue
documentJSON Nothing    = JSNull
documentJSON (Just doc) = doc

type ClientID = String

data Step = Step ClientID JSValue
data Document = Document (Maybe JSValue) [Step]

emptyDocument :: Document
emptyDocument = Document Nothing []
