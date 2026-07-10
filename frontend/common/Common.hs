-- | Helpers shared by the client and admin frontends: JSON XHR wrappers,
-- browser-integration utilities (localStorage, location, print, clipboard)
-- and the common look.
module Common
  ( postJsonXhr
  , putJsonXhr
  , postEmptyXhr
  , deleteXhr
  , getJson
  , decodeXhr
  , localStorageGet
  , localStorageSet
  , locationHash
  , redirectTo
  , printPage
  , copyToClipboard
  , baseCss
  , btnAttrs
  , mxn
  ) where

import           Data.Aeson                  (FromJSON, ToJSON, decodeStrict,
                                              encode)
import qualified Data.ByteString.Lazy        as BL
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified GHCJS.DOM                   as DOM
import qualified GHCJS.DOM.Location          as Location
import qualified GHCJS.DOM.Storage           as Storage
import qualified GHCJS.DOM.Window            as Window
import           Language.Javascript.JSaddle (MonadJSM, liftJSM, eval, call,
                                              toJSVal, jsg)
import           Reflex.Dom

import           Directo.Types               (centsToMxn)

-- ── XHR ─────────────────────────────────────────────────────────────────

jsonReq :: ToJSON a => Text -> Text -> a -> XhrRequest Text
jsonReq method url body = XhrRequest method url $ def
  & xhrRequestConfig_headers  .~ ("Content-Type" =: "application/json")
  & xhrRequestConfig_sendData .~ TE.decodeUtf8 (BL.toStrict (encode body))

postJsonXhr :: ToJSON a => Text -> a -> XhrRequest Text
postJsonXhr = jsonReq "POST"

putJsonXhr :: ToJSON a => Text -> a -> XhrRequest Text
putJsonXhr = jsonReq "PUT"

postEmptyXhr :: Text -> XhrRequest ()
postEmptyXhr url = XhrRequest "POST" url def

deleteXhr :: Text -> XhrRequest ()
deleteXhr url = XhrRequest "DELETE" url def

getJson :: Text -> XhrRequest ()
getJson url = XhrRequest "GET" url def

decodeXhr :: FromJSON a => XhrResponse -> Maybe a
decodeXhr r = case _xhrResponse_responseText r of
  Just t | _xhrResponse_status r == 200 -> decodeStrict (TE.encodeUtf8 t)
  _                                     -> Nothing

-- ── browser integration ─────────────────────────────────────────────────

localStorageGet :: MonadJSM m => Text -> m (Maybe Text)
localStorageGet key = liftJSM $ do
  window  <- DOM.currentWindowUnchecked
  storage <- Window.getLocalStorage window
  Storage.getItem storage key

localStorageSet :: MonadJSM m => Text -> Text -> m ()
localStorageSet key val = liftJSM $ do
  window  <- DOM.currentWindowUnchecked
  storage <- Window.getLocalStorage window
  Storage.setItem storage key val

locationHash :: MonadJSM m => m Text
locationHash = liftJSM $ do
  window   <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  Location.getHash location

redirectTo :: MonadJSM m => Text -> m ()
redirectTo url = liftJSM $ do
  window   <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  Location.setHref location url

printPage :: MonadJSM m => m ()
printPage = liftJSM $ do
  _ <- eval ("window.print();" :: Text)
  pure ()

copyToClipboard :: MonadJSM m => Text -> m ()
copyToClipboard txt = liftJSM $ do
  f <- eval ("(function (t) { if (navigator.clipboard) { navigator.clipboard.writeText(t); } })" :: Text)
  this <- jsg ("window" :: Text)
  arg  <- toJSVal txt
  _ <- call f this [arg]
  pure ()

-- ── look ────────────────────────────────────────────────────────────────

mxn :: Int -> Text
mxn cents = centsToMxn cents <> " MXN"

btnAttrs :: Text -> (Text, Text) -> (Text, Text)
btnAttrs cls (k, v) = (k, v <> " " <> cls)

baseCss :: Text
baseCss = T.unlines
  [ "*{box-sizing:border-box;margin:0;padding:0;}"
  , "body{font-family:'Segoe UI',system-ui,-apple-system,sans-serif;background:#f5f6f8;color:#1d2733;}"
  , "a{color:inherit;}"
  , ".topbar{background:#12365f;color:#fff;padding:0.8rem 1.4rem;display:flex;align-items:center;gap:1rem;flex-wrap:wrap;position:sticky;top:0;z-index:20;}"
  , ".brand{font-size:1.25rem;font-weight:700;letter-spacing:0.02em;}"
  , ".brand small{font-weight:400;opacity:0.8;display:block;font-size:0.7rem;}"
  , ".topbar .spacer{flex:1;}"
  , ".navbtn{background:transparent;color:#fff;border:1px solid rgba(255,255,255,0.35);border-radius:6px;padding:0.45rem 0.9rem;cursor:pointer;font-size:0.9rem;}"
  , ".navbtn:hover{background:rgba(255,255,255,0.12);}"
  , ".navbtn.primary{background:#e8a33d;border-color:#e8a33d;color:#1d2733;font-weight:600;}"
  , ".wrap{max-width:1100px;margin:0 auto;padding:1.4rem;}"
  , ".controls{display:flex;gap:0.8rem;flex-wrap:wrap;margin-bottom:1.2rem;align-items:center;}"
  , ".search{flex:1;min-width:220px;padding:0.6rem 0.9rem;border:1px solid #cdd5df;border-radius:8px;font-size:0.95rem;}"
  , ".pills{display:flex;gap:0.4rem;flex-wrap:wrap;}"
  , ".pill{border:1px solid #cdd5df;background:#fff;border-radius:999px;padding:0.35rem 0.85rem;font-size:0.85rem;cursor:pointer;}"
  , ".pill.on{background:#12365f;color:#fff;border-color:#12365f;}"
  , ".grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(230px,1fr));gap:1rem;}"
  , ".card{background:#fff;border:1px solid #e2e7ee;border-radius:10px;padding:1rem;display:flex;flex-direction:column;gap:0.45rem;}"
  , ".card .cat{font-size:0.72rem;text-transform:uppercase;letter-spacing:0.06em;color:#6b7a8c;}"
  , ".card .nm{font-weight:600;line-height:1.25;min-height:2.4em;}"
  , ".card .br{font-size:0.85rem;color:#4a5a6d;}"
  , ".card .pr{font-size:1.1rem;font-weight:700;color:#12365f;}"
  , ".card .st{font-size:0.8rem;color:#6b7a8c;}"
  , ".card button{margin-top:auto;background:#12365f;color:#fff;border:none;border-radius:8px;padding:0.55rem;cursor:pointer;font-size:0.9rem;}"
  , ".card button:disabled{background:#aab6c3;cursor:default;}"
  , ".card button:hover:enabled{background:#0d2947;}"
  , "table{border-collapse:collapse;width:100%;background:#fff;border:1px solid #e2e7ee;border-radius:10px;overflow:hidden;}"
  , "th,td{padding:0.6rem 0.8rem;text-align:left;border-bottom:1px solid #eef1f5;font-size:0.92rem;}"
  , "th{background:#f0f3f7;font-size:0.8rem;text-transform:uppercase;letter-spacing:0.04em;color:#4a5a6d;}"
  , "tr:last-child td{border-bottom:none;}"
  , ".qtybtn{border:1px solid #cdd5df;background:#fff;border-radius:6px;width:1.8rem;height:1.8rem;cursor:pointer;}"
  , ".danger{color:#b3261e;background:none;border:none;cursor:pointer;font-size:0.85rem;text-decoration:underline;}"
  , ".total{font-size:1.2rem;font-weight:700;margin:1rem 0;text-align:right;}"
  , ".field{display:flex;flex-direction:column;gap:0.25rem;margin-bottom:0.8rem;}"
  , ".field label{font-size:0.82rem;color:#4a5a6d;}"
  , ".field input,.field select,.field textarea{padding:0.55rem 0.8rem;border:1px solid #cdd5df;border-radius:8px;font-size:0.95rem;background:#fff;}"
  , ".formgrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));column-gap:1rem;}"
  , ".cta{background:#e8a33d;color:#1d2733;border:none;border-radius:8px;padding:0.7rem 1.4rem;font-weight:700;font-size:1rem;cursor:pointer;}"
  , ".cta:hover{background:#d99328;}"
  , ".cta:disabled{background:#e9d9bd;cursor:default;}"
  , ".muted{color:#6b7a8c;font-size:0.9rem;}"
  , ".error{color:#b3261e;font-size:0.9rem;margin:0.5rem 0;}"
  , ".ok{color:#1b7f4b;font-size:0.95rem;margin:0.5rem 0;}"
  , ".panel{background:#fff;border:1px solid #e2e7ee;border-radius:10px;padding:1.4rem;margin-bottom:1.2rem;}"
  , ".badge{display:inline-block;border-radius:999px;padding:0.15rem 0.7rem;font-size:0.78rem;font-weight:600;}"
  , ".badge.created{background:#e8edf3;color:#3c4c60;}"
  , ".badge.pending_payment{background:#fdf1dc;color:#8a5b00;}"
  , ".badge.paid{background:#e0f2e9;color:#1b7f4b;}"
  , ".badge.shipped{background:#e0ebfa;color:#1d5bb8;}"
  , ".badge.completed{background:#e5e5f7;color:#4740a6;}"
  , ".badge.cancelled{background:#fbe3e1;color:#b3261e;}"
  , ".rowbtns{display:flex;gap:0.5rem;flex-wrap:wrap;margin-top:0.8rem;}"
  , ".rowbtns button{border:1px solid #cdd5df;background:#fff;border-radius:8px;padding:0.45rem 0.9rem;cursor:pointer;font-size:0.88rem;}"
  , ".rowbtns button:hover{background:#f0f3f7;}"
  , ".guia{font-family:'Courier New',monospace;background:#fff;border:1px dashed #98a6b6;border-radius:8px;padding:1.2rem;white-space:pre-wrap;font-size:0.9rem;}"
  , ".h2{font-size:1.3rem;margin:0 0 1rem;}"
  , ".kpis{display:flex;gap:1rem;flex-wrap:wrap;margin-bottom:1.2rem;}"
  , ".kpi{background:#fff;border:1px solid #e2e7ee;border-radius:10px;padding:0.8rem 1.2rem;min-width:130px;}"
  , ".kpi .n{font-size:1.5rem;font-weight:700;color:#12365f;}"
  , ".kpi .l{font-size:0.78rem;color:#6b7a8c;text-transform:uppercase;letter-spacing:0.04em;}"
  , ".cartbadge{background:#e8a33d;color:#1d2733;font-weight:700;border-radius:999px;padding:0.05rem 0.5rem;font-size:0.8rem;margin-left:0.3rem;}"
  , "@media print { .topbar, .noprint { display:none !important; } body{background:#fff;} .wrap{max-width:none;padding:0;} }"
  ]
