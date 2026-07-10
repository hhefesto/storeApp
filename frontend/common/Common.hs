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
  , setLocationHash
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

setLocationHash :: MonadJSM m => Text -> m ()
setLocationHash h = liftJSM $ do
  window   <- DOM.currentWindowUnchecked
  location <- Window.getLocation window
  Location.setHash location h

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

-- | Design system shared by the storefront and the admin.
--
-- The visual identity is the shop's own world — a 43-year-old parts
-- counter in Querétaro's centro: teal from the legacy brand (darkened
-- for contrast), ink, paper, and one price-label orange reserved for
-- money. Display type is Archivo stretched wide (industrial signage);
-- data (SKUs, prices, refs, hours) is set in IBM Plex Mono like a
-- parts catalog. Fonts are self-hosted at /static/fonts/fonts.css.
baseCss :: Text
baseCss = T.unlines
  [ "*{box-sizing:border-box;margin:0;padding:0;}"
  , ":root{"
  , "  --tinta:#10312f; --tinta-suave:#41615d;"
  , "  --marca:#17877d; --marca-osc:#0f5f58; --marca-tinte:#e3f1ef;"
  , "  --cielo:#028bca; --cielo-tinte:#e3f2fa;"
  , "  --papel:#f2f5f4; --blanco:#ffffff;"
  , "  --etiqueta:#c9491d;"
  , "  --linea:#dce4e2; --linea-fuerte:#b8c6c3;"
  , "  --sombra:0 1px 2px rgba(16,49,47,0.06),0 4px 12px rgba(16,49,47,0.05);"
  , "  --radio:10px;"
  , "}"
  , "body{font-family:'Archivo',system-ui,sans-serif;background:var(--papel);color:var(--tinta);line-height:1.55;}"
  , "a{color:var(--marca);}"
  , "button{font-family:inherit;}"
  , ":focus-visible{outline:2px solid var(--cielo);outline-offset:2px;}"
  , "@media (prefers-reduced-motion:reduce){*,*::before,*::after{animation:none!important;transition:none!important;}}"
  -- display + utility type
  , ".display{font-stretch:125%;font-weight:850;text-transform:uppercase;letter-spacing:0.01em;line-height:1.05;}"
  , ".mono,.guia,.card .sku,.card .pr,.pr-mono{font-family:'IBM Plex Mono',ui-monospace,monospace;}"
  , ".eyebrow{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:0.72rem;letter-spacing:0.14em;text-transform:uppercase;color:var(--marca);margin-bottom:0.5rem;}"
  -- topbar (admin keeps using it; the storefront has its own header)
  , ".topbar{background:var(--tinta);color:#fff;padding:0.8rem 1.4rem;display:flex;align-items:center;gap:1rem;flex-wrap:wrap;position:sticky;top:0;z-index:20;}"
  , ".brand{font-size:1.15rem;font-stretch:125%;font-weight:850;text-transform:uppercase;letter-spacing:0.02em;}"
  , ".brand small{font-weight:400;font-stretch:100%;text-transform:none;opacity:0.75;display:block;font-size:0.7rem;letter-spacing:0;}"
  , ".topbar .spacer{flex:1;}"
  , ".navbtn{background:transparent;color:#fff;border:1px solid rgba(255,255,255,0.35);border-radius:8px;padding:0.45rem 0.9rem;cursor:pointer;font-size:0.9rem;}"
  , ".navbtn:hover{background:rgba(255,255,255,0.12);}"
  , ".navbtn.primary{background:var(--marca);border-color:var(--marca);color:#fff;font-weight:600;}"
  , ".navbtn.primary:hover{background:var(--marca-osc);}"
  , ".wrap{max-width:1120px;margin:0 auto;padding:1.4rem;}"
  -- controls
  , ".controls{display:flex;gap:0.8rem;flex-wrap:wrap;margin-bottom:1.2rem;align-items:center;}"
  , ".search{flex:1;min-width:220px;padding:0.6rem 0.9rem;border:1px solid var(--linea-fuerte);border-radius:8px;font-size:0.95rem;font-family:inherit;background:var(--blanco);}"
  , ".pills{display:flex;gap:0.4rem;flex-wrap:wrap;}"
  , ".pill{border:1px solid var(--linea-fuerte);background:var(--blanco);border-radius:999px;padding:0.35rem 0.85rem;font-size:0.85rem;cursor:pointer;color:var(--tinta);}"
  , ".pill:hover{border-color:var(--marca);}"
  , ".pill.on{background:var(--tinta);color:#fff;border-color:var(--tinta);}"
  -- product grid: each card is a parts-bin ticket (la ficha de almacén)
  , ".grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(240px,1fr));gap:1rem;}"
  , ".card{background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);padding:0;display:flex;flex-direction:column;overflow:hidden;transition:transform .15s ease,box-shadow .15s ease;}"
  , ".card:hover{transform:translateY(-2px);box-shadow:var(--sombra);}"
  , ".card .foto{aspect-ratio:1;background:var(--blanco);display:flex;align-items:center;justify-content:center;border-bottom:1px dashed var(--linea-fuerte);position:relative;cursor:pointer;}"
  , ".card .foto img{max-width:82%;max-height:82%;object-fit:contain;}"
  , ".card .foto .sinfoto{color:var(--linea-fuerte);font-size:0.8rem;}"
  , ".card .sku{position:absolute;top:0.6rem;left:0.6rem;font-size:0.68rem;letter-spacing:0.08em;color:var(--tinta-suave);background:var(--papel);padding:0.1rem 0.45rem;border-radius:4px;}"
  , ".card .cuerpo{padding:0.85rem 0.95rem 0.95rem;display:flex;flex-direction:column;gap:0.4rem;flex:1;}"
  , ".card .cat{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:0.66rem;text-transform:uppercase;letter-spacing:0.12em;color:var(--marca);}"
  , ".card .nm{font-weight:600;line-height:1.3;cursor:pointer;}"
  , ".card .nm:hover{color:var(--marca);}"
  , ".card .br{font-size:0.82rem;color:var(--tinta-suave);}"
  , ".card .pr{font-size:1.02rem;font-weight:600;color:var(--etiqueta);}"
  , ".card .st{font-size:0.78rem;color:var(--tinta-suave);}"
  , ".card .cuerpo button{margin-top:auto;background:var(--marca);color:#fff;border:none;border-radius:8px;padding:0.55rem;cursor:pointer;font-size:0.9rem;font-weight:600;}"
  , ".card .cuerpo button:disabled{background:var(--linea-fuerte);cursor:default;}"
  , ".card .cuerpo button:hover:enabled{background:var(--marca-osc);}"
  -- tables
  , "table{border-collapse:collapse;width:100%;background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);overflow:hidden;}"
  , "th,td{padding:0.6rem 0.8rem;text-align:left;border-bottom:1px solid var(--papel);font-size:0.92rem;}"
  , "th{background:var(--papel);font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:0.72rem;text-transform:uppercase;letter-spacing:0.1em;color:var(--tinta-suave);font-weight:500;}"
  , "tr:last-child td{border-bottom:none;}"
  , ".qtybtn{border:1px solid var(--linea-fuerte);background:var(--blanco);border-radius:6px;width:1.8rem;height:1.8rem;cursor:pointer;}"
  , ".qtybtn:hover{border-color:var(--marca);}"
  , ".danger{color:#b3261e;background:none;border:none;cursor:pointer;font-size:0.85rem;text-decoration:underline;}"
  , ".total{font-size:1.15rem;font-weight:600;margin:1rem 0;text-align:right;}"
  , ".total b,.pr-mono{font-family:'IBM Plex Mono',ui-monospace,monospace;color:var(--etiqueta);}"
  -- forms
  , ".field{display:flex;flex-direction:column;gap:0.25rem;margin-bottom:0.8rem;}"
  , ".field label{font-size:0.82rem;color:var(--tinta-suave);}"
  , ".field input,.field select,.field textarea{padding:0.55rem 0.8rem;border:1px solid var(--linea-fuerte);border-radius:8px;font-size:0.95rem;background:var(--blanco);font-family:inherit;color:var(--tinta);}"
  , ".field textarea{min-height:7rem;resize:vertical;}"
  , ".formgrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));column-gap:1rem;}"
  , ".cta{background:var(--marca);color:#fff;border:none;border-radius:8px;padding:0.7rem 1.4rem;font-weight:700;font-size:1rem;cursor:pointer;}"
  , ".cta:hover{background:var(--marca-osc);}"
  , ".cta:disabled{background:var(--linea-fuerte);cursor:default;}"
  , ".cta.contorno{background:transparent;color:var(--marca);border:2px solid var(--marca);}"
  , ".cta.contorno:hover{background:var(--marca-tinte);}"
  , ".muted{color:var(--tinta-suave);font-size:0.9rem;}"
  , ".error{color:#b3261e;font-size:0.9rem;margin:0.5rem 0;}"
  , ".ok{color:#1b7f4b;font-size:0.95rem;margin:0.5rem 0;}"
  , ".panel{background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);padding:1.4rem;margin-bottom:1.2rem;}"
  -- order status badges
  , ".badge{display:inline-block;border-radius:999px;padding:0.15rem 0.7rem;font-size:0.78rem;font-weight:600;}"
  , ".badge.created{background:#e8edec;color:#3c504d;}"
  , ".badge.pending_payment{background:#fdf1dc;color:#8a5b00;}"
  , ".badge.paid{background:#e0f2e9;color:#1b7f4b;}"
  , ".badge.shipped{background:var(--cielo-tinte);color:#0b5f8a;}"
  , ".badge.completed{background:#e5e5f7;color:#4740a6;}"
  , ".badge.cancelled{background:#fbe3e1;color:#b3261e;}"
  , ".rowbtns{display:flex;gap:0.5rem;flex-wrap:wrap;margin-top:0.8rem;}"
  , ".rowbtns button{border:1px solid var(--linea-fuerte);background:var(--blanco);border-radius:8px;padding:0.45rem 0.9rem;cursor:pointer;font-size:0.88rem;}"
  , ".rowbtns button:hover{background:var(--papel);}"
  , ".guia{background:var(--blanco);border:1px dashed var(--linea-fuerte);border-radius:8px;padding:1.2rem;white-space:pre-wrap;font-size:0.9rem;}"
  , ".h2{font-size:1.25rem;margin:0 0 1rem;font-stretch:112%;font-weight:750;}"
  -- KPI cards
  , ".kpis{display:flex;gap:1rem;flex-wrap:wrap;margin-bottom:1.2rem;}"
  , ".kpi{background:var(--blanco);border:1px solid var(--linea);border-radius:var(--radio);padding:0.8rem 1.2rem;min-width:130px;}"
  , ".kpi .n{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:1.5rem;font-weight:600;color:var(--tinta);}"
  , ".kpi .l{font-size:0.72rem;color:var(--tinta-suave);text-transform:uppercase;letter-spacing:0.08em;}"
  , ".cartbadge{background:var(--etiqueta);color:#fff;font-weight:700;border-radius:999px;padding:0.05rem 0.5rem;font-size:0.8rem;margin-left:0.3rem;font-family:'IBM Plex Mono',ui-monospace,monospace;}"
  , "@media print { .topbar, .noprint { display:none !important; } body{background:#fff;} .wrap{max-width:none;padding:0;} }"
  ]
