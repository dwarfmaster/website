{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module NBlog where

import Prelude                  hiding (span)
import Data.Text                hiding (span)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Data.Monoid
import Data.Foldable
import Data.String
import Control.Monad.Error.Class
import Text.Pandoc.Definition
import Text.Pandoc.Class
import Text.Pandoc.Error
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.String

makeAttribute :: ToValue v => String -> v -> Attribute
makeAttribute name = customAttribute (fromString name) . toValue

ida  = makeAttribute "id"
href = makeAttribute "href"
alta = makeAttribute "alt"
srca = makeAttribute "src"


--  ___       _ _            
-- |_ _|_ __ | (_)_ __   ___ 
--  | || '_ \| | | '_ \ / _ \
--  | || | | | | | | | |  __/
-- |___|_| |_|_|_|_| |_|\___|
--                           

wrap :: PandocMonad m => (Html -> Html) -> [Inline] -> m Html
wrap effect inside = effect . fold <$> mapM inlineWriter inside

inlineWriter :: forall m. PandocMonad m => Inline -> m Html

inlineWriter (Str str)         = return $ toMarkup str
inlineWriter (Emph ins)        = wrap em ins
inlineWriter (Strong ins)      = wrap strong ins
inlineWriter (Strikeout ins)   = wrap del ins
inlineWriter (Superscript ins) = wrap sup ins
inlineWriter (Subscript ins)   = wrap sub ins
inlineWriter (SmallCaps ins)   = wrap (span ! ida ("smallcaps" :: Text)) ins
inlineWriter (Quoted _ ins)    = wrap q ins
inlineWriter (Span _ ins)      = wrap span ins
inlineWriter SoftBreak         = return br
inlineWriter LineBreak         = return br

-- Won't support citations for now

inlineWriter (Code _ cd) = return . pre . code . toMarkup $ cd

inlineWriter Space = return . toMarkup $ (" " :: String)

-- Won't support math for now

inlineWriter (RawInline _ raw) = return . pre . toMarkup $ raw

inlineWriter (Link _ ins (target,_)) = wrap (a ! href target) ins

inlineWriter (Image _ alt (target,_)) = altText >>= \altT -> return $ img ! srca target ! alta altT
  where altText :: m String
        altText = renderHtml . contents . fold <$> mapM inlineWriter alt

-- Won't support Note for now

inlineWriter inline = throwError $ PandocShouldNeverHappenError $ "Unsupported inline " <> show inline

