module Debug where

import Prelude

import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)

debug :: forall a. String -> (Unit -> a) -> a
debug s x = unsafePerformEffect $ log s >>= \_ -> pure (x unit)
-- debug _ x = x unit
