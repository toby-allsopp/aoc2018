module Profile (profile, profileM, summary) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.HashMap as HashMap
import Data.HashMap (HashMap)
import Data.List as List
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Console
import Effect.Ref as Ref
import Effect.Ref (Ref)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import System.Clock (nanoseconds)

data EventType
    = Enter String
    | Exit String

instance showEventType :: Show EventType where
    show (Enter label) = "(Enter " <> show label <> ")"
    show (Exit label) = "(Exit " <> show label <> ")"

type Event = {
    type :: EventType,
    time :: Number
}

type Info = {
    events :: List Event
}

info :: Ref Info
info = unsafePerformEffect $ Ref.new { events: List.Nil }

pushEvent :: Event -> Effect Unit
pushEvent e = Ref.modify_ (\i -> i { events = e List.: i.events }) info

profileM :: forall m a. Monad m => String -> m a -> m a
profileM label f =
    let entryTime = unsafePerformEffect nanoseconds in
    --pushEvent { type: Enter label, time: entryTime }
    do
        result <- f
        let exitTime = unsafePerformEffect nanoseconds
        let _ = unsafePerformEffect $ pushEvent { type: Exit label, time: exitTime - entryTime }
        pure result

profile :: forall a. String -> (Unit -> a) -> a
profile label f = unsafePerformEffect do
    entryTime <- nanoseconds
    --pushEvent { type: Enter label, time: entryTime }
    let result = f unit
    exitTime <- nanoseconds
    pushEvent { type: Exit label, time: exitTime - entryTime }
    pure result

summary :: Effect (Array { label :: String, totalTime :: Number, count :: Int })
summary = do
    i <- Ref.read info
    pure $ Array.sortWith (_.totalTime) $ HashMap.toArrayBy (\label {time, count} -> { label, totalTime: time / 1000000000.0, count }) $ _.m $ foldl f { m: HashMap.empty, stack: [] } $ List.reverse i.events
        where
            f :: { m :: HashMap String { time :: Number, count :: Int }, stack :: Array Event } -> Event -> { m :: HashMap String { time :: Number, count :: Int }, stack :: Array Event }
            f {m, stack} e = case e.type of
                Enter label -> {m, stack: Array.snoc stack e}
                Exit label ->
                    -- case Array.unsnoc stack of
                    --     Just { init, last } ->
                    --         let elapsed = e.time - last.time in
                            {m: HashMap.alter (Just <<< (\{time, count} -> {time: time + e.time, count: count + 1}) <<< fromMaybe { time: 0.0, count: 0 }) label m,
                            stack: []}--init}
                        -- Nothing -> unsafeCrashWith "?"