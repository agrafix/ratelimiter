{-# LANGUAGE OverloadedStrings #-}
module Control.RateLimiter
    ( WindowSize(..), RateLimitMode(..), RateLimitConfig(..)
    , RateLimiter, newRateLimiter
    , isRateLimited, withRetryRateLimiter
    )
where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans
import Data.IORef
import Data.Time
import Data.Time.TimeSpan
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

data WindowSize
    = WsMinute
    | WsHour
    | WsSecond
    | WsDay
    deriving (Show, Eq, Enum, Bounded)

data RateLimitMode
    = RollingWindow !DiffTime
    | FixedWindow !WindowSize
    deriving (Show, Eq)

data RateLimitConfig
    = RateLimitConfig
    { rlc_mode :: !RateLimitMode
    , rlc_maximum :: !Int
    }

data Entry k
  = Entry
  { eTime :: !UTCTime
  , eKey :: !k
  }

data RateLimiterImpl k
    = RateLimiterImpl
    { rl_config :: !RateLimitConfig
    , rl_state :: !(IORef (Seq.Seq (Entry k)))
    }

newtype RateLimiter k
    = RateLimiter { _unRateLimiter :: V.Vector (RateLimiterImpl k) }

-- | Create a new rate limiter with a list of configurations
newRateLimiter :: V.Vector RateLimitConfig -> IO (RateLimiter k)
newRateLimiter cfgs =
    liftM RateLimiter $ forM cfgs $ \cfg ->
    do ref <- newIORef mempty
       pure $ RateLimiterImpl { rl_config = cfg, rl_state = ref }

-- | Check if a given key is rate limited. Use `()` if you don't need multiple keys
isRateLimited :: (Eq k, MonadIO m) => k -> RateLimiter k -> m Bool
isRateLimited key (RateLimiter rls) =
    anyM (isRateLimitedImpl key) $ V.toList rls

isRateLimitedImpl :: Eq k => MonadIO m => k -> RateLimiterImpl k -> m Bool
isRateLimitedImpl key rl =
    do now <- liftIO getCurrentTime
       (_, isLimited) <-
           liftIO $
           atomicModifyIORef' (rl_state rl) $ \xs ->
           do let (xs', bucketSize) =
                      currentBucketSize now key (rlc_mode $ rl_config rl) xs
              if bucketSize < rlc_maximum (rl_config rl)
                 then (xs' Seq.|> Entry now key, (bucketSize, False))
                 else (xs', (bucketSize, True))
       pure isLimited

-- | Retry action if rate limited after 1 second
withRetryRateLimiter :: Eq k => MonadIO m => k -> RateLimiter k -> m a -> m a
withRetryRateLimiter key rl action =
    do limited <- isRateLimited key rl
       if not limited
          then action
          else do liftIO $ sleepTS (seconds 1)
                  withRetryRateLimiter key rl action

currentBucketSize ::
    Eq k => UTCTime -> k -> RateLimitMode -> Seq.Seq (Entry k) -> (Seq.Seq (Entry k), Int)
currentBucketSize now key rlm times =
    let timeOfDay =
          timeToTimeOfDay (utctDayTime now)
        takeUntil =
            case rlm of
              RollingWindow dt ->
                  addUTCTime (fromRational . toRational $ (-1) * dt) now
              FixedWindow ws ->
                  case ws of
                    WsMinute ->
                        now {utctDayTime = timeOfDayToTime $ timeOfDay { todSec = 0}}
                    WsHour ->
                        now {utctDayTime = timeOfDayToTime $ timeOfDay { todMin = 0, todSec = 0}}
                    WsSecond ->
                        now { utctDayTime = fromInteger $ truncate (utctDayTime now) }
                    WsDay ->
                        now { utctDayTime = 0 }
        newSeq = Seq.takeWhileR (\el -> eTime el >= takeUntil) times
    in ( newSeq
       , Seq.length $ Seq.filter (\el -> eKey el == key) newSeq
       )
