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

data RateLimiterImpl
    = RateLimiterImpl
    { rl_config :: !RateLimitConfig
    , rl_state :: !(IORef (Seq.Seq UTCTime))
    }

newtype RateLimiter
    = RateLimiter { _unRateLimiter :: V.Vector RateLimiterImpl }

newRateLimiter :: V.Vector RateLimitConfig -> IO RateLimiter
newRateLimiter cfgs =
    liftM RateLimiter $ forM cfgs $ \cfg ->
    do ref <- newIORef mempty
       pure $ RateLimiterImpl { rl_config = cfg, rl_state = ref }

isRateLimited :: MonadIO m => RateLimiter -> m Bool
isRateLimited (RateLimiter rls) =
    anyM isRateLimitedImpl $ V.toList rls

isRateLimitedImpl :: MonadIO m => RateLimiterImpl -> m Bool
isRateLimitedImpl rl =
    do now <- liftIO getCurrentTime
       (_, isLimited) <-
           liftIO $
           atomicModifyIORef' (rl_state rl) $ \xs ->
           do let (xs', bucketSize) =
                      currentBucketSize now (rlc_mode $ rl_config rl) xs
              if bucketSize < rlc_maximum (rl_config rl)
                 then (xs' Seq.|> now, (bucketSize, False))
                 else (xs', (bucketSize, True))
       pure isLimited

withRetryRateLimiter :: MonadIO m => RateLimiter -> m a -> m a
withRetryRateLimiter rl action =
    do limited <- isRateLimited rl
       if not limited
          then action
          else do liftIO $ sleepTS (seconds 1)
                  withRetryRateLimiter rl action

currentBucketSize ::
    UTCTime -> RateLimitMode -> Seq.Seq UTCTime -> (Seq.Seq UTCTime, Int)
currentBucketSize now rlm times =
    let timeOfDay = timeToTimeOfDay (utctDayTime now)
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
        newSeq = Seq.takeWhileR (>= takeUntil) times
    in (newSeq, Seq.length newSeq)
