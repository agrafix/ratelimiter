# Haskell: ratelimiter

A simple in-memory rate-limiter library.

## Usage

``` haskell
import Control.RateLimiter

main :: IO
main =
  do limiter <- newRateLimiter $ RateLimitConfig (RollingWindow 60) 200
     let myRateLimitedFunction =
           do isLimited <- isRateLimited limiter
              if isLimitd then pure Nothing else Just <$> someExpensiveWork
     -- ... use myRateLimitedFunction
```

