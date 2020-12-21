# Haskell: ratelimiter

A simple in-memory rate-limiter library.

## Usage

``` haskell
import Control.RateLimiter
import qualified Data.Vector as V

main :: IO
main =
  -- one rate limiter can have multiple rules
  do limiter <- 
        newRateLimiter $ V.fromList
        [ RateLimitConfig (RollingWindow 60) 200 -- 200 per second
        , RateLimitConfig (RollingWindow 3600) 400 -- 400 per hour
        ]
     let myRateLimitedFunction =
           do isLimited <- isRateLimited () limiter
              if isLimitd then pure Nothing else Just <$> someExpensiveWork
     -- ... use myRateLimitedFunction
```

