--https://kseo.github.io/posts/2017-01-21-writer-monad.html

import Control.Monad
import Control.Monad.Trans.Writer.Strict

newtype LogEntry = LogEntry { msg::String }
  deriving (Eq, Show)

calc :: Writer [LogEntry] Integer
calc = do
  output "start"
  let x = sum [1..10000000]
  output (show x)
  output "done"
  return x

output :: String -> Writer [LogEntry] ()
output x = tell [LogEntry x]

main = mapM_ print $ execWriter calc