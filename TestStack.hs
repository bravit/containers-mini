import Data.Stack

main = do
  let st = push 15 $ push 10 $ push 5 $ push 0 empty
      -- st == 0, 5, 10, 15 <<-- top
      st' = pop $ pop st -- 0, 5 <<-- top
      st'' = push 100 st' -- 0, 5, 100 <<-- top
      shouldBeTrue = [ top st' == Just 5,
                       top st'' == Just 100,
                       isEmpty $ pop $ pop st']
  print $ and shouldBeTrue
