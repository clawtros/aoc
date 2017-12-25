module Ulam (getPosition) where


getPosition :: Int -> (Int, Int)
getPosition 1 = (0, 0)
getPosition n =
  let
    squared =
      sqrt $ fromIntegral n

    ring =
      ceiling $ (squared - 1) / 2
      
    ring2 =
      (2 * ring)
      
    l =
      (ring2 - 1) * (ring2 - 1)
      
    d =
      n - (l + 1)
    sd =
      if d > 0 then 
        d `mod` ring2
      else
        0
    s =
      if ring > 0 then
        quot d ring2
      else
        0
  in
    case s of
      1 ->
        (ring - 1 - sd, ring)
      2 ->
        (-ring, (ring - 1) - sd)
      3 ->
        (-ring + 1 + sd, -ring)
      _ ->
        (ring, (-ring + 1) + sd)


test :: Int -> IO ()
test n =
  let
    pos = map getPosition [1..n]
  in
    mapM_ (putStrLn . show) pos
  
  
