sum3 :: (Num a) => [a] -> [a] -> [a] -> [a]
sum3 as bs cs =
  if null as && null bs && null cs then
    []
  else if null as && null cs then
    bs
  else if null as && null bs then
    cs
  else if null bs && null cs then
    as
  else if null as then
    head bs + head cs : sum3 [] (tail bs) (tail cs)
  else if null bs then
    head as + head cs : sum3 (tail as) [] (tail cs)
  else if null cs then
    head as + head bs : sum3 (tail as) (tail bs) []
  else
    head as + head bs + head cs : sum3 (tail as) (tail bs) (tail cs)
