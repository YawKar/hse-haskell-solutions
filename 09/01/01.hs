-- тип Board и функция next :: Board -> [Board] определены в вызывающем коде
doNTurns :: Int -> Board -> [Board]
doNTurns 0 ini = [ini]
doNTurns n ini = concatMap (doNTurns (n - 1)) $ next ini
