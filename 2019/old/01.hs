fuel_needed :: Int -> Int
fuel_needed n = (n `div` 3) - 2

total_fuel :: Int -> Int
total_fuel n
  | fuel <= 0 = 0
  | otherwise = fuel total_fuel fuel
where
	fuel = fuel_needed n
