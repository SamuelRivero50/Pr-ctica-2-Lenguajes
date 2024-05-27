-- Función Nicomanchus classification scheme:
nicomanchus :: Int -> String
nicomanchus x = nicomanchusAux x 1 0
    where
    nicomanchusAux x y z
        | y > (x `div` 2) && z > x = "Administrative"
        | y > (x `div` 2) && z < x = "Humanities"
        | y > (x `div` 2)          = "Engineering"
        | x `mod` y == 0 = nicomanchusAux x (y+1) (z+y)
        | otherwise  = nicomanchusAux x (y+1) z

--Función even or odd
evenodd :: Int -> String
evenodd x
    | x `mod` 2 == 0 = "even"
    | otherwise = "odd"

--Función que muestra el output
output :: Int -> String
output x =  
    let 
        twoFirst = x `div` 1000000
        third = (x `div` 100000) `mod` 10
        twoMiddle = (x `div` 1000) `mod` 100
        lastThree = x `mod` 1000
    in  
        "20" ++ show(twoFirst) ++ "-" ++ show(third) ++ " " ++ nicomanchus twoMiddle ++ " num" ++ show(lastThree) ++ " " ++ evenodd lastThree

-- Ejecutar función principal
main :: IO()
main = do
    -- Verificar la función promedio
    x <- readLn :: IO Int
    putStrLn $ (output x)
