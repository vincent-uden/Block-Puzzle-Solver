import Data.List (intercalate)

-- 3-Dimensional structures that go inside the grid --
data Block = Block [[[Char]]]
    deriving(Eq)

instance Show Block where
    show (Block xs) = tail $ foldr make2d "" xs
        where make2d = (\x acc -> "\n" ++ (intercalate "\n" x) ++ acc)

bContents :: Block -> [[[Char]]]
bContents (Block xs) = xs

-- Imaginative space where blocks are placed --
data Grid = Grid [[[Char]]]
    deriving(Eq)

instance Show Grid where
    show (Grid xs) = foldr make2d "" xs
        where make2d = (\x acc -> "\n" ++ (intercalate "\n" ["#" ++ y ++ "#" | y <- x]) ++ acc)

gContents :: Grid -> [[[Char]]]
gContents (Grid xs) = xs

-- Wrapper for grid, removes tediousness from creating grids
createGrid :: Int -> Int -> Int -> Char -> Grid
createGrid x y z = Grid . (replicate z) . (replicate y) . (replicate x)

placePoint :: [Char] -> Char -> Int -> [Char]
placePoint line point x = if line !! x /= ' ' then line else
    take x line ++ [point] ++ drop (x + 1) line

canPlaceShape :: [[Char]] -> [[Char]] -> Int -> Int -> Bool
canPlaceShape b g x y =  x + w <= length (head g) && y + h <= length g && allEmpty
    where h = length b
          w = length $ head b
          ysection = take h $ drop y g
          section = [take w (drop x line) | line <- ysection]
          flatSection = concat section
          flatShape = concat b
          filteredSection = [fst a | a <- zip flatSection flatShape, snd a /= ' ']
          allEmpty = and [l == ' ' | l <- filteredSection]

canPlaceBlock :: Block -> Grid -> Int -> Int -> Int -> Bool
canPlaceBlock block grid x y z = 
    x + w <= length (head (head g)) && 
    y + h <= length (head g) && 
    z + d <= length g &&
    allEmpty
        where
            b = bContents block
            g = gContents grid
            w = length $ head $ head b
            h = length $ head b
            d = length b
            layers = [canPlaceShape (fst a) (snd a) x y| a <- zip b g]
            allEmpty = and layers

placeBlock :: Block -> Grid -> Int -> Int -> Int -> Maybe Grid
placeBlock block grid x y z = if canPlaceBlock block grid x y z 
                              then Just newGrid 
                              else Nothing
    where b = bContents block
          g = gContents grid
          w = length $ head $ head b
          h = length $ head b
          d = length b
          affectedPlanes = take d $ drop z g
          newGrid = Grid $ overWriteGrid b g x y z

overWriteGrid :: [[[Char]]] -> [[[Char]]] -> Int -> Int -> Int -> [[[Char]]]
overWriteGrid b g x y z = newGrid
    where 
        gw = length $ head $ head g
        gh = length $ head g
        gd = length g
        w = length $ head $ head b
        h = length $ head b
        d = length b
        padd = (\xs l -> xs ++ [' ' | y <- [1.. l - length xs]])
        paddY = (\lx ly xss -> xss ++ [padd "" lx | i <- [1..ly - length xss]])
        paddZ = (\lx ly lz xss -> xss ++ [paddY lx ly [] | i <- [1..ly - length xss]])
        flatG = concat $ concat $ g
        flatB = concat $ concat $ paddZ gw gh gd $ map (paddY gw gh) [[padd row (gw) | row <- plane] | plane <- b]
        strRot = (\l offset -> take (length l) $ drop (offset `mod` (length l)) $ cycle l)
        -- Rotate to take x y z offset into account
        rottedB = strRot flatB ((-1) * (x + y * gw + z * gw * gh))
        newFlat = foldr (\pair acc -> if snd pair /= ' ' then (snd pair):acc else (fst pair):acc) "" (zip flatG rottedB)
        newGrid = arrWrapper newFlat gw gh

construct2dArr :: [Char] -> Int -> [[Char]]
construct2dArr "" _ = []
construct2dArr g w = (take w g):(construct2dArr (drop w g) w)

construct3dArr :: [[Char]] -> Int -> Int -> [[[Char]]]
construct3dArr [] _ _ = []
construct3dArr g w h = (take h g):(construct3dArr (drop h g) w h)

arrWrapper g w h = construct3dArr (construct2dArr g w) w h

-- All building blocks --
bZ = Block [["ZZ ", " ZZ"]]
bz = Block [["zz ", " zz"]]
bT = Block [["TTT", " T "]]
bt = Block [["ttt", " t "]]
bC = Block [["CC", " C"], [" C", "  "]]
bg = Block [["gg", " g"], ["g ", "  "]]
bv = Block [["v ", "vv"]]

testGrid = createGrid 3 3 3 ' '
testShape = head $ bContents bZ
testLattice = ["  x", "x  ", " x "]
testGrid2 = Grid [testLattice, testLattice, testLattice]
testShape2 = bContents bZ
testLattice2 = [testLattice, testLattice, testLattice]
