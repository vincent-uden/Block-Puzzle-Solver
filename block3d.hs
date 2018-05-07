import Data.List (intercalate, transpose, nub)

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

dims :: Grid -> (Int, Int, Int)
dims g = (w, h, d)
    where
        c = gContents g
        d = length c
        h = length $ head c
        w = length $ head $ head c

maybeExtract :: Maybe Grid -> Grid
maybeExtract Nothing = Grid [[""]]
maybeExtract (Just g) = g

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

padd = (\xs l -> xs ++ [' ' | y <- [1.. l - length xs]])
paddY = (\lx ly xss -> xss ++ [padd "" lx | i <- [1..ly - length xss]])
paddZ = (\lx ly lz xss -> xss ++ [paddY lx ly [] | i <- [1..ly - length xss]])
overWriteGrid :: [[[Char]]] -> [[[Char]]] -> Int -> Int -> Int -> [[[Char]]]
overWriteGrid b g x y z = newGrid
    where 
        gw = length $ head $ head g
        gh = length $ head g
        gd = length g
        w = length $ head $ head b
        h = length $ head b
        d = length b
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

rotate2dArr :: [[Char]] -> Int -> [[Char]]
rotate2dArr arr 0 = arr
rotate2dArr arr r = rotate2dArr (transpose $ [reverse row | row <- arr]) (r-1)

rotateX :: [[[Char]]] -> Int -> [[[Char]]]
rotateX arr 0 = arr
rotateX arr r = rotateX (transpose $ [reverse plane| plane <- arr]) (r-1)

rotateY :: [[[Char]]] -> Int -> [[[Char]]]
rotateY arr 0 = arr
rotateY arr r = rotateY (rotateX (rotateZ (rotateX arr 1) 1) 3) (r - 1)

rotateZ :: [[[Char]]] -> Int -> [[[Char]]]
rotateZ arr r = [rotate2dArr plane r | plane <- arr]

rotate3dArr :: [[[Char]]] -> Char -> Int -> [[[Char]]]
rotate3dArr arr axis r = case axis of 
        'x' -> rotateX arr r -- Rotates cube on x axis backwards 
        'y' -> rotateY arr r -- Rotates cube clockwise as seen from above
        'z' -> rotateZ arr r

depthFirst :: [Block] -> Grid -> [Grid]
--depthFirst :: [Block] -> Grid -> Int
depthFirst [] inpGrid = [inpGrid]
depthFirst blocks inpGrid = allGrids
    where
        bs = [bContents b | b <- blocks]
        rotations = (\b -> map Block (concat $ concat $ [[[rotateZ (rotateY (rotateX b x) y) z | x <- [0..3]] | y <- [0..3]] | z <- [0..3]]))
        allRots = nub $ rotations $ head bs
        (dx, dy, dz) = dims inpGrid
        placements = concat $ concat $ concat $ [[[[placeBlock rot inpGrid x y z | x <- [0..(dx - 1)]] | y <- [0..(dy - 1)]] | z <- [0..(dz - 1)]] | rot <- allRots]
        maybeGrids = filter (/=Nothing) placements
        actualGrids = map maybeExtract maybeGrids
        newGrids = map (depthFirst (tail blocks)) actualGrids
        allGrids = foldr (\acc item -> item ++ [Grid [["qq", "qq"], ["qq", "qq"], ["qq", "qq"]]] ++ acc) [] newGrids

-- All building blocks --
bZ = Block [["ZZ ", " ZZ"]]
bz = Block [["zz ", " zz"]]
bT = Block [["TTT", " T "]]
bt = Block [["ttt", " t "]]
bC = Block [["CC", " C"], [" C", "  "]]
bg = Block [["gg", " g"], ["g ", "  "]]
bv = Block [["v ", "vv"]]
bV = Block [["V ", "VV"]]
bi = Block [["ii"]]

-- Try to fill this grid with 2 x bv, 1 x bi
-- Answer will be [["vV", "vv"], ["VV", "ii"]]
testGrid = createGrid 2 2 2 ' '

tes = head $ bContents bZ
--x + w <= length (head g) && y + h <= length g &&
canPlaceShapeP :: [[Char]] -> [[Char]] -> Int -> Int -> ([Char], [Char])
canPlaceShapeP b g x y = allEmpty
    where h = length b
          w = length $ head b
          ysection = take h $ drop y g
          section = [take w (drop x line) | line <- ysection]
          flatSection = concat section
          flatShape = concat b
          filteredSection = [fst a | a <- zip flatSection flatShape, snd a /= ' ']
          allEmpty = (flatSection, flatShape)



canPlaceBlockP :: Block -> Grid -> Int -> Int -> Int -> [Bool] 
canPlaceBlockP block grid x y z = 
--    x + w <= length (head (head g)) && 
--    y + h <= length (head g) && 
--    z + d <= length g &&
    allEmpty
        where
            b = bContents block
            g = gContents grid
            w = length $ head $ head b
            h = length $ head b
            d = length b
            gw = length $ head $ head g
            gh = length $ head g
            gd = length g
            paddedB = paddZ gw gh gd b
            layers = [canPlaceShape bLayer gLayer x y| (bLayer, gLayer) <- zip paddedB g]
            allEmpty = layers
problem1 = placeBlock bv testGrid 0 0 1
problem = canPlaceBlockP bV (maybeExtract (  placeBlock bv testGrid 0 0 1) )0 0 1
