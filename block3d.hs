import Data.List (intercalate)

-- 3-Dimensional structures that go inside the grid --
data Block = Block [[[Char]]]
    deriving(Eq)

instance Show Block where
    show (Block xs) = tail $ foldr make2d "" xs
       where make2d = (\x acc -> acc ++ "\n" ++ (intercalate "\n" x))

bContents :: Block -> [[[Char]]]
bContents (Block xs) = xs

-- Imaginative space where blocks are placed --
data Grid = Grid [[[Char]]]
    deriving(Eq)

instance Show Grid where
    show (Grid xs) = tail $ foldr make2d "" xs
       where make2d = (\x acc -> acc ++ "\n" ++ (intercalate "\n" x))

gContents :: Grid -> [[[Char]]]
gContents (Grid xs) = xs

-- Wrapper for grid, removes tediousness from creating grids
createGrid :: Int -> Int -> Int -> Char -> Grid
createGrid x y z = Grid . (replicate z) . (replicate y) . (replicate x)

placePoint :: [Char] -> Char -> Int -> [Char]
placePoint line point x = if line !! x /= ' ' then line else
    take x line ++ [point] ++ drop (x + 1) line

--canPlaceShape :: [[Char]] -> [[Char]] -> Int -> Int -> Bool
canPlaceShape :: [[Char]] -> [[Char]] -> Int -> Int -> [Bool]
--canPlaceShape b g x y =  x + w < length (head g) && y + h < length g && allEmpty
canPlaceShape b g x y = allEmpty
    where h = length b
          w = length $ head b
          ysection = take h $ drop y g
          section = [take w (drop x line) | line <- ysection]
          flatSection = concat section
          flatShape = concat b
          filteredSection = [fst a | a <- zip flatSection flatShape, snd a /= ' ']
          allEmpty = [l == ' ' | l <- filteredSection]

--canPlaceBlock :: [[[Char]]] -> [[[Char]]] -> Int -> Int -> Int -> Bool
canPlaceBlock :: Block -> Grid -> Int -> Int -> Int -> [[Bool]]
canPlaceBlock block grid x y z = 
--    x + w < length (head (head g)) && 
--    y + h < length (head g) && 
--    z + d < length g &&
    allEmpty
        where
            b = bContents block
            g = gContents grid
            w = length $ head $ head b
            h = length $ head b
            d = length b
            layers = [canPlaceShape (fst a) (snd a) x y| a <- zip b g]
            allEmpty = layers

placeBlock :: Block -> Grid -> Int -> Int -> Int -> Grid
placeBlock b g x y z = Grid newcontents
    where newcontents = [["kek"]]

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
testLattice = ["   ", "   ", "   "]
