module Main where

  ----------------------
  -- Exploratory work --
  ----------------------
  {-
     TODO: 1. Remove pixel from network.
     TODO: 2. Display network.
     TODO: 3. Optimise trav function - calculate complexity..
  -}

  type Value = String
  type Loc = [Int]
                                  -- L     R     U     D
  data Pixel = Dead | Live Value Loc Pixel Pixel Pixel Pixel
              deriving(Show)

  instance Eq Pixel where
    --(==) :: Eq => Pixel -> Pixel -> Bool
    (==) Dead Dead = True
    (==) (Live _ l1 _ _ _ _) (Live _ l2 _ _ _ _) = l1 == l2
    (==) _ _ = False

  px00, px01, px1, px2, px3, px4 :: Pixel

  px00 = Live "0" [2,0] Dead px01 Dead Dead
  px01 = Live "1" [2,1] px00 px02 px03 Dead
  px02 = Live "2" [2,2] px01 Dead Dead Dead
  px03 = Live "3" [2,3] Dead Dead Dead px01

  px0 = Live "0" [0,-1] Dead Dead px1 Dead
  px1 = Live "1" [0,0]  Dead px4 px2 px0
  px2 = Live "2" [0,1]  Dead px3 Dead px1
  px3 = Live "3" [1,1]  px2 Dead Dead px4
  px4 = Live "4" [1,0]  px1 Dead px3 Dead

  px5 = Live "5" [0,5] Dead Dead Dead Dead

  px6 = Live "0" [0] px7 Dead Dead Dead
  px7 = Live "1" [1] Dead px7 Dead Dead

  getValue :: Pixel -> Value
  --Pre: Pixel is live.
  getValue (Live v _ _ _ _ _) = v

  getLocation :: Pixel -> Loc
  getLocation (Live _ l _ _ _ _) = l

  getAbove :: Pixel -> Pixel
  --Pre: Pixel is alive.
  getAbove (Live _ _ _ _ p _) = p

  getBelow :: Pixel -> Pixel
    --Pre: Pixel is alive.
  getBelow (Live _ _ _ _ _ p) = p

  getLeft :: Pixel -> Pixel
    --Pre: Pixel is alive.
  getLeft (Live _ _ p _ _ _) = p

  getRight :: Pixel -> Pixel
    --Pre: Pixel is alive.
  getRight (Live _ _ _ p _ _) = p

  isLive :: Pixel -> Bool
  isLive Dead = False
  isLive _    = True

  isDead :: Pixel -> Bool
  isDead = not . isLive

  addPixelLeft, addPixelRight, addPixelAbove, addPixelBelow, addPixelToNetwork
   :: Pixel -> Pixel -> Pixel

  addPixelLeft p Dead = p
  addPixelLeft Dead p = p
  addPixelLeft (Live v1 loc1 l1 r1 a1 b1) (Live v loc l r a b)
    = let newPix  = Live v loc network r a b
          network = Live v1 loc1 l1 newPix a1 b1
      in network

  addPixelRight p Dead = p
  addPixelRight Dead p = p
  addPixelRight (Live v1 loc1 l1 r1 a1 b1) (Live v loc l r a b)
    = let newPix  = Live v loc l network a b
          network = Live v1 loc1 newPix r1 a1 b1
        in network

  addPixelAbove p Dead = p
  addPixelAbove Dead p = p
  addPixelAbove (Live v1 loc1 l1 r1 a1 b1) (Live v loc l r a b)
    = let newPix  = Live v loc l r network b
          network = Live v1 loc1 l1 r1 a1 newPix
      in network

  addPixelBelow p Dead = p
  addPixelBelow Dead p = p
  addPixelBelow (Live v1 loc1 l1 r1 a1 b1) (Live v loc l r a b)
   = let newPix  = Live v loc l r b network
         network = Live v1 loc1 l1 r1 newPix a1
     in network

  -- Adds pixel at next free spot on node.
  addPixelToNetwork p Dead = p
  addPixelToNetwork Dead p = p
  addPixelToNetwork p1@(Live v1 loc1 l1 r1 a1 b1) p@(Live v loc l r a b)
    | isDead l = addPixelLeft p1 p
    | isDead r = addPixelRight p1 p
    | isDead a = addPixelAbove p1 p
    | isDead b = addPixelBelow p1 p


  splitNetworkHoriz, splitNetworkVert :: Pixel -> (Pixel, Pixel)
  --Pre: Not dead network.
  splitNetworkHoriz (Live v loc l r a b)
     = let Live v1 loc1 l1 r1 a1 b1 = l
           leftSplit  = Live v1 loc1 l1 Dead a1 b1
           rightSplit = Live v loc Dead r b a
       in (leftSplit, rightSplit)

  splitNetworkVert (Live v loc l r a b)
     = let Live v1 loc1 l1 r1 a1 b1 = a
           aboveSplit  = Live v1 loc1 l1 r1 a1 Dead
           belowSplit = Live v loc l r Dead b
       in (aboveSplit, belowSplit)

  trav :: Pixel -> [Value]
  trav Dead = []
  trav p
    = fst (traversePixels p [])
    where
      traversePixels :: Pixel -> [Loc] -> ([Value], [Loc])
      traversePixels Dead ls = ([], ls)
      traversePixels p' ls | elem (getLocation p') ls = ([], ls)
      traversePixels p' ls
        = (getValue p' : vals1 ++ vals1' ++ vals1'' ++ vals1''', belowLocs')
        where
          (vals1, leftLocs') = traversePixels (getLeft p') (l : ls)
          (vals1' , rightLocs') = traversePixels (getRight p') leftLocs'
          (vals1'', aboveLocs') = traversePixels (getAbove p') rightLocs'
          (vals1''', belowLocs') = traversePixels (getBelow p') aboveLocs'
          l = getLocation p'

 {- mapNetwork :: Pixel -> String
  mapNetwork Dead = "Dead network."
  mapNetwork p
    = (convertArrayMap . mapNetworkIntoArray) p
    where
    --Pre: Sub array represents a row.
      convertArrayMap :: [String] -> String
      convertArrayMap
        = foldl1 (\x y -> x ++ "\n" ++ y)

      mapNetworkIntoArray :: Pixel -> [Loc] -> ([String], [Loc])
      mapNetworkIntoArray (Live v loc l r a b) ls = undefined
-}


  traversePixels :: Pixel -> [Loc] -> ([Value], [Loc])
  traversePixels Dead ls = ([], ls)
  traversePixels p' ls | elem (getLocation p') ls = ([], ls)
  traversePixels p' ls
    = (getValue p' : ["L["] ++ vals1 ++ ["] R["] ++ vals1' ++ ["] A["] ++
    vals1'' ++ ["] B["] ++ vals1''' ++ ["]"],
    belowLocs')
    where
      (vals1, leftLocs') = traversePixels (getLeft p') (l : ls)
      (vals1' , rightLocs') = traversePixels (getRight p') leftLocs'
      (vals1'', aboveLocs') = traversePixels (getAbove p') rightLocs'
      (vals1''', belowLocs') = traversePixels (getBelow p') aboveLocs'
      l = getLocation p'