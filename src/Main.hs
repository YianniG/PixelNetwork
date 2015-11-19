module Main where

  ----------------------
  -- Exploratory work --
  ----------------------
  type Value = Int
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

  px00 = Live 0 [0,0] Dead px01 Dead Dead
  px01 = Live 1 [0,1] px00 px02 px03 Dead
  px02 = Live 2 [0,2] px01 Dead Dead Dead
  px03 = Live 3 [0,3] Dead Dead Dead px01

  px0 = Live 0 [0,-1] Dead Dead px1 Dead
  px1 = Live 1 [0,0]  Dead px4 px2 px0
  px2 = Live 2 [0,1]  Dead px3 Dead px1
  px3 = Live 3 [1,1]  px2 Dead Dead px4
  px4 = Live 4 [1,0]  px1 Dead px3 Dead

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