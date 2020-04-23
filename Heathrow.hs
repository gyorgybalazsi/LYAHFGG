data Label = A | B | C deriving (Eq, Show)

data Path = Path {labels :: [Label], time :: Int} deriving (Eq, Show)

instance Semigroup Path 
   where (<>) p1 p2 = Path {labels = labels p1 ++ labels p2, time = time p1 + time p2}

instance Monoid Path 
  where mempty = Path {labels = [], time =0}
        mappend = (<>)

instance Ord Path where
  compare p1 p2 | time p1 < time p2 = LT
                | time p1 == time p2 = EQ
                | time p1 > time p2 = GT

data Section = Section {pathA :: Path, pathB :: Path, pathC:: Path}

type RoadSystem = [Section]

type DrivePlan = (Path, Path)

step :: DrivePlan -> Section -> DrivePlan
step (pA, pB) s =
    let 
        pA' | (pA <> pathA s) <= (pB <> pathB s <> pathC s) = pA <> pathA s
            | otherwise = pB <> pathB s <> pathC s
        pB' | (pB <> pathB s) <= (pA <> pathA s <> pathC s) = pB <> pathB s
            | otherwise = pA <> pathA s <> pathC s
    in (pA', pB')

heathrow = [
    Section { 
        pathA = Path {
              labels = [A]
            , time = 50
            },
        pathB = Path {
              labels = [B]
            , time = 10
            },
        pathC = Path {
              labels = [C]
            , time = 30
            }
        },
    Section { 
        pathA = Path {
                labels = [A]
            , time = 5
            },
        pathB = Path {
                labels = [B]
            , time = 90
            },
        pathC = Path {
                labels = [C]
            , time = 20
            }
        },
    Section { 
        pathA = Path {
                labels = [A]
            , time = 40
            },
        pathB = Path {
                labels = [B]
            , time = 2
            },
        pathC = Path {
                labels = [C]
            , time = 25
            }
        },
    Section { 
        pathA = Path {
                labels = [A]
            , time = 10
            },
        pathB = Path {
                labels = [B]
            , time = 8
            },
        pathC = Path {
                labels = [C]
            , time = 0
            }
        }
            ]

plan = scanl step (mempty, mempty) heathrow

