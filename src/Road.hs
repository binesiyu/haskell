module  Road() where

data Node = Node Road Road | EndNode Road

data Road = Road Int Node


data Section = Section {
             getA :: Int,
             getB :: Int,
             getC :: Int
             } deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30,
                   Section 5 90 20,
                   Section 40 2 5,
                   Section 10 8 0]

data Label = A | B | C deriving (Show)

type Path = [(Label,Int)]

roadStepA,roadStepB :: (Path,Path,Int,Int) -> Section -> (Path,Int)

roadStepA (pathA,pathB,priceA,priceB) (Section a b c) =
        let forwardPriceToA = priceA + a
            crossPriceToA = priceB + b + c
            (newPathToA,newPriceA) = if forwardPriceToA <= crossPriceToA
                             then ((A,a):pathA,forwardPriceToA)
                             else ((C,c):(B,b):pathB,crossPriceToA)
        in (newPathToA,newPriceA)

roadStepB (pathA,pathB,priceA,priceB) (Section a b c) =
        let forwardPriceToB = priceB + b
            crossPriceToB = priceA + a + c
            (newPathToB,newPriceB) = if forwardPriceToB <= crossPriceToB
                              then ((B,b):pathB,forwardPriceToB)
                              else ((C,c):(A,a):pathA,crossPriceToB)
        in (newPathToB,newPriceB)

roadStep :: (Path,Path,Int,Int) -> Section -> (Path,Path,Int,Int)
roadStep path section =
        let (newPathToA,newPriceA) = roadStepA path section
            (newPathToB,newPriceB)= roadStepB path section
        in (newPathToA,newPathToB,newPriceA,newPriceB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
        let (bestAPath,bestBPath,_,_) = foldl roadStep ([],[],0,0) roadSystem
        in if sum (map snd bestAPath) <= sum (map snd bestBPath)
                   then reverse bestAPath
                   else reverse bestBPath


