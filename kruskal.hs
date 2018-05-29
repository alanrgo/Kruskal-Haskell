import Control.Monad
import Data.Char
import System.IO (isEOF)
import qualified Data.List as L

data Point = Point Char [Float] deriving (Show)
data SetID = SetID Char Integer deriving (Show)
data Edge = Edge [Char] Float deriving (Show)
data Graph = Graph [SetID] [Edge] deriving (Show)

main = do 
    contents <- getContents
    let allLines = lines contents
        nGroup = read (head allLines)::Int
        listOfPoints = mkPoints $ tail allLines
        graph = compareW $ createGraph listOfPoints (Graph [] []) 0
        kGraph = kruskal graph
        tuples = listGroupsBySet $ compareSet $ updateSet $ removeNEdges kGraph (nGroup - 1)
    printOrganized $ compareTuples tuples

-- Return a list os Points from a list of Strings representing the lines -- 
mkPoints :: [String] -> [Point]
mkPoints lines = mkPoints' lines []
    where mkPoints' [] l = l
          mkPoints' (x:xs) l = mkPoints' xs ((toPoint x):l)

-- Get a String which starts with a char and then numbers, and turn it into a Point structure -- 
toPoint :: String -> Point
toPoint x = 
    let list = words x
    in (Point (head $ head list) (map read $ tail list) )

-- From two Points, compute weight -- 
computeWeight :: Point -> Point -> Float
computeWeight (Point n1 x) (Point n2 y) = computeWeight' x y 0
    where computeWeight' [] _ sum = sqrt sum
          computeWeight' (p:ps) (q:qs) sum = computeWeight' ps qs (sum + ((p-q)**2))


-- From a list of Points, return the correpondent Graph -- 
createGraph :: [Point] -> Graph -> Integer -> Graph
createGraph [] nGraph _ = nGraph
createGraph ((Point a p):xs) (Graph ns ed) i = 
    let edges = getEdges (Point a p) xs
    in createGraph xs (Graph ((SetID a i):ns) (edges ++ ed)) (i+1)

-- Create Edges from one point to all others in a list -- 
getEdges :: Point -> [Point] -> [Edge]
getEdges a ps = getEdges' a ps []
    where getEdges' _ [] list = list
          getEdges' (Point a l1) ((Point b l2):xs) list = getEdges' (Point a l1) xs ((Edge [a, b] (computeWeight (Point a l1) (Point b l2))):list)

-- return the list of edges --
edgeList :: Graph -> [Edge]
edgeList (Graph _ l) = l

-- sort edges based on their weight -- 
compareW :: Graph -> Graph
compareW (Graph a eds ) = 
    let list = L.sortBy (\(Edge _ x) (Edge _ y) -> compare x y ) eds
    in (Graph a list)

kruskal :: Graph -> Graph
kruskal gOr = kruskal' gOr (Graph [] [])
    where kruskal' (Graph _ [] ) mst = mst
          kruskal' (Graph sets (e:es) ) (Graph _ ceds) = 
              let seta = getFstSet sets e
                  setb = getSndSet sets e
                  isUnion = seta /= setb
                  sets' = if isUnion then union sets seta setb else sets
              in if isUnion then kruskal' (Graph sets' es) (Graph sets' (e:ceds)) else kruskal' (Graph sets' es) (Graph sets' ceds)

getFstSet :: [SetID] -> Edge -> Integer
getFstSet sets (Edge ed _ ) = findInSet sets $ head ed

getSndSet :: [SetID] -> Edge -> Integer
getSndSet sets (Edge ed _ ) = findInSet sets $ (head $ tail ed)

findInSet :: [SetID] -> Char -> Integer
findInSet [] _ = -1
findInSet ((SetID x n):xs) targ = if x == targ then n else findInSet xs targ

union :: [SetID] -> Integer -> Integer -> [SetID]
union sets seta setb = union' sets seta setb []
    where union' [] _ _ nSet = nSet
          union' ((SetID c n):ss) a b nSet = if b == n then union' ss a b ((SetID c a):nSet) else union' ss a b ((SetID c n):nSet)

getGroups g nGroups = updateSet $ removeNEdges g nGroups

removeNEdges g 0 = g
removeNEdges (Graph a (e:es)) n = removeNEdges (Graph a es) (n-1)

updateSet (Graph sets edges) = 
    let sets' = initializeSet sets 
    in computeSets (Graph sets' edges)

computeSets (Graph sets eds) = computeSets' (Graph sets eds) eds
    where computeSets' g [] = g
          computeSets' (Graph sets ed) (e:es) = 
              let seta = getFstSet sets e
                  setb = getSndSet sets e
                  sets' = union sets seta setb
              in computeSets' (Graph sets' ed) es

initializeSet sets = initializeSet' sets [] 0
    where initializeSet' [] nSet _ = nSet
          initializeSet' ((SetID a _):ss) nSet i = initializeSet' ss ((SetID a i):nSet) (i+1)    


listGroupsBySet (Graph sets _) = listGroups sets 

listGroups ((SetID c i):ss) = listGroups' ((SetID c i):ss) [] 
    where listGroups' [] array = array 
          listGroups' ((SetID c i):ss) array = listGroups' ss ((c, i):array)

compareSet (Graph sets ed) = 
    let sets' = L.sortBy (\(SetID _ a) (SetID _ b) -> compare a b ) sets
    in (Graph sets' ed)

compareTuples x = L.sortBy (\(_,b) (_, d) -> compare b d ) x


printOrganized [] = return ()
printOrganized ((a,b):xs) = printOrganized' xs b [a]
    where printOrganized' [] _ array = print array
          printOrganized' ((a,b):xs) t array = if b == t then printOrganized' xs t (a:array)
                                                         else do print $ L.sort array
                                                                 printOrganized' xs b [a]




