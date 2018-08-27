
import Data.List          -- sort
import Data.Array
import Data.Maybe
import Control.Parallel.Strategies
import Data.List.Split    -- split
import System.Environment -- getArgs


-- Types
type Vertex = Int
type Edge = (Vertex, Vertex)
type Grid = Array Vertex (Maybe [Vertex]) -- edge를 array로 표현함

-- (min id, max id)
box :: [Edge] -> Edge
box is = ( minID, maxID)
    where minID = minimum $ [ a | (a,b) <- is ] ++ [ b | (a,b) <- is]
          maxID = maximum $ [ a | (a,b) <- is ] ++ [ b | (a,b) <- is]

-- string g를 입력으로 받아 Edge형태로 바꿔준다
parsePair :: String -> Maybe Edge
parsePair g = if length li == 2 then
                   Just ( read (li !! 0) :: Int, read (li !! 1) :: Int)
              else Nothing
  where li = splitOn " " g
  
-- 입력을 Grid형태로 바꿈
-- 입력이 잘못 됐으면 Nothing
parseGrid :: [String] -> Maybe Grid
parseGrid ss = do
  pair <- mapM parsePair ss
  let ids = nub $ [ a | (a,b) <- pair ] ++ [ b | (a,b) <- pair]  -- 존재하는 모든 vertex들
  return $ array (box pair) [ (id, Just $ (sort.nub) [ a | (a,b) <- pair, b == id ] ++ [ b | (a,b) <- pair, a == id])
                            | id <- ids]

-- vertex v와 연결된 vertex들을 보여준다
-- v가 이미 지워진 상태이면 (Nothing이면) 결과도 Nothing 
-- 입력은 grid, vertex v, grid에 존재하는 모든 vertex이다
neighbor :: Maybe Grid -> Vertex -> [Vertex] -> Maybe [Vertex]
neighbor mg v [] = Just []
neighbor mg v (s:ss) = do
  g <- mg
  neigh <- neighbor mg v ss  -- 다른 edge들도 연결 되어 있는지 재귀로 확인한다
  case v `melem` (g ! s) of  -- s가 v와 연결 됐는지 확인한다
                             -- grid에서 s의 값에 v가 있는지 확인한다
    Just True  -> return $ s : neigh  -- s가 이웃이면 s를 포함하여 반환한다
    _          -> return neigh
  where melem :: Vertex -> Maybe [Vertex] -> Maybe Bool
        melem val mlist = do
          l <- mlist
          return (elem val l)

-- vertex n과 연결된 모든 edge를 지운다
-- (v:vs)에서 n을 삭제한다
-- 입력은 grid, n과 연결된 vertex들 (v:vs), vertex n이어야 한다
eliminateEdge :: Maybe Grid -> [Vertex] -> Vertex -> Maybe Grid
eliminateEdge mg [] n = mg
eliminateEdge mg (v:vs) n = eliminateEdge newGrid vs n -- v삭제 후, 재귀로 호출하여 vs도 삭제
  where newGrid = do -- v를 삭제한 grid
          g <- mg
          edges <- g ! v
          return (g // [(v, Just ( delete n edges))])

-- vertex v를 삭제한다
-- vertex v와 연결된 모든 edge들을 삭제한다
eliminateVertex :: Maybe Grid -> Vertex  -> Maybe Grid
eliminateVertex mg v = do
  g <- mg
  nei <- neighbor mg v (indices g)        -- v의 이웃 구하기
  gridWithoutN <- eliminateEdge mg nei v  -- 이웃 삭제
  newGrid <- Just $ gridWithoutN // [(v,Nothing)] -- v삭제
  if (all (== Nothing) (elems newGrid))
    then Nothing       -- 모든 vertex가 삭제되면 끝냄
  else return newGrid

-- vertex v와 v의 이웃들(n:ns)을 삭제한다
-- 입력으로 grid와 vertex v를 받는다
eliminateNeigh :: Maybe Grid -> Vertex -> Maybe Grid
eliminateNeigh mg v = do
  g <- mg
  nei <- neighbor mg v (indices g)        -- v의 이웃들
  eliminate (eliminateVertex mg v) v nei  -- v를 삭제하고, eliminate를 호출하여 이웃들 삭제
   where -- eliminateVertex를 모든 이웃에 대하여 호출한다
    eliminate :: Maybe Grid -> Vertex -> [Vertex]-> Maybe Grid
    eliminate mg v' [] = mg
    eliminate mg v' (n:ns) =
      if (all (== Nothing) (elems (fromJust mg))) -- 모든 vertex가 삭제되면 끝낸다
        then Nothing
      else (eliminate (eliminateVertex mg n) v' ns)

-- 순서 (o:os)대로 vertex를 삭제한다
-- 입력으로 grid와 순서를 가지는 vertex가 들어온다.
-- 출력으로 삭제된 
eliminateInOrder           :: Maybe Grid -> [Vertex] -> [Vertex]
eliminateInOrder Nothing _ = []
eliminateInOrder _ []      = []
eliminateInOrder mg (o:os) =
  if fromJust ifONothing then  -- o가 Nothing이면(이미 지워졌으면),
    eliminateInOrder mg os -- 다음순서 호출
  else
    let nextGrid = eliminateNeigh mg o in   -- o를 삭제했을 때, 
    case nextGrid of
      Nothing -> o : []                             -- 모든 vertex가 삭제 됐으면, o를 포함한 vertex들 반환
      _       -> o : (eliminateInOrder nextGrid os) -- 아니면, o를 포함하여 다음 순서 호출
  where
    ifONothing :: Maybe Bool
    ifONothing = do
      g <- mg
      case (g ! o) of
         Nothing -> return True
         _       -> return False

-- bs에 6개 이하가 남을 때까지 분해
-- bs에서 하나를 뽑아 as에 순서대로 넣는다
splitInto6 :: ([Vertex], [Vertex]) -> [([Vertex], [Vertex])]
splitInto6 ( as, bs ) | length bs <= 6   = [(as,bs)]
                      | otherwise        = concat [ splitInto6 (b:as, delete b bs) | b <- bs ]

-- eliminateInOrder를 모든 720 (6!)의 경우애 대해 호출한다
order :: Maybe Grid -> [Vertex] -> [[Vertex]]
order mg xs = set ( map func (splitInto6 ([], xs)) `using` parList rdeepseq) -- 6!개로 쪼갠 후, func을 병렬로 실행
  where
    set = nub.sort.concat
    -- 모든 순서를 만들고, 그 순서대로 삭제하여 값을 구한다
    func :: ([Vertex], [Vertex]) -> [[Vertex]]
    func (as, bs) = map (sort.eliminateInOrder mg) $ map (as ++) (permutations bs)

-- 입력을 grid로 변환시키고, order를 호출하여 값을 찾는다
solve :: [String] -> Maybe [[Vertex]]
solve ss = do
  gs <- parseGrid ss
  let mg       = parseGrid ss
      ids      = indices gs
  return $ order mg ids

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let indices = lines file
  let r = solve indices
  case r of
    Nothing -> print "Wrong Input"
    _       -> print $ fromJust r
  
{--
입력 예시
-- << inputs
1 2
1 5
1 6
2 1
2 3
3 4
3 7
4 5
5 6
5 7
6 7
-- >>
let inp = ["1 2", "1 5", "1 6", "2 1", "2 3", "3 4", "3 7", "4 5", "5 6", "5 7", "6 7"]
let ins = [(1,2),(1,5),(1,6),(2,1),(2,3),(3,4),(3,7),(4,5),(5,6),(5,7),(6,7)]
let peerEx = [(1,[2,5,6]),(2,[1,3]),(3,[2,4,7]),(4,[3,5]),(5,[1,4,6,7]),(6,[1,5,7]),(7,[3,5,6])]


answer with duplication =
[[1,3],[1,4],[1,4,7],[1,7],[2,4,6],[2,4,7],[2,5],[2,6],[2,7],[3,5],[3,6],[4,6]]
--}

-- 'case it is array
-- the vertex ids must be contiguous!!!!!!
