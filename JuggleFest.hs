module JuggleFest where

import Control.Monad.ST
import Data.Array
import Data.Array.ST
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap as IM
import System.Environment


main = do
    input <- BS.readFile =<< fmap head getArgs
    print $ solve (parseInstance input)


-- Types

type Skills = [Int]

type Match = Int

type CircuitId = Int

type JugglerId = Int

data Juggler = Juggler { jugglerId :: JugglerId, preferences :: [(CircuitId,Match)] }
    deriving Show

type Instance = (CircuitId,[Juggler])

type Team = IM.IntMap [Juggler]

type TeamTable s = STArray s CircuitId Team


-- Logic

teamSize :: Team -> Int
teamSize = IM.foldr (\js acc -> length js + acc) 0

joinTeam :: Team -> Match -> Juggler -> Team
joinTeam team match juggler = IM.insertWith (++) match [juggler] team

pruneTeam :: Int -> Team -> (Team,[Juggler])
pruneTeam limit team = case IM.minView team of
    Nothing                       -> (team  ,[]  )
    Just (weak,strong)
        | teamSize strong < limit -> (team  ,[]  )
        | otherwise               -> (strong,weak)

place1 :: Int -> TeamTable s -> Juggler -> ST s ()
place1 limit teamTable juggler = case preferences juggler of
    []                -> return ()
    (cid,match):prefs -> do
        team <- readArray teamTable cid
        let (team',losers) = pruneTeam limit $ joinTeam team match $ juggler {preferences = prefs}
        writeArray teamTable cid team'
        place limit teamTable losers

place :: Int -> TeamTable s -> [Juggler] -> ST s ()
place limit teamTable = mapM_ (place1 limit teamTable)

solve :: Instance -> Int
solve (numCircuits,jugglers) = runST $ do
    let limit = length jugglers `div` numCircuits
    teamTable <- newArray (0,numCircuits-1) IM.empty
    place limit teamTable jugglers
    fmap (sum . map jugglerId . IM.foldr (++) []) $ readArray teamTable 1970


-- Parser

type ByteString = BS.ByteString

parseInt :: ByteString -> Int
parseInt = maybe (error "no parse for integer") fst . BS.readInt

parseId :: ByteString -> Int
parseId = parseInt . BS.tail

parseSkills :: [ByteString] -> [Int]
parseSkills = map (parseInt . BS.drop 2)

parseCircuit :: ByteString -> (CircuitId,Skills)
parseCircuit str =
    let [_,cid,ch,ce,cp] = BS.words str
    in (parseId cid,parseSkills [ch,ce,cp])

parseJuggler :: (CircuitId -> Skills) -> ByteString -> Juggler
parseJuggler reqref str =
    let [_,jid,jh,je,jp,pref] = BS.words str
        abl = parseSkills [jh,je,jp]
    in  Juggler (parseId jid) (map (\str -> let cid = parseId str in (cid,sum . zipWith (*) abl $ reqref cid)) $ BS.split ',' pref)

parseInstance :: ByteString -> Instance
parseInstance str =
    let (circs,_:juggs) = break BS.null $ BS.lines str
        circuits = map parseCircuit circs
        numCircuits = length circuits
        circuitsArr = listArray (0,numCircuits-1) (map snd circuits)
        jugglers = map (parseJuggler (circuitsArr !)) juggs
    in  (numCircuits,jugglers)
