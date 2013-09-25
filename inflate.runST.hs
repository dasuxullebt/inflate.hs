{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, BangPatterns, ParallelListComp #-}
{-# OPTIONS_GHC -Wall #-}

import System.IO
import qualified Data.Array
import Control.Applicative
import Control.DeepSeq

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.ST
import Data.Array.ST

import Prelude hiding (lookup)
import Data.List
import qualified Data.Map as Map
import qualified Data.ByteString
import Data.Map ((!), Map)
import Data.Word
import Data.Bits
-- import Data.Binary
import qualified System.Environment

--import Debug.Trace
trace :: t -> t1 -> t1
trace _ a = a

-- #define TRACE_BYTES(str) !_ <- traceBytes (str)
#define TRACE_BYTES(str)

{-
data OutChars a = OutChars
  { len :: Int
  , current :: Data.Array.Array
-}

type OutChars a = [a]
-- data OutChars a = Nil | Cons a (OutChars (a,a)) | Cons2 a a (OutChars (a,a))

emptyOutChars :: [a]
emptyOutChars = []

mcons :: a -> OutChars a -> OutChars a
mcons = (:)
{-
mcons a Nil = Cons a Nil
mcons a (Cons b l) = Cons2 a b l
mcons a (Cons2 b c l) = Cons a $ mcons (b,c) l
-}

oc2listReverse :: OutChars a -> [a]
oc2listReverse = reverse

oc2list :: OutChars a -> [a]
oc2list = id
{-
oc2list o =
  let unpair :: [(a, a)] -> [a]
      unpair [] = []
      unpair ((a, b) : l) = a : b : (unpair l)
      oc2list' :: OutChars a -> [a]
      oc2list' Nil = []
      oc2list' (Cons b l) = b : (unpair $ oc2list' l)
      oc2list' (Cons2 a b l) = a : b : (unpair $ oc2list' l)
  in
   oc2list' o
-}
list2oc :: [a] -> OutChars a
list2oc = id
{-
list2oc [] = Nil
list2oc (x : l) = mcons x $ list2oc l
-}

concOcWithReversedList :: [a] -> OutChars a -> OutChars a
concOcWithReversedList [] oo = oo
concOcWithReversedList (x : ll) oo = concOcWithReversedList ll $ mcons x oo

concOcWithList :: [a] -> OutChars a -> OutChars a
concOcWithList l oc = concOcWithReversedList (reverse l) oc

{-
ocNth :: Int -> OutChars a -> Maybe a
ocNth _ Nil = Nothing
ocNth 0 (Cons a _) = Just a
ocNth n (Cons _ l) = let m = testBit n 0
                         n' = shift (n-1) (-1)
                         o = ocNth n' l
                     in
                      case o of Nothing -> Nothing
                                Just (x, y) -> Just $ if m then x else y
ocNth 0 (Cons2 a _ _) = Just a
ocNth n (Cons2 _ b l) = ocNth (n-1) (Cons b l)
-}

traceme :: Show a => String -> a -> a
traceme s a = trace (s ++ " " ++ (show a)) a

-- traceBytes :: String -> DeflateParse ()
-- traceBytes q = do
--   (!_, !by, !_) <- gets strIn
--   l <- gets inLen
--   return $ trace (q ++ " " ++ (show by) ++ " of " ++ (show l) ++ " bytes processed") ()

data CodeTree a = EmptyLeaf | Leaf a | Fork (CodeTree a) (CodeTree a)

weaveCodeTree :: Show a => Map a [Bool] -> Either String (CodeTree a)
weaveCodeTree x =
  let
    eitherFork :: Either String (CodeTree a) -> Either String (CodeTree a) -> Either String (CodeTree a)
    eitherFork (Right xx) (Right y) = Right (Fork xx y)
    eitherFork (Left xx) _ = Left xx
    eitherFork (Right _) (Left y) = Left y
    splitCodeList :: [(a, [Bool])] -> ([(a,[Bool])],[(a,[Bool])],[a])
    splitCodeList [] = ([],[],[])
    splitCodeList ((y, b) : l) =
      let (nl, nr, ne) = splitCodeList l
      in
       case b of [] -> (nl, nr, y : ne)
                 False : br -> ((y, br) : nl, nr, ne)
                 True : br -> (nl, (y, br) : nr, ne)
    weaveList :: Show a => [(a, [Bool])] -> Either String (CodeTree a)
    weaveList [] = Right EmptyLeaf
    weaveList cl = case (splitCodeList cl) of (l, r, []) -> eitherFork (weaveList l) (weaveList r)
                                              ([], [], [xx]) -> Right (Leaf xx)
                                              f -> Left ("Error in weaveList with map " ++
                                                         (show cl) ++ ": " ++ (show f))
  in weaveList $ filter (\ (_, y) -> case y of [] -> False
                                               _ -> True) (Map.toList x)

-- (bit in current byte, byte number, word list)
--type BitStream = (Int, Int, [Word8])

type BitStream = (Int, [[Bool]])

bitsOf :: Word8 -> [Bool]
bitsOf c = map (testBit c) [0..7]
               
wordToBitSeq :: Data.Array.Array Word8 [Bool]
wordToBitSeq = Data.Array.listArray (0,255) (map (bitsOf . fromIntegral) [0..255])

wordsToBitSeq :: [Word8] -> [[Bool]]
wordsToBitSeq = map (wordToBitSeq Data.Array.!)

wordsToBitStream :: [Word8] -> BitStream
wordsToBitStream w = (0, wordsToBitSeq w)

data OutWord = WCh Word8 | BackRef Int -- BackRef Dist

data DeflateState = DeflateState {strIn :: BitStream, out :: OutChars OutWord, inLen :: Int }

newtype DeflateParse a = DeflateParse
  { unDeflateParse :: ErrorT String (State DeflateState) a }
  deriving (Functor, Applicative, Alternative,
            Monad, MonadPlus, MonadState DeflateState)

throwUp :: String -> DeflateParse a
throwUp aleph =
  do (byte, _) <- gets strIn
     _sofar <- gets out
     DeflateParse {unDeflateParse = throwError (aleph ++ ", byte=" ++ (show byte))}

-- ++ ", decompressed so far:" ++ (show $ Data.ByteString.pack (reverse $ oc2list sofar)) ++ ")")}

--throwUp aleph = error aleph

{-# INLINE peekAndPopBit #-}
peekAndPopBit :: BitStream -> Maybe (Bool, BitStream)
peekAndPopBit (_, []) = Nothing
peekAndPopBit (n, [] : l) = peekAndPopBit (n+1, l)
peekAndPopBit (n, (c : r) : l) = Just (c, (n, r : l))

peekBit :: BitStream -> Maybe Bool
peekBit (_, []) = Nothing
peekBit (n, [] : l) = peekBit (n+1, l)
peekBit (_n, (c : _r) : _l) = Just c

popBit :: BitStream -> Maybe BitStream
popBit (_, []) = Nothing
popBit (n, [] : l) = popBit (n+1, l)
popBit (n, (_c : r) : l) = Just (n, r : l)

finishByte :: BitStream -> Maybe BitStream
finishByte (_, []) = Nothing
finishByte (n, [] : l) = finishByte (n+1, l)
finishByte (n, _ : l) = Just (n+1, l)

peekByte :: BitStream -> Either String Word8
peekByte (_, []) = Left "peekByte at end of stream"
peekByte (n, [] : l) = peekByte (n+1, l)
peekByte (_n, j : _l) =
  if (length j) < 8
     then Left ("peekByte not at byte position in byte " ++ (show j))
  else Right $ fromIntegral (lsbToInt j)

-- map every integer onto the count of the numbers of its occurences in the passed list
countNumbers :: [Int] -> Map Int Int
countNumbers l = foldl' (\ m i -> Map.insertWith (+) i 1 m) Map.empty l

minimalCodeOfLength :: Map Int Int -> Int -> Int
minimalCodeOfLength _ 0 = 0
minimalCodeOfLength countNums n = 2 * ((minimalCodeOfLength countNums (n-1)) +
                                       (Map.findWithDefault 0 (n-1) countNums))

-- Convert a sequence of code lengths to a code map CharCode ->
-- (Length, Code)
toCodeMap :: [Int] -> Map Int (Int, Int)
toCodeMap l =
  let cNum = countNumbers l
      mLen = maximum l
      mCodes = Map.fromList (map (\ x -> (x, minimalCodeOfLength cNum x)) [1 .. mLen])
      (_, tCodes) = mapAccumL (\ (codeMap, charCode) len ->
                           let cc = codeMap ! len
                           in
                            ((Map.insert len (1+cc) codeMap , 1+charCode), (charCode, (len, cc)))) (mCodes, 0) l
  in
   Map.fromList tCodes

intToCode :: Int -> Int -> [Bool]
intToCode l c = Prelude.map (\ x -> testBit c x) (reverse [0..(l-1)])

toBoolCodeMap :: [Int] -> Map Int [Bool]
toBoolCodeMap l = (Map.map (\ (x, y) -> intToCode x y) (toCodeMap l))

toBoolCodeMapWithoutZeroes :: [Int] -> Map Int [Bool]
toBoolCodeMapWithoutZeroes l =
  let
    withCodes = zip [0..] l
    (zeroCodes, realCodes) = partition (\ (_, y) -> y == 0) withCodes
    remaining = map (\ (_, y) -> y) realCodes
    codeMap = toBoolCodeMap remaining
    xCodeMap = Map.mapKeys (\ k -> case realCodes !! k of (code, _len) ->
                                                                 code)
               codeMap
  in
   Map.union xCodeMap (Map.fromList
                            (map (\ (x, _) -> (x, [])) zeroCodes))

-- Tests
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap [2,1,3,3]))
-- fromList [(0,[True,False]),(1,[False]),(2,[True,True,False]),(3,[True,True,True])]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 0
-- [False,False,True,True,False,False,False,False]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 143
-- [True,False,True,True,True,True,True,True]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 144
-- [True,True,False,False,True,False,False,False,False]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 255
-- [True,True,True,True,True,True,True,True,True]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 256
-- [False,False,False,False,False,False,False]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 279
-- [False,False,True,False,True,True,True]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 280
-- [True,True,False,False,False,False,False,False]
-- *Main> (Map.map (\ (x, y) -> intToCode x y) (toCodeMap ((replicate 144 8) ++ (replicate 112 9) ++ (replicate 24 7) ++ (replicate 8 8)))) ! 287
-- [True,True,False,False,False,True,True,True]
-- *Main>

mapStrIn :: (BitStream -> BitStream) -> DeflateState -> DeflateState
mapStrIn f st = st {strIn = f (strIn st)}

setStrIn :: BitStream -> DeflateState -> DeflateState
setStrIn bs st = st {strIn = bs}

mapOut :: (OutChars OutWord -> OutChars OutWord) -> DeflateState -> DeflateState
mapOut f st = st {out = f (out st)}

setOut :: OutChars OutWord -> DeflateState -> DeflateState
setOut bs st = st { out = bs }

advanceInput :: DeflateParse ()
advanceInput =
  do
    !si <- gets strIn
    case popBit si of Just sj -> modify $ setStrIn sj
                      Nothing -> throwUp "advanceInput: End of strInt"

peekAtNextBit :: DeflateParse Bool
peekAtNextBit = do
  !sti <- gets strIn
  case peekBit sti of
    Just x -> return x
    Nothing -> throwUp "peekAtNextBit failed: End of bitstream"

advanceToNextByte :: DeflateParse ()
advanceToNextByte = do
  !sti <- gets strIn
  case finishByte sti of
    Just x -> modify $ setStrIn x
    Nothing -> throwUp "advanceToNextByte: End of strInt"

peekAtNextByte :: DeflateParse Word8
peekAtNextByte = do
  !sti <- gets strIn
  case peekByte sti of Left x -> throwUp x
                       Right x -> do return x

singleBit :: Bool -> DeflateParse ()
singleBit x = do
  guard . (x ==) =<< peekAtNextBit
  advanceInput

parseBit :: DeflateParse Bool
parseBit = do
  !sti <- gets strIn
  case peekAndPopBit sti of
    Nothing ->  throwUp "parseBit: End of strInt"
    Just (b, sti2) -> do
      modify $ setStrIn sti2
      return b

parseByte :: DeflateParse Word8
parseByte = do
  !b <- peekAtNextByte
  _ <- parseBit
  advanceToNextByte
  return b

-- | Read @i@ bits.
nBits :: Int -> DeflateParse [Bool]
nBits i = do
  !bits <- sequence $ replicate i parseBit
  return bits

msbToInt :: [Bool] -> Int
msbToInt bits = foldl' (\ acc b -> acc * 2 + if b then 1 else 0) 0 bits

lsbToInt :: [Bool] -> Int
lsbToInt bits = foldr (\ b acc -> acc * 2 + if b then 1 else 0) 0 bits

parseBytesReverse :: Int -> DeflateParse [Word8]
parseBytesReverse 0 = do return []
parseBytesReverse i = do
  TRACE_BYTES("parseBytesReverse")
  byte <- parseByte
  rest <- parseBytesReverse $ i - 1
  return $ byte : rest

uncompressedBlock :: DeflateParse ()
uncompressedBlock = do
  advanceToNextByte
  !lenBits <- nBits 16
  !negLenBits <- nBits 16
  if (and (map (\ (y, z) -> not (y == z)) (zip lenBits negLenBits)))
    then do !bytes <- parseBytesReverse (lsbToInt lenBits)
            TRACE_BYTES("uncompressed Block" ++ (show $ lsbToInt lenBits))
            modify $ mapOut (concOcWithReversedList (map WCh bytes))
    else throwUp "LEN and NLEN are not negated in uncompressed block"

parseAccToTree :: (CodeTree a) -> DeflateParse a
parseAccToTree EmptyLeaf = throwUp "Parsing illegal code"
parseAccToTree (Leaf x) = do return x
parseAccToTree (Fork ctA ctB) = do
  !b <- parseBit
  parseAccToTree (if b then ctB else ctA)

repeatCodeToBitNum :: Int -> Int
repeatCodeToBitNum rc = case rc of _ | rc <= 264 -> 0
                                     | rc == 285 -> 0
                                     | otherwise -> ((rc - 265) `div` 4) + 1

repeatCodeToBase :: Int -> Int
repeatCodeToBase 257 = 3
repeatCodeToBase 285 = 258
repeatCodeToBase rc = 2^(repeatCodeToBitNum $ rc - 1) + (repeatCodeToBase $ rc - 1)

distCodeToBitNum :: Int -> Int
distCodeToBitNum lc = if (lc <= 3) then 0 else ((lc - 4) `div` 2) + 1


distCodeToBaseC :: Int -> Int
distCodeToBaseC 0 = 1
distCodeToBaseC dc = 2^(distCodeToBitNum $ dc - 1) + (distCodeToBase $ dc - 1)

distCodeToBaseA :: Data.Array.Array Int Int
distCodeToBaseA = Data.Array.listArray (0,29) [ distCodeToBaseC j | j <- [0..29] ]

distCodeToBase :: Int -> Int
distCodeToBase = (distCodeToBaseA Data.Array.!)



{-
forwardRepeatUnsafe :: Int -> Int -> OutChars a -> Maybe (OutChars a)
forwardRepeatUnsafe _ 0 sq = Just sq
forwardRepeatUnsafe dist len sq =
  case ocNth dist sq of Nothing -> Nothing
                        Just x -> forwardRepeatUnsafe dist (len - 1) (mcons x sq)

forwardRepeat :: Show a => Int -> Int -> OutChars a -> Maybe (OutChars a)
forwardRepeat dist len sq =
  case dist of _ | dist < 1 -> Nothing
                 | otherwise ->
                   if len < 3 then Nothing
                   else forwardRepeatUnsafe (dist - 1) len sq
-}

parseRepeater :: CodeTree Int -> Int -> DeflateParse ()
parseRepeater distMap code =
  do
    !bits <- nBits $ repeatCodeToBitNum code
    !distCode <- parseAccToTree distMap
    if (distCode < 0) || (distCode > 29) then throwUp "Illegal Distance Code in parseRepeater"
      else
      do distBits <- nBits $ distCodeToBitNum distCode
         --o <- gets out
         let dist = (distCodeToBase distCode) + (lsbToInt distBits)
             len = (repeatCodeToBase code) + (lsbToInt bits)
--             !xout = forwardRepeat dist len o
           in
             modify $ mapOut $ (concOcWithReversedList (replicate len (BackRef dist)))
--            case xout of Nothing -> throwUp "Problem with repeting in parseRepeater"
--                         Just x -> modify $ setOut x

parseCompressedBlock :: CodeTree Int -> CodeTree Int -> DeflateParse ()
parseCompressedBlock codeMap distMap = do
  !code <- parseAccToTree codeMap
  case code of _ | code < 0 -> throwUp "Error: Code<0 in parseCompressedBlock"
                 | code <= 255 -> do !o <- gets out
                                     modify $ setOut (mcons (WCh (fromIntegral code)) o)
                                     TRACE_BYTES("compressed Block")
                                     parseCompressedBlock codeMap distMap
                 | code == 256 -> do
                   return ()
                 | code <= 285 -> do
                   parseRepeater distMap code
                   TRACE_BYTES("compressed Block")
                   parseCompressedBlock codeMap distMap
                 | code <= 287 -> throwUp "Error: code 286 or 287 in parseCompressedBlock"
                 | otherwise -> throwUp "Error: Code>287 in parseCompressedBlock"

staticallyCompressedBlock :: DeflateParse ()
staticallyCompressedBlock =
  let
    cm_ = weaveCodeTree $ toBoolCodeMap ((replicate 144 8) ++
                                        (replicate 112 9) ++
                                        (replicate 24 7) ++
                                        (replicate 8 8))
    dm_ = weaveCodeTree $ toBoolCodeMap (replicate 32 5)
    cm = case cm_ of Right x -> x
                     _ -> error "staticallyCompressedBlock: cm: This should never happen!"
    dm = case dm_ of Right x -> x
                     _ -> error "staticallyCompressedBlock: dm: This should never happen!"

  in do
    parseCompressedBlock cm dm

dynamicBlockHeader :: DeflateParse (Int, Int, Int)
dynamicBlockHeader = do
  !hlitBits <- nBits 5
  !hdistBits <- nBits 5
  !hclenBits <- nBits 4
  return ((lsbToInt hlitBits) + 257,
          (lsbToInt hdistBits) + 1,
          (lsbToInt hclenBits) + 4)

dynamicBlockLenListRaw :: Int -> DeflateParse [Int]
dynamicBlockLenListRaw hclen = do
  !code <- sequence $ replicate hclen $ nBits 3
  return $ map lsbToInt code

dynamicBlockLenListToActualLens :: [Int] -> [Int]
dynamicBlockLenListToActualLens !l =
  let
    -- cf RFC 1951, page 13
    helper = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
    !mapz = Map.fromList $ zip helper l
  in
   map (\ x -> Map.findWithDefault 0 x mapz) ([0..18] :: [Int])

dynamicBlockLenMap :: Int -> DeflateParse (CodeTree Int)
dynamicBlockLenMap hclen = do
  !raw <- dynamicBlockLenListRaw hclen
  let
    lM = weaveCodeTree $ toBoolCodeMapWithoutZeroes $ dynamicBlockLenListToActualLens raw
   in
     case lM of Right lm -> do return lm
                Left lm -> throwUp ("Did not receive Length Map in dynamicBlockLenMap: " ++ lm)

readCodeLengthCode :: CodeTree Int -> DeflateParse Int
readCodeLengthCode tree = do parseAccToTree tree

-- read the raw code lengths and, if applicable, the repeat length
-- (which defaults to 0 if not applicable)
readCodeLengthsRaw :: Int -> CodeTree Int -> DeflateParse [(Int, Int)]
readCodeLengthsRaw 0 _ = return []
readCodeLengthsRaw n ct =
  if (n < 0) then throwUp ("Error in readCodeLengthsRaw: negative number of codes: " ++ (show n))
  else let
    proceed :: Int -> Int -> Int -> Int -> DeflateParse [(Int, Int)]
    proceed m clc bitNum base = do
      !bits <- nBits bitNum
      let repeats = base + (lsbToInt bits) in
        do !rest <- readCodeLengthsRaw (m - repeats) ct
           return $ (clc, repeats) : rest
  in
   do
     !clc <- readCodeLengthCode ct
     case clc of _ | clc <= 15 -> proceed n clc 0 1 -- TODO: reference RFC
                   | clc == 16 -> proceed n clc 2 3
                   | clc == 17 -> proceed n clc 3 3
                   | clc == 18 -> proceed n clc 7 11
                   | otherwise -> throwUp "Error in readCodeLengthsRaw: illegal code"

-- use this with Map.fromList
rawCodeLengthsToActualCodeLengths :: [(Int,Int)] -> Either String (Map Int Int)
rawCodeLengthsToActualCodeLengths [] = Left "Empty Code Length List in rawCodeLengthsToActualCodeLengths"
rawCodeLengthsToActualCodeLengths [(16,_)] = Left "Replication of last Byte at start in rawCodeLengthsToActualCodeLengths"
rawCodeLengthsToActualCodeLengths rcl =
  Right $ Map.fromList $ zip [0..] (
    foldl' ( \ accum (cC, cR) ->
              accum ++ case cC of 18 -> replicate cR 0
                                  17 -> replicate cR 0
                                  16 -> replicate cR (last accum) -- exists, see above
                                  _ -> [cC] ) [] rcl )

dynamicBlockCodeTree :: Int -> CodeTree Int -> DeflateParse (CodeTree Int)
dynamicBlockCodeTree n ct = do
  !codeList <- readCodeLengthsRaw n ct
  case rawCodeLengthsToActualCodeLengths codeList of
    Left x -> throwUp x
    Right x -> case weaveCodeTree (toBoolCodeMapWithoutZeroes
                                   $ map (\ m -> Map.findWithDefault 0 m x) [0..(maximum $ Map.keys x)]) of
      Right y -> return y
      Left xx -> throwUp ("Error in dynamicBlockCodeTree: " ++ xx)

dynamicallyCompressedBlock :: DeflateParse ()
dynamicallyCompressedBlock = do
  (!hlit, !hdist, !hclen) <- dynamicBlockHeader
  !lm <- dynamicBlockLenMap hclen
  !cm <- dynamicBlockCodeTree hlit lm
  !dm <- dynamicBlockCodeTree hdist lm
  parseCompressedBlock cm dm

-- traceState :: String -> DeflateParse ()
-- traceState q = do
--   (bt, by, _) <- gets strIn
--   si <- gets strIn
--   --o <- gets out
--   modify $ trace (q ++ "trace: bit=" ++ (show bt) ++ " byte=" ++ (show by)) $ setStrIn si

--  modify $ trace (q ++ "trace: bit=" ++ (show bt) ++ " byte=" ++ (show by) ++ " out=" ++ (show $ reverse $ oc2list o)) $ setStrIn si

-- toplevelParser :: DeflateParse (GHC.Arr.Array Int Word8)
-- toplevelParser =
--     let block = do !headers <- nBits 2
--                    let !ret = case headers of [True, True] -> (throwUp "Illegal Block Type 11 in toplevelParser")
--                                               [False, False] -> do
--                                               TRACE_BYTES("start uncompressed block")
--                                               uncompressedBlock
--                                             [False, True] -> do
--                                               TRACE_BYTES("start dynamically compressed block")
--                                               dynamicallyCompressedBlock
--                                             [True, False] -> do
--                                               TRACE_BYTES("start statically compressed block")
--                                               staticallyCompressedBlock
--                                             _ -> error "Impossible error in toplevelParser"
--                    in ret
--       nBlock = do { singleBit False; block }
--       lBlock = do { singleBit True; block }
--   in do
--     _ <- many nBlock
--     lBlock
--     !o <- gets out
--     let
--         !ro = reverse (oc2list o)
--         rl = length ro
--         arr = Data.Array.array (0, rl-1) [(i, case i of WCh x -> x; BackRef dist -> arr !! (i - dist)) | i <- [0..(rl-1)]]
--     in
--       return arr


replaceBackreferences :: [OutWord] -> (Data.Array.Array Int Word8)
replaceBackreferences l =
  let
    replaceNth :: Int -> OutWord -> (STArray s Int Word8) -> ST s ()
    replaceNth n (WCh c) arr = writeArray arr n c
    replaceNth n (BackRef b) arr = do c <- readArray arr (n - b)
                                      writeArray arr n c
    
    insertToArray :: Int -> [OutWord] -> (STArray s Int Word8) -> ST s ()
    insertToArray _ [] _arr = return ()
    insertToArray n (x : ll) arr = do replaceNth n x arr
                                      insertToArray (n + 1) ll arr
    toplevel :: [OutWord] -> ST s (STArray s Int Word8)
    toplevel ow =
      do arr <- newArray (0, (length ow) - 1) (fromIntegral 0) :: ST s (STArray s Int Word8)
         insertToArray 0 ow arr
         return arr
  in runSTArray (toplevel l)

toplevelParser :: DeflateParse (Data.Array.Array Int Word8)
toplevelParser =
  let block = do !headers <- nBits 2
                 let !ret = case headers of [True, True] -> (throwUp "Illegal Block Type 11 in toplevelParser")
                                            [False, False] -> do
                                              TRACE_BYTES("start uncompressed block")
                                              uncompressedBlock
                                            [False, True] -> do
                                              TRACE_BYTES("start dynamically compressed block")
                                              dynamicallyCompressedBlock
                                            [True, False] -> do
                                              TRACE_BYTES("start statically compressed block")
                                              staticallyCompressedBlock
                                            _ -> error "Impossible error in toplevelParser"
                   in ret
      nBlock = do { singleBit False; block }
      lBlock = do { singleBit True; block }
  in do
    _ <- many nBlock
    lBlock
    !o <- gets out
    let !ro = oc2listReverse o
--         rl = length ro
--         arr = Data.Array.listArray (0, rl-1)
--                 [ case j of
--                        WCh x -> x
--                        BackRef dist -> arr Data.Array.! (i - dist)
--                 | (i,j) <- zip [0..] ro
-- --                | i <- [0..(rl-1)]
-- --                | j <- ro
--                 ]
    return $ replaceBackreferences ro


inflate :: [Word8] -> Either String (Data.Array.Array Int Word8)
inflate !str =
  let !blub = Control.Monad.State.runState (
        runErrorT ( unDeflateParse toplevelParser )) ( DeflateState { strIn = wordsToBitStream str, out = emptyOutChars, inLen = length str })
  in case blub of (!r, _) -> r

gzStrip :: [Word8] -> [Word8]
gzStrip !w8 =
  let
    untilZero :: [Word8] -> ([Word8], [Word8])
    untilZero [] = ([], [])
    untilZero (0 : l) = ([], l)
    untilZero (r : l) = let (x, y) = untilZero l in (r : x, y)
    [_ftext, fhcrc, fextra, fname, fcomment] = take 5 [0..]
    [_id1, _id2, _cm, flg, _mtime1, _mtime2, _mtime3, _mtime4, _xfl, _os] = take 10 w8
    n1 = drop 10 w8
    (n2, _extra) = if testBit flg fextra
                  then let [xl1, xl2] = map fromIntegral $ take 2 n1
                       in (drop (2 + xl1 + 256*xl2) n1, take (xl1 + 256*xl2) (drop 2 n1))
                  else (n1, [])
    (_name, n3) = if testBit flg fname then untilZero n2 else ([], n2)
    (_comment, n4) = if testBit flg fcomment then untilZero n3 else ([], n3)
    (_hcrc, n5) = if testBit flg fhcrc then (take 2 n4, drop 2 n4) else ([], n4)
    !ret = take ((length n5) - 8) n5
  in ret

gunzip :: [Word8] -> Either String (Data.Array.Array Int Word8)
gunzip = inflate . gzStrip

mainWithArgs :: [String] -> IO ()
mainWithArgs args = do
  !content <- Data.ByteString.readFile (head args)
  case gunzip (Data.ByteString.unpack content) of
    Left x -> print $ "Error: " ++ x
    Right !arr -> do
      arr `deepseq` print "Success."
      -- print $ foldl' xor 255 $ Data.Array.elems arr
      Data.ByteString.writeFile (args !! 1) (Data.ByteString.pack (Data.Array.elems arr))

main :: IO ()
main = do
  args <- System.Environment.getArgs
  mainWithArgs args

-- deflcont :: IO ()
-- deflcont = do content <- Data.ByteString.readFile "/tmp/deflcont"
--               case (inflate $ Data.ByteString.unpack content) of
--                 Left x -> print $ "Error: " ++ x
--                 Right y -> print $ Data.ByteString.pack y

