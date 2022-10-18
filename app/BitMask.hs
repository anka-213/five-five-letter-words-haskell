module BitMask where
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IntMap
import Data.Bits
import Utils.Containers.Internal.BitUtil
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.IntSet.Internal as IntSet.Internal
import Data.IntSet.Internal (IntSet(..))
import Data.Coerce (coerce)


-- | A simpler IntSet for numbers less than 32
newtype Bitmask32 = BM {unBM :: Int}
  deriving (Eq, Ord)

empty :: Bitmask32
empty = BM 0

insert :: IntMap.Key -> Bitmask32 -> Bitmask32
insert k (BM m) | k >= 32 || k < 0 = error $ "BitMask.insert: out of range: " ++ show k
insert k (BM m) = BM $ setBit m k

fromList :: [Int] -> Bitmask32
fromList = foldr insert empty

disjoint :: Bitmask32 -> Bitmask32 -> Bool
disjoint (BM a) (BM b) = a .&. b == 0

-- >>> BitMask.minimum $ fromList [5,7]
-- >>> BitMask.minimum $ fromList []
-- Just 5
-- Nothing
minimum :: Bitmask32 -> Maybe Int
minimum = adjustThing . countTrailingZeros . unBM

adjustThing :: Int -> Maybe Int
adjustThing 64 = Nothing
adjustThing n = Just n

notMember :: IntMap.Key -> Bitmask32 -> Bool
notMember k = not . member k

member :: IntMap.Key -> Bitmask32 -> Bool
member k (BM b) = testBit b k

size :: Bitmask32 -> Int
size = popCount . unBM

union :: Bitmask32 -> Bitmask32 -> Bitmask32
union (BM a) (BM b) = BM (a .|. b)

fromIntSet :: IntSet -> [Bitmask32]
fromIntSet = coerce IntSet.toList

-- showTree

bin :: IntSet.Internal.Prefix -> IntSet.Internal.Mask -> IntSet -> IntSet -> IntSet
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
{-# INLINE bin #-}


tip :: IntSet.Internal.Prefix -> IntSet.Internal.BitMap -> IntSet
tip _ 0 = Nil
tip kx bm = Tip kx bm
{-# INLINE tip #-}

-- -- Remove all elements of an IntSet that shares a 1-bit with the BitMask
restrictIntSet :: Bitmask32 -> IntSet -> IntSet
restrictIntSet (BM a) = go
  where
    lowMask = complement $ low6Mask a
    go (Bin prefix mask is1 is2)
        | prefix .&. a /= 0 = Nil
        -- mask something something -- TODO: mask is a power of 2, if its bit corresponds to an element of our mask, we can filter one
        | otherwise = bin prefix mask (go is1) (go is2)
    go (Tip prefix wo)
        | prefix .&. a == 0 = tip prefix (wo .&. lowMask)
        | otherwise = Nil -- Overlap means
    go Nil = Nil

-- -- low6Mask 0  = 0x0000000000000000
-- -- low6Mask 1  = 0xaaaaaaaaaaaaaaaa
-- -- low6Mask 2  = 0xcccccccccccccccc
-- -- low6Mask 4  = 0xf0f0f0f0f0f0f0f0
-- -- low6Mask 8  = 0xff00ff00ff00ff00
-- -- low6Mask 16 = 0xffff0000ffff0000
-- -- low6Mask 32 = 0xffffffff00000000

-- -- low6Mask 3  = 0xeeeeeeeeeeeeeeee

-- -- low6Mask 0 = 0xffffffff
low6Mask :: Int -> IntSet.Internal.BitMap
low6Mask n =
        (if n .&. 1  /= 0 then 0xaaaaaaaaaaaaaaaa else 0)
    .|. (if n .&. 2  /= 0 then 0xcccccccccccccccc else 0)
    .|. (if n .&. 4  /= 0 then 0xf0f0f0f0f0f0f0f0 else 0)
    .|. (if n .&. 8  /= 0 then 0xff00ff00ff00ff00 else 0)
    .|. (if n .&. 16 /= 0 then 0xffff0000ffff0000 else 0)
    .|. (if n .&. 32 /= 0 then 0xffffffff00000000 else 0)

-- >>> import Text.Printf
-- >>> printf "0x%0.16x" $ low6Mask 1 :: String
-- >>> printf "0x%0.16x" $ low6Mask 3 :: String
-- "0xaaaaaaaaaaaaaaaa"
-- "0xeeeeeeeeeeeeeeee"

-- >>> import Text.Printf
-- >>> intSetToBinary ( Data.IntSet.Internal.Tip 0 n) = printf "0b%0.64b\n" n
-- >>> intSetToHex (Data.IntSet.Internal.Tip 0 n) = printf "0x%0.16x\n" n
-- >>> map (\n -> intSetToHex $ IntSet.fromList [ x | x <- [0..63], x .&. (2^n) /= 0]) [0..5] :: [String]
-- No instance for (Bits IntSet) arising from a use of ‘complement’

-- mapM_ (\n -> intSetToHex $ IntSet.fromList [ x | x <- [0..63], x .&. (n) /= 0]) [1..63]

-- >>> intSetToBitMap (Data.IntSet.Internal.Tip 0 n) = n
-- >>> all (\n -> (== low6Mask n) . intSetToBitMap $ IntSet.fromList [ x | x <- [0..63], x .&. (n) /= 0]) [1..63]
-- True
