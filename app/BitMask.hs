module BitMask where
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IntMap
import Data.Bits
import Utils.Containers.Internal.BitUtil


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

