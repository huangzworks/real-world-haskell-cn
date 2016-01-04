-- file: ch14/Carrier.hs
import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress

-- file: ch14/Carrier.hs
variation1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

-- file: ch14/Carrier.hs
variation2 person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address

-- file: ch14/Carrier.hs
variation2a person phoneMap carrierMap addressMap = do
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap

-- file: ch14/Carrier.hs
variation3 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where lookup = flip M.lookup
