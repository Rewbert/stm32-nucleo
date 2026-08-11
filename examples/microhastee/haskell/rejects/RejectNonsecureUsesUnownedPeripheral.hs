module RejectNonsecureUsesUnownedPeripheral where

import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.HAL

type OwnedLED = GPIO N7 C
type OtherLED = GPIO N2 G

-- REJECTED: mirror of 'RejectSecureUsesUnownedPeripheral' -- the nonsecure
-- world can't touch a peripheral it was never given 'Member' evidence for
-- either. The type-level split is symmetric between 'Secure' and
-- 'Nonsecure'.
badNonsecureAction :: Member OwnedLED effects => GPIO N7 C -> GPIO N2 G -> Nonsecure effects ()
badNonsecureAction owned other = do
    gpio_toggle owned
    gpio_toggle other

main :: IO ()
main = return ()
