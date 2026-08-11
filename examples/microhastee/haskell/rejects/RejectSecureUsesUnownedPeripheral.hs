module RejectSecureUsesUnownedPeripheral where

import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.HAL

type OwnedLED = GPIO N2 G
type OtherLED = GPIO N7 C

-- REJECTED: this secure-world function only carries 'Member OwnedLED
-- effects' evidence, but tries to toggle 'OtherLED' too -- a peripheral it
-- was never given access to. The secure/nonsecure split is enforced by the
-- 'effects' list attached to 'Secure', so this must fail to typecheck no
-- matter what the 'Setup' phase actually did at runtime.
badSecureAction :: Member OwnedLED effects => GPIO N2 G -> GPIO N7 C -> Secure effects ()
badSecureAction owned other = do
    gpio_toggle owned
    gpio_toggle other

main :: IO ()
main = return ()
