module RejectUnacquiredPeripheral where

import Effectful.NonSecure
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Number
import Effectful.HAL

-- REJECTED: 'gpio_toggle' requires 'Member (GPIO pin port) effects'. This
-- function's signature gives no such evidence for its 'effects' parameter --
-- it could be instantiated at 'Nil' -- so calling 'gpio_toggle' on a pin that
-- was never proven acquired must fail to typecheck.
badAction :: GPIO N2 G -> Nonsecure effects ()
badAction pin = gpio_toggle pin

main :: IO ()
main = return ()
