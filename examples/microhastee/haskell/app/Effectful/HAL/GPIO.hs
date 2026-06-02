{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE CPP #-}
module Effectful.HAL.GPIO (
    GPIO,
    HAL.GPIOMode(..),
    HAL.GPIOPull(..),
    HAL.GPIOAF(..),
    GPIOConfig(..),
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    get_gpio,
    gpio_init_secure,
    gpio_init_nonsecure,
    GPIOActions(..)
) where

import Data.Proxy

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup

#ifdef SECURE
import Effectful.Secure
#else
import Effectful.NonSecure
#endif

import Effectful.TypeLevel.Number
import Effectful.TypeLevel.List
import qualified HAL as HAL

data GPIO pin port = GPIO HAL.GPIO

data A
data B
data C
data D
data E
data F
data G
data H

class ToGPIOPort a where
    toPort :: proxy a -> HAL.GPIOPort

instance ToGPIOPort A where
    toPort :: proxy A -> HAL.GPIOPort
    toPort _ = HAL.A
instance ToGPIOPort B where
    toPort :: proxy B -> HAL.GPIOPort
    toPort _ = HAL.B
instance ToGPIOPort C where
    toPort :: proxy C -> HAL.GPIOPort
    toPort _ = HAL.C
instance ToGPIOPort D where
    toPort :: proxy D -> HAL.GPIOPort
    toPort _ = HAL.D
instance ToGPIOPort E where
    toPort :: proxy E -> HAL.GPIOPort
    toPort _ = HAL.E
instance ToGPIOPort F where
    toPort :: proxy F -> HAL.GPIOPort
    toPort _ = HAL.F
instance ToGPIOPort G where
    toPort :: proxy G -> HAL.GPIOPort
    toPort _ = HAL.G
instance ToGPIOPort H where
    toPort :: proxy H -> HAL.GPIOPort
    toPort _ = HAL.H

get_gpio :: forall pin port ns s .
            (ToInt pin, ToGPIOPort port) => Setup ns s ns (Cons (GPIO pin port) s) (GPIO pin port)
get_gpio = Ix.do
    let pin' = toInt (undefined :: Proxy pin)
        port' = toPort (undefined :: Proxy port)

    g <- liftSetupIO $ HAL.board_gpio_create port' pin'
    Ix.return $ GPIO g

data GPIOConfig = GPIOConfig
  { mode      :: HAL.GPIOMode
  , pull      :: HAL.GPIOPull
  , alternate :: HAL.GPIOAF
  }

gpio_init_secure :: (Member (GPIO pin port) s) => GPIO pin port -> GPIOConfig -> Setup ns s ns s ()
gpio_init_secure (GPIO g) cfg =
    let cfg' = HAL.GPIOConfig { HAL.mode = mode cfg
                              , HAL.pull = pull cfg
                              , HAL.alternate = alternate cfg
                              , HAL.security_domain = HAL.GPIOSecure
                              }
    in liftSetupIO $ HAL.gpio_init g cfg'

gpio_init_nonsecure :: forall s' pin port ns s .
                       ( Member (GPIO pin port) s
                       , Delete (GPIO pin port) s s')
                    => GPIO pin port -> GPIOConfig -> Setup ns s (Cons (GPIO pin port) ns) s' ()
gpio_init_nonsecure (GPIO g) cfg =
    let cfg' = HAL.GPIOConfig { HAL.mode = mode cfg
                              , HAL.pull = pull cfg
                              , HAL.alternate = alternate cfg
                              , HAL.security_domain = HAL.GPIONonsecure
                              }
    in liftSetupIO $ HAL.gpio_init g cfg'

class GPIOActions m where
    gpio_toggle :: (Member (GPIO pin port) effects) => GPIO pin port -> m effects ()
    gpio_read :: (Member (GPIO pin port) effects) => GPIO pin port -> m effects Bool
    gpio_write :: (Member (GPIO pin port) effects) => GPIO pin port -> Bool -> m effects ()

instance GPIOActions Secure where
    gpio_toggle (GPIO g) = secureLiftIO $ HAL.gpio_toggle g
    gpio_read (GPIO g) = secureLiftIO $ HAL.gpio_read g
    gpio_write (GPIO g) b = secureLiftIO $ HAL.gpio_write g b

instance GPIOActions Nonsecure where
    gpio_toggle (GPIO g) = nonsecureLiftIO $ HAL.gpio_toggle g
    gpio_read (GPIO g) = nonsecureLiftIO $ HAL.gpio_read g
    gpio_write (GPIO g) b = nonsecureLiftIO $ HAL.gpio_write g b