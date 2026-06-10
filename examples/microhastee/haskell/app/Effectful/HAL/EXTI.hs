{-# LANGUAGE CPP #-}
{-# LANGUAGE QualifiedDo #-}
module Effectful.HAL.EXTI (
    EXTI,
    HAL.EXTIEdge(..),
    HAL.EXTIConfig(..),
    get_exti,
    get_button_exti,
    exti_init,
    exti_release,
    exti_irqn,
    exti_enable,
    exti_disable,
    exti_on_secure,
    exti_on_nonsecure
) where

import Data.Proxy

import Foreign.C.Types

import qualified Control.Monad.IxMonad as Ix
import Effectful.Setup

#ifdef SECURE
import Effectful.Secure
#else
import Effectful.NonSecure
#endif

import Effectful.HAL.GPIO
import Effectful.TypeLevel.Number
import Effectful.TypeLevel.List
import qualified HAL as HAL

foreign import ccall "exti_trampoline.h h_exti_register_callback" c_h_exti_register_callback :: HAL.EXTI -> CInt -> IO ()

data EXTI pin port = EXTI Int HAL.EXTI

get_exti :: forall pin port ns s .
            (ToInt pin, ToGPIOPort port) => Setup ns s ns (Cons (EXTI pin port) s) (EXTI pin port)
get_exti = Ix.do
    let line = toInt (undefined :: Proxy pin)
    exti <- liftSetupIO $ HAL.board_exti_create line
    Ix.return $ EXTI line exti

get_button_exti :: Setup ns s ns (Cons (EXTI N13 C) s) (EXTI N13 C)
get_button_exti = Ix.do
    exti <- liftSetupIO $ HAL.board_button_exti HAL.BLUE_BUTTON
    Ix.return $ EXTI 13 exti

exti_init :: (Member (EXTI pin port) s) => EXTI pin port -> HAL.EXTIConfig -> Setup ns s ns s ()
exti_init (EXTI _ exti) cfg = liftSetupIO $ HAL.exti_init exti cfg

exti_release :: forall s' pin port ns s .
                ( Member (EXTI pin port) s
                , Delete (EXTI pin port) s s'
                )
             => EXTI pin port -> Setup ns s (Cons (EXTI pin port) ns) s' ()
exti_release (EXTI line exti) = Ix.do
    liftSetupIO $ HAL.exti_set_security exti HAL.EXTINonsecure
    liftSetupIO $ HAL.nvic_set_target_nonsecure (extiLineIRQ line)

exti_irqn :: EXTI pin port -> Setup ns s ns s Int
exti_irqn (EXTI line _) = Ix.return $ extiLineIRQ line

exti_enable :: EXTI pin port -> Setup ns s ns s ()
exti_enable (EXTI _ exti) = liftSetupIO $ HAL.exti_enable exti

exti_disable :: EXTI pin port -> Setup ns s ns s ()
exti_disable (EXTI _ exti) = liftSetupIO $ HAL.exti_disable exti

exti_on_secure :: (Member (EXTI pin port) s)
               => EXTI pin port
               -> HAL.EXTIEdge
               -> (HAL.EXTIEdge -> Secure s ())
               -> Setup ns s ns s ()

exti_on_nonsecure :: (Member (EXTI pin port) ns)
                  => EXTI pin port
                  -> HAL.EXTIEdge
                  -> (HAL.EXTIEdge -> Nonsecure ns ())
                  -> Setup ns s ns s ()

#ifdef SECURE
exti_on_secure (EXTI line exti) edge cb = Ix.do
    liftSetupIO $ c_h_exti_register_callback exti (fromIntegral line)
    registerExtiCallback line (fromEnum edge) $ \actualEdge ->
        let Secure ioa = cb (toEnum actualEdge)
         in ioa

exti_on_nonsecure _ _ _ = Ix.return ()
#else
exti_on_secure _ _ _ = Ix.return ()

exti_on_nonsecure (EXTI line exti) edge cb = Ix.do
    liftSetupIO $ c_h_exti_register_callback exti (fromIntegral line)
    registerExtiCallback line (fromEnum edge) $ \actualEdge ->
        let Nonsecure ioa = cb (toEnum actualEdge)
         in ioa
#endif

extiLineIRQ :: Int -> Int
extiLineIRQ line = line + 11
