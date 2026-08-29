{-# LANGUAGE CPP #-}

module MicroHasTEE (
    module Effectful.HAL,
    module Effectful.Setup,
    module Effectful.TypeLevel.List,
    module Effectful.TypeLevel.Number,
    module Effectful.TypeLevel.Lock,
    module Domain,
) where

import Effectful.HAL
import Effectful.Setup
import Effectful.TypeLevel.List
import Effectful.TypeLevel.Lock
import Effectful.TypeLevel.Number

#ifdef SECURE
import Effectful.Secure as Domain
#else
import Effectful.NonSecure as Domain
#endif
