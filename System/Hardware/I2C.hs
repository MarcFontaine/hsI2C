----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.I2C
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module provides an I2C interface for Haskell.
-- It just reexports the implementation specific module.
-- For now, only LinuxDevI2C is supported.

module System.Hardware.I2C
(
  module System.Hardware.LinuxDevI2C
)
where
import System.Hardware.LinuxDevI2C
