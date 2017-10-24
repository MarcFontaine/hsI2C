----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.I2CTest
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module demonstrates how to access I2C hardware.
-- The examples print the barometer and the compass mesurements of an I2C IMU
-- (Inertial mesurement Unit).
-- The examples assume that an IMU unit is attached to //dev//i2c-7.
-- Don't use these examples on the internal I2C bus of your PC or Laptop.
-- (Unless you know what you are doing.)
-- As I2C addresses are only 7 Bit wide there is some chance that the
-- addresses of the IMU chips colide with other hardware and that this
-- other hardware gets confused.

module System.Hardware.I2CTest
(
  testBarometer
 ,testCompass
)
where
import System.Hardware.I2C
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
       
testBarometer :: IO ()
testBarometer = withDevice "/dev/i2c-7" $ \device -> do
  setSlaveAddr device 0x77
  replicateM_ 20 $ do
     writeByte device 0x58
     threadDelay 10000
     res <-readI2CBlockData device 0 3
     writeByte device 0x48
     case parseTemp res of
        Just val -> print val
        _ -> print "Bad Read"
     threadDelay 1000000

parseTemp :: ByteString -> Maybe Integer
parseTemp bs
  = case BS.unpack bs of
      [a,b,c] -> Just $
                        (256*256*(fromIntegral a)
                        + 256*(fromIntegral b)
                        + (fromIntegral b) ) `div` 339
      _ -> Nothing


testCompass :: IO ()
testCompass = withDevice "/dev/i2c-7" $ \device -> do
  --   Turn on I2C Passthrough
  putStrLn "switch MPU to i2c bypass"
  setSlaveAddr device 0x68 --MPU6050
  updateByte device 0x6A $ \b -> b .&. 0xDF
  updateByte device 0x37 $ \b -> b .|. 2
  updateByte device 0x6B $ \b -> b .&.  0xBF
  --   continue with Compass
  putStrLn "compass continius mode"
  setSlaveAddr device 0x1E  -- HMC5883L
  writeByteData device 2 0
  forever $ do
     res <-readI2CBlockData device 03 6
     case parseCompass res of
        Just val -> print val
        _ -> print "Bad Read"
     threadDelay 1000000

        
updateByte :: Device -> Word8 -> (Word8 -> Word8) -> IO ()
updateByte device addr update = do
  r <- readI2CBlockData device addr 1
  writeByteData device addr $ update $ BS.head r

parseCompass :: ByteString -> Maybe (Float,Float,Float)
parseCompass bs
  = case BS.unpack bs of
      [xh,xl,yh,yl,zh,zl] ->Just $ 
        (highLow xh xl, highLow yh yl, highLow zh zl)

      _ -> Nothing
  where
    highLow h l = if testBit h 7
           then 256 * (fromIntegral h) + (fromIntegral l) - 0x10000
           else 256 * (fromIntegral h) + (fromIntegral l)
