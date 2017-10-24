----------------------------------------------------------------------------
-- |
-- Module      :  System.Hardware.LinuxDevI2C
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module provides an I2C interface based Linux //dev/i2c-xxx

{-#  LANGUAGE ForeignFunctionInterface #-}
module System.Hardware.LinuxDevI2C
(
  Addr
 ,Command
 ,Device
 ,withDevice
 ,openDevice
 ,closeDevice
 ,setSlaveAddr
 ,setSlaveAddrForce
 ,writeByte
 ,writeByteData
 ,readByte
 ,readByteData
 ,readI2CBlockData
 ,writeI2CBlockData
)

where

import Data.ByteString as BS       
import System.Posix.IO (openFd,closeFd,OpenMode(..),defaultFileFlags)
import System.Posix.Types (Fd(..))
import Foreign.C.Types
import Foreign.C.Error (Errno (..), errnoToIOError) 
import Foreign.ForeignPtr (withForeignPtr,withForeignPtr,mallocForeignPtrBytes)
import Foreign.Ptr (Ptr (..))
import Foreign.Ptr (Ptr (..),castPtr)
import GHC.Stack (HasCallStack,callStack,prettyCallStack)
import Control.Exception.Base (bracket)
import Data.Word (Word8)
import Control.Monad
       
type Addr = Word8
type Command = Word8
type Device = Fd
type File=CInt

-- | Open an I2C device run, an IO action and close the device again.
-- A device file on Linux could be for example //dev//i2c-7. 
withDevice :: FilePath -> (Device -> IO a) -> IO a
withDevice fp action = bracket (openDevice fp) closeDevice action

-- | Open an I2C Device.                    
openDevice :: FilePath -> IO Device
openDevice fp = openFd fp ReadWrite Nothing defaultFileFlags

-- | Close an I2C Device.
closeDevice :: Device -> IO ()
closeDevice = closeFd

-- | Set the address of the I2C Slave
setSlaveAddr :: HasCallStack => Device -> Addr -> IO ()
setSlaveAddr (Fd file) addr
  = checkReturn $ ioctl_set_slave_addr file 0x0703 (fromIntegral addr)

setSlaveAddrForce :: HasCallStack => Device -> Addr -> IO ()
setSlaveAddrForce (Fd file) addr
  = checkReturn $ ioctl_set_slave_addr file 0x0706 (fromIntegral addr)

writeByte :: HasCallStack => Device -> Command -> IO ()
writeByte (Fd file) cmd
   = checkReturn $ c_writeByte file (fromIntegral cmd)

writeByteData :: HasCallStack => Device -> Command -> Word8 -> IO ()
writeByteData (Fd file) cmd byte
   = checkReturn $ c_writeByteData file (fromIntegral cmd) (fromIntegral byte)

readByte :: HasCallStack => Device -> IO ()
readByte (Fd file)
   = checkReturn $ c_readByte file
   
readByteData :: HasCallStack => Device -> Command -> IO Word8
readByteData (Fd file) cmd
   = castRet fromIntegral $ c_readByteData file (fromIntegral cmd)
          
readI2CBlockData :: HasCallStack => Device -> Command -> Int -> IO ByteString
readI2CBlockData (Fd file) cmd size = do
   checkBlockSizeLimit size
   p <- mallocForeignPtrBytes size
   withForeignPtr p  $ \ptr -> do
      readLen <- (c_readI2CBlockData
                         file
                         (fromIntegral cmd)
                         (fromIntegral size)
                         (castPtr ptr))
      when (fromIntegral readLen /= size) $ do
        fail ("short read :\n"
              ++ "read :" ++ show readLen
              ++ "  expected :" ++ show size
              ++ "\n" ++ prettyCallStack callStack
              )
      packCStringLen (ptr,size)       

   
writeI2CBlockData :: HasCallStack => Device -> Command -> ByteString -> IO ()
writeI2CBlockData (Fd file) cmd block
  = BS.useAsCStringLen block $ \(cstring,len) -> do
      checkBlockSizeLimit len
      checkReturn $ c_writeI2CBlockData file
           (fromIntegral cmd) (fromIntegral len) (castPtr cstring)

checkBlockSizeLimit :: HasCallStack => Int -> IO ()
checkBlockSizeLimit s
  = when (s > 32) $ do
     fail ("blockSize>32 ( blockSize=" ++ show s ++ ") \n"
           ++ prettyCallStack callStack)

checkReturn :: HasCallStack => IO CInt -> IO ()
checkReturn action = do
   ret <- action
   if ret < 0
      then ioError $ errnoToIOError (prettyCallStack callStack)
                      (Errno ret) Nothing Nothing
      else return ()
   
castRet :: HasCallStack => (CInt -> a) -> IO CInt -> IO a
castRet cast action = do
   ret <- action
   if ret < 0
      then ioError $ errnoToIOError (prettyCallStack callStack)
                      (Errno ret) Nothing Nothing
      else return $ cast ret

foreign import ccall unsafe
    "smbus.h i2c_smbus_write_quick" c_writeQuick
    :: File -> CUChar -> IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_read_byte" c_readByte
    :: File -> IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_write_byte" c_writeByte
    :: File -> CUChar ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_read_byte_data" c_readByteData
    :: File -> CUChar -> IO CInt
       
foreign import ccall unsafe
    "smbus.h i2c_smbus_write_byte_data" c_writeByteData
    :: File -> CUChar -> CUChar -> IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_read_word_data" c_readWordData
    :: File -> CUChar -> IO CInt
       
foreign import ccall unsafe
    "smbus.h i2c_smbus_write_word_data" c_writeWordData
    :: File -> CUChar -> CUShort ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_process_call" c_processCall
    :: File -> CUChar -> CUShort ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_read_block_data" c_readBlockData
    :: File -> CUChar -> Ptr CUChar ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_write_block_data" c_writeBlockData
    :: File -> CUChar -> CUChar -> Ptr CUChar ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_read_i2c_block_data" c_readI2CBlockData
    :: File -> CUChar -> CUChar -> Ptr CUChar ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_write_i2c_block_data" c_writeI2CBlockData
    :: File -> CUChar -> CUChar -> Ptr CUChar ->IO CInt

foreign import ccall unsafe
    "smbus.h i2c_smbus_block_process_call" c_blockProcessCall
    :: File -> CUChar -> CUChar -> Ptr CUChar ->IO CInt

foreign import ccall unsafe
    "sys/ioctl.h ioctl" ioctl_set_slave_addr
    :: File -> CULong -> CInt ->IO CInt
