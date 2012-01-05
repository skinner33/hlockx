{-# LANGUAGE ForeignFunctionInterface #-}

module DPMS
    ( dPMSEnable
    , dPMSDisable
    , dPMSForceLevel
    , dPMSInfo
	, dPMSModeOn
	, dPMSModeStandby
	, dPMSModeSuspend
	, dPMSModeOff
    ) where

import Foreign
import Foreign.C
import Graphics.X11.Xlib

#include "X11/extensions/dpmsconst.h"

type CARD16 = CUInt

dPMSModeOn :: CARD16
dPMSModeOn = #const DPMSModeOn
dPMSModeStandby :: CARD16
dPMSModeStandby = #const DPMSModeStandby
dPMSModeSuspend :: CARD16
dPMSModeSuspend = #const DPMSModeSuspend
dPMSModeOff :: CARD16
dPMSModeOff = #const DPMSModeOff


foreign import ccall unsafe "X11/extensions/dpms.h DPMSEnable"
  dPMSEnable :: Display -> IO Status

foreign import ccall unsafe "X11/extensions/dpms.h DPMSDisable"
  dPMSDisable :: Display -> IO Status

foreign import ccall unsafe "X11/extensions/dpms.h DPMSForceLevel"
  dPMSForceLevel :: Display -> CARD16 -> IO Status

foreign import ccall unsafe "X11/extensions/dpms.h DPMSInfo"
  dPMSInfo :: Display -> Ptr CARD16 -> Ptr Bool -> IO Status


