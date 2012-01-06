{-# LANGUAGE ForeignFunctionInterface #-}

module DPMS
    ( dPMSEnable
    , dPMSDisable
    , dPMSForceLevel
    , dPMSInfo
    , dPMSGetTimeouts
    , dPMSSetTimeouts
    , getCurrentDPMSStatus
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

foreign import ccall unsafe "X11/extensions/dpms.h DPMSGetTimeouts"
  dPMSGetTimeouts :: Display -> Ptr CARD16 -> Ptr CARD16 -> Ptr CARD16 -> IO Bool

foreign import ccall unsafe "X11/extensions/dpms.h DPMSSetTimeouts"
  dPMSSetTimeouts :: Display -> CARD16 -> CARD16 -> CARD16 -> IO Status

getCurrentDPMSStatus :: Display -> IO (CARD16, CARD16, CARD16, Bool)
getCurrentDPMSStatus dpy =
	alloca $ \pOff ->
	alloca $ \pSuspend ->
	alloca $ \pStandby ->
	alloca $ \dpmsWasActive ->
	alloca $ \dpmsPowerLevel -> do
		_ <- dPMSInfo dpy dpmsPowerLevel dpmsWasActive
		_ <- dPMSGetTimeouts dpy pStandby pSuspend pOff
		oldStandby <- peek pStandby
		oldSuspend <- peek pSuspend
		oldOff     <- peek pOff
		oldStatus  <- peek dpmsWasActive
		return (oldStandby, oldSuspend, oldOff, oldStatus)

