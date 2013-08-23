{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module T1_IOString where

import Base

type instance LC = LA

t1_ioString :: T1 -> IO String
t1_ioString (T1 x) = x
