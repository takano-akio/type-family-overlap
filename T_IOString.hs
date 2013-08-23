{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module T_IOString where

import Base

type instance LB = LA

t_ioString :: T -> IO String
t_ioString (T x) = x
