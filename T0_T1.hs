{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module T0_T1 where

import Base

type instance LB = LC -> LC

t0_t1 :: T0 -> T1
t0_t1 (T0 x) = T1 x
