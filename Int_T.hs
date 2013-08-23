{-# LANGUAGE TypeFamilies, UndecidableInstances #-}
module Int_T where

import Base

type instance LA = LB -> LB

int_t0 :: Int -> T
int_t0 = T
