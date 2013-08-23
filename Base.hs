{-# LANGUAGE TypeFamilies #-}
module Base where

-- This program demonstrates how Int can be cast to (IO String)
-- using GHC 7.6.3.

type family F a
type instance F (a -> a) = Int
type instance F (a -> a -> a) = IO String

-- Given this type family F, it is sufficient to prove
-- (LA -> LA) ~ (LA -> LA -> LA)
-- for some LA. This needs to be done in such a way that
-- GHC does not notice LA is an infinite type, otherwise
-- it will complain.
--
-- This can be done by using 3 auxiliary modules, each of which
-- provides a fragment of the proof using different partial knowledge
-- about the definition of LA.
--
-- LA -> LA
-- = {LA~LB}     -- only Int_T0.hs knows this
-- LA -> LB
-- = {LB~LC->LC} -- only T0_T1.hs knows this
-- LA -> LC -> LC
-- = {LA~LC}     -- only T1_IOString.hs knows this
-- LA -> LA -> LA

type family LA
type family LB
type family LC

data T0 = T0 (F (LA -> LB))
data T1 = T1 (F (LA -> LC -> LC))
