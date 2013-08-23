import Int_T0
import T0_T1
import T1_IOString

main :: IO ()
main = do
  str <- t1_ioString $ t0_t1 $ int_t0 100
  print str
