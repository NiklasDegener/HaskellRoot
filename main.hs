import Debug.Trace
import Data.Ratio

--main = do
  --putStrLn rootn 5

rootSin :: Rational -> Rational -> Rational -> Rational
rootSin val cur factor =
  if (cur*cur == val) then
      cur
    else
      if (cur*cur < val) then
      --trace ("Current Number = " ++ show cur) $
      rootSin val (cur+(1/factor)) factor
    else
      cur-1/factor

rootMult:: Rational -> Rational -> Rational -> Rational -> Rational
rootMult n it maxIt akku = if (it==maxIt) then
   akku
   else
     --trace("Current akku = " ++ show akku) $
     rootMult n (it+1) maxIt (rootSin n akku (it*10))

rootn n = rootSin n 0 10
rootnn n m l = rootSin n m l
rootm n = rootMult n 1 100 0

fac n = if n == 0 then 1 else n * fac (n - 1)

potenzen = [2^n | n<-[1..100]]
