import Debug.Trace
import Data.Ratio

rootSin :: Rational -> Rational -> Rational -> Rational
rootSin val cur factor =
  if (cur*cur == val) then
      cur
    else
      if (cur*cur < val) then
      rootSin val (cur+(1/factor)) factor
    else
      cur-1/factor

rootMult:: Rational -> Integer -> Integer -> Rational -> Double
rootMult n it maxIt akku = if (it==maxIt+1) then
   fromRational(akku)
   else
     rootMult n (it+1) maxIt (rootSin n akku (10^it))

root n m = rootMult n 1 m 0

fac n = if n == 0 then 1 else n * fac (n - 1)

potenzen = [2^n | n<-[1..100]]
