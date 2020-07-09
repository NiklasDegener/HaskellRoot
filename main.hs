import Debug.Trace
import Data.Ratio
import Data.NumberLength

rootSin :: Rational -> Integer -> Rational -> Rational -> Rational
rootSin val pot cur factor =
  if (cur^pot == val) then
      cur
    else
      if (cur^pot < val) then
      rootSin val pot (cur+(1/factor)) factor
    else
      cur-1/factor

rootMult:: Rational -> Integer -> Integer  -> Integer -> Rational -> Rational
rootMult n pot it maxIt akku = if (it==maxIt+1) then
    trace("Länge: " ++ lengthInteger(akku))
    akku
   else
     rootMult n pot (it+1) maxIt (rootSin n pot akku (10^it))

root n pot m = rootMult n pot 1 m 0
