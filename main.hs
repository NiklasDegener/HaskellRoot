import Debug.Trace
import Data.Ratio

rootSin :: Rational -> Integer -> Rational -> Rational -> Rational
rootSin val pot cur factor =
  if (cur^pot == val) then
      cur
    else
      if (cur^pot < val) then
        rootSin val pot (cur+((10^((rationalLength val 0)-1))/factor)) factor
    else
      cur-((10^((rationalLength val 0)-1))/factor)

rootMult:: Rational -> Integer -> Integer  -> Integer -> Rational -> Rational
rootMult n pot it maxIt akku = if (it==maxIt+1) then
    akku
   else
     rootMult n pot (it+1) maxIt (rootSin n pot akku (10^it))

root n pot m = rootMult n pot 0 (m+(rationalLength n 0)-1) 0

rationalLength:: Rational -> Integer -> Integer
rationalLength rat res = if (rat < 10) then
    res+1
  else
    rationalLength (rat/10) (res+1)
