import Data.List
import Data.Char
import System.Random

--performs product of sum of lists from list of lists
addme::[[Int]]-> Int
addme [[]]=0
addme(x:[])=sum x
addme(x:xs)=(sum x)*(addme xs)

--we define a new data type Conslist
data ConsList elem = Empty | Cons elem (ConsList elem)


--converts from list to haskelllist
toHaskellList Empty	=[]
toHaskellList (Cons x xs)=x:(toHaskellList xs)

--here list cannot be printed directly so we print it as String
printfunc Empty="Empty"
printfunc (Cons x xs)="(Cons "++show(x)++" "++(printfunc xs)++")"


putout x=putStrLn (printfunc x)

todo []=Empty
todo (x:xs)=Cons x (todo xs)

--converts from Haskellist to list
toList x=putout (todo x)



--greatest x y returns the item in y that maximizes function f
greatest :: (a -> Int) -> [a] -> a
greatest x (y:[])=y
greatest x (y:ys)
	|(x y) >=x (greatest x ys) = y
	|otherwise = greatest x ys



