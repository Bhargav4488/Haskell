import Data.List 
import Data.Char
import Data.Array (assocs, accumArray, Array)
import qualified Data.Map as Map
import System.IO


continuousSubSeqs = filter (not . (null)) . concatMap inits . tails


frequency :: String -> [(Char,Int)]--finding frequency list of (character,numoftimesofoccurence) from string
frequency = filter (\f -> snd f>0) . assocs . accumArray (+) 0 ('\0','\255') . map (\x->(x,1))


insertinmap::[(Char,Int)]->Int->Map.Map [(Char,Int)] Int->Map.Map [(Char,Int)] Int -- used for inserting in map frequency list to 1 in the map when the frquency list has not occured for any substring before
insertinmap a b a1=a2 where a2=Map.insertWith(+) a b a1

updateinmap::[(Char,Int)]->Map.Map [(Char,Int)] Int->Map.Map [(Char,Int)] Int--used for increasing the count for a particulat frequency list in the map when it is already in the map previosuly
updateinmap s a1=do
	let a = Map.findWithDefault 1 s a1
	let b = a+1
	let a2=Map.delete s a1
	Map.insert s b a2
	

trav1::Map.Map [(Char,Int)] Int->Int->Int->[[Char]]->Map.Map [(Char,Int)] Int
trav1 a1 i len str=do
	let a=frequency (str!!i)
	if ((Map.member a a1)==True) then do
		let a2=updateinmap a a1
		let a3=i+1
		travers a2 a3 len str
	else do
		let a2=insertinmap (frequency(str!!i)) 1 a1
		let a3=i+1
	 	travers a2 a3 len str
	
travers::Map.Map [(Char,Int)] Int->Int->Int->[[Char]]->Map.Map [(Char,Int)] Int --we traverse all substrings and updating the map every time
travers a1 i len str=
	if(i<len) then trav1 a1 i len str else a1
		


toInt :: Float -> Int
toInt x = round x

find1::Int->Int->[Int]->Int->Int  --when we get the frequency vector to count map from each from each such count we can take two elements so nC2.We add all such counts
find1 i len1 b tot=
	if(i<len1) then do
		let c=b!!i
		let c2=c*(c-1)
		let c3=c2 `quot` 2
		let c1=tot+c3
		let i1=i+1
		find1 i1 len1 b c1
	else tot

ana::[Char]->Int
ana c=do
	let str=continuousSubSeqs c    --continuousSubSeqs is used for finding all the substrings of a string
	let len=length str             --total number of possible strings
	if(len==0)then 0
	else do
		let a=frequency (str!!0)		
		let a1=Map.fromList [(a,1)] 
		let a2=travers a1 1 len str     --we find a frequency mapping map from [(Char,Int)] to Int from each substring to count for each [(Char,Int)]
		let b=Map.elems a2
		let len1=length b
		find1 0 len1 b 0
	
	


--a be the set of cards brought by Dylan and b be the set of cards brought by Linda we combine them together to form string c
anagram :: [[Char]]->Int
anagram x=do
	let a=x!!0
	let b=x!!1
	let c=a++b
	ana c

