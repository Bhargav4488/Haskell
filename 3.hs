import Data.List
import Data.Char
import System.IO
import Data.Time
import System.Random 
import Data.Array.IO
import Data.Map
import Data.IORef
import System.IO.Unsafe

list=["BS","CM","CH","CV","CS","DS","EE","HU","MA","ME","PH","ST"]--list of the teams participating

--fixtures=[["BS","CM","1-11","9:30AM"],["CH","CV","1-11","7:30PM"],["CS","DS","2-11","9:30AM"],["EE","HU","2-11","7:30PM"],["MA","ME","3-11","9:30AM"],["PH","ST","3-11","7:30PM"]]

fixtures::IORef [[String]]--we define a IORef which list of list of strings
{-# NOINLINE fixture#-}
fixtures=unsafePerformIO (newIORef [[]])


getval=do
	readIORef fixtures --used for the reading the mutable varaiables
	



printmatches i len a1 = --used for printing all matches from fixtures
	if(i<len) then do
		let a =(a1!!i)
		putStrLn ((a!!0)++" VS "++(a!!1)++"   "++(a!!2)++"   "++(a!!3))
		let i1=i+1
		printmatches i1 len a1		 
	else putStr ""


printteammatch i len a1 s= --used for printing particular match of a team
	if(i<len) then do
		let a =(a1!!i)
		if(s==(a!!0)) then putStrLn ((a!!0)++" VS "++(a!!1)++"   "++(a!!2)++"   "++(a!!3))
		else if(s==(a!!1)) then putStrLn ((a!!1)++" VS "++(a!!0)++"   "++(a!!2)++"   "++(a!!3))
		else do
			let i1=i+1
			printteammatch i1 len a1 s
	else putStrLn "Wrong input"


timefunc::Int->String                                --timefunc for generating time alteratively for each match
timefunc i=
	if (mod i 4==0) then "07:30" else "21:30"

datefunc::Int->Int->Int                               --used for deteerming the date of the match
datefunc i date=
	if (mod i 4==0) then date else (date+1)

getready::[String]->Int->Int->Int->[[String]]->[[String]]--generating fixtures from the teams in list matches between teams 1 and 2 and between3 and 4 and so on
getready fixture i date len1 l=
	if(i>=len1) then l
	else do
		let team1=(fixture!!i)
		let team2=(fixture!!(i+1))
		let time=timefunc i
		let date1=datefunc i date
		let strdate=show date
		let b=[team1,team2,strdate,time]
		let l1=b:l
		let i1=i+2
		(getready fixture i1 date1 len1 l1)


fixture s=do
	let len1=length list
	
	
	
	if s=="all" then do              -- if s=="all " we generate an altogether different random fixtures
		newlist<-shuffle list         --generates random list every time from the list
		let b=getready newlist 0 1 len1 [] -- generating fixtures beetween (1,2) teams ,(3,4) teams and so on
		writeIORef fixtures b             --writing to the mutable variable
		let a=reverse b
		let len=length a
		printmatches 0 len a  --printing all the fixtures
		
	else do
		b<-readIORef fixtures  --reading from the mutable varaiable
		let a =reverse b
		if(elem s list) then do
			let len=length a
			printteammatch 0 len a s--printing only the given teams match

		else putStrLn "Wrong input" 


timeFormat = "%H:%M"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat

times::String->UTCTime
times s=understandTime s



printnextmatch i len1 a date time1=do --recursion for printing the next match
	if(i>=len1) then putStrLn "All matches done"
	else do
		let a1=(a!!i)
		let date1=(a1!!2)
		let time=(a1!!3)
		if(date1==date) then
			if ((understandTime time)>time1) then putStrLn ((a1!!0)++" VS "++(a1!!1)++"   "++(a1!!2)++"   "++(a1!!3))
				else
					if ((i+1)>=len1) then putStrLn "All matches done"
					else do
						let a2=(a!!(i+1))
						if((understandTime (a2!!3)) > time1) then putStrLn ((a2!!0)++" VS "++(a2!!1)++"   "++(a2!!2)++"   "++(a2!!3))
						else 
							if((i+2)>=len1) then putStrLn "All matches done"
								else do
									let a3=a!!(i+2)
									putStrLn ((a3!!0)++" VS "++(a3!!1)++"   "++(a3!!2)++"   "++(a3!!3))
		else do
			printnextmatch (i+1) len1 a date time1



isval::Char->Bool --checking if the string contains only number or not
isval c
	|c `elem` ['0'..'9']=True
	|otherwise=False

checkfornum s=(all isval s)

check date --checking whether date is correct or not for input to func nextmatch
	|(read date) `elem` [1..30] = True
	|otherwise=False

checktime time= --checking whether time is correct or not for input to func nectmatch
	let a=Data.List.filter (/=' ') time
	if((length a)==5) then
		if((a!!2==':') && checkfornum (Data.List.take 2 a) && checkfornum (Data.List.drop 3 a) ) then True
			else False
	else False
		

nextmatch date time=  --used for the next match from the fixtures given a date and time
	if(checkfornum date==False||check date==False)then putStrLn("Wrong date")
	else
		if(checktime time==False) then putStrLn "Wrong time"
		else do
			let time2=times "23:59"
			let time1=times time
			if(time1>time2||((checktime time)==False)) then putStrLn "WrongTime"
			else do
				let len1=length list
				b<-readIORef fixtures
				let a =reverse b
				let len=length a
				printnextmatch 0 len a date time1
	

shuffle x = if length x < 2 then return x else do--used fro randomly shuffling a list
	i <- System.Random.randomRIO (0, length(x)-1)
	r <- shuffle ((Data.List.take i x) ++ (Data.List.drop (i+1) x))
	return (x!!i : r)

 
