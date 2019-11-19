import Data.Array
type Board = Array (Int,Int) Int --Board is stored as an array where(Int,Int) is the possition in the board and Int is the count is the value in the board


solve :: Board -> IO()
solve b= do
  let sol=solutions b --returns all the solutions to the puzzle using backtracking
  if((length sol)==0) then putStrLn "No solution" --if length ==0 ->no solution
    else do
      let a=(sol!!0) --we print the first solution from the list of solutions
      mapM_ putStrLn [show $ (elementsinrow a row 0 []) | row <- [0..8]]






solutions' []     b = [b]
solutions' (x:xs) b = do
  let posscandidates=possiblecandidates 1 x b [] --finding out all the possible candidates for a position x in the board
  let candidateBoards=map (\p -> addingnewvalue p x b) posscandidates--putting those candidiates in those places on the board 
  concatMap (solutions' xs) candidateBoards

solutions :: Board -> [Board]
solutions b= solutions' (emptylocations b) b
 

possiblecandidates num x b list=            --finding out all the possible candidates for a position x in the board all of which are stored in the list
  if((possibility num x b)==True) then do
    let list1=list++[num]
    if(num==9) then list1
    else (possiblecandidates (num+1) x b list1)
  else do
    if (num==9) then list
    else (possiblecandidates (num+1) x b list) 


find1 i j b list= --inserting the (i,j) in the list if the place has value 0 or else 
  if(b!(i,j)==0)then list++[(i,j)]
    else list

emp i j b list=do --recursive func for finding all the empty locations in the board
  let x=find1 i j b list
  if(i==8 && j==8) then x
    else if(j==8) then (emp (i+1) 0 b x)
      else (emp i (j+1) b x)


emptylocations b=emp 0 0 b []--used for finding all empty locations from the board



possibility :: Int -> (Int,Int) -> Board -> Bool          --checking for the constraints for putting a value in the position (row,col) in the board
possibility m (row, col) b = notInRow && notInColumn && notInBox
  where
    notInRow    = notElem m (elementsinrow b row 0 [])
    notInColumn = notElem m (elementsincolumn b 0 col [])
    notInBox    = notElem m (elementsinmatrix b (row, col))



addingnewvalue val (row,col) b=b // [((row, col), val)]

elementsinrow b row j list=do
  let x=list++[b!(row,j)]
  if(j==8) then x
    else elementsinrow b row (j+1) x



elementsincolumn b i col list=do
  let x=list++[b!(i,col)]
  if(i==8) then x
    else elementsincolumn b (i+1) col x



eleinmatrix b i j m n list=do
  let x= list++[b!(i,j)]
  if(i==m && j==n) then x
    else if(j==n) then (eleinmatrix b (i+1) (j-2) m n x)
      else (eleinmatrix b i (j+1) m n x)

elementsinmatrix b (row,col)=do
  let x=(div row 3)
  let startrow=x*3
  let y=(div col 3)
  let startcol=y*3
  eleinmatrix b startrow startcol (startrow+2) (startcol+2) []




listtoarray = concatMap rowAssocs . zip [0..8]
  where
    rowAssocs (row, marks) = colAssocs row $ zip [0..8] marks
    colAssocs row cols = map (\(col, m) -> ((row, col), m)) cols



sudoku::[[Int]]->Board
sudoku board= array ((0, 0), (8, 8)) (listtoarray board)


solvesudoku board= do
    let b=sudoku board
    solve b


--[[5, 3, 0,  0, 7, 0,  0, 0, 0],[6, 0, 0,  1, 9, 5,  0, 0, 0],[0, 9, 8,  0, 0, 0,  0, 6, 0],[8, 0, 0,  0, 6, 0,  0, 0, 3],[4, 0, 0,  8, 0, 3,  0, 0, 1],[7, 0, 0,  0, 2, 0,  0, 0, 6],[0, 6, 0,  0, 0, 0,  2, 8, 0],[0, 0, 0,  4, 1, 9,  0, 0, 5],[0, 0, 0,  0, 8, 0,  0, 7, 0]]