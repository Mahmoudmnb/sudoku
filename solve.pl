% merge two list into one
append([],L2,L2).
append([H|T],L2,[H|L3]):-append(T,L2,L3).
% remove element from list (v is the element,L should be empty in the call, [H|T] is the list that we want to remove the element from ,F the list after the delete)
removeElement(_,L,[],F):- append(L,[],F),!.
removeElement(V,L,[H|T],F):-H =\= V ,append(L,[H],NL), removeElement(V,NL,T,F).
removeElement(_,L,[_|T],F):- append(L,T,F),!.
% remove elements from the list (the first parameters is the list of element to remove, the second one the the list to remove from, the third is the result)
removeElements([],R,F):-append(R,[],F).
removeElements([H],R,F):-removeElement(H,[],R,F).
removeElements([H|T],R,F):-removeElement(H,[],R,F1),removeElements(T,F1,F).
% check if the two list are equal
isEqual([],[]).
isEqual([H|T],[H1|T1]):-H=:=H1,isEqual(T,T1).
% check if the list is contains V
contain(V,[H]):-H=:=V.
contain(V,[H|_]):-H=:=V.
contain(V,[_|T]):-contain(V,T).
% search for list of items if contain in other list and return true if on of this items is found ,[first argument is the list to search for the second argument is the list to search in]
containList([],_):-1=:=2.
containList([H|_],L):-contain(H,L).
containList([_|T],L):-containList(T,L).
% fill the cell with all probability(1 -> 9) (X,Y the position of the cell,N is an index should be one , L is list of all probability)
fillProb(X,Y,10,L):-assert(cell(X,Y,L,0)).
fillProb(X,Y,N,L) :- append(L,[N],R), NN is N+1, fillProb(X,Y,NN,R).
% fill all cells with all probability (1->9),(X,Y the first cell position)
fillPuzzleProb(_,10).
fillPuzzleProb(X,Y):- cell(X,Y,_,_),NX is X+1, fillPuzzleProb(NX,Y).
fillPuzzleProb(X,Y):- X<10, fillProb(X,Y,1,[]),NX is X+1, fillPuzzleProb(NX,Y).
fillPuzzleProb(_,Y):- NY is Y+1, fillPuzzleProb(1,NY).
% remove V from probability list in the selected row (V is the value want to remove,[X,Y] position of the cell to ignore,I is an index to start from)
clearRowProb(_,_,_,10,0,0).
clearRowProb(_,_,_,10,1,1).
clearRowProb(V,X,Y,I,S,C):- I =:= X,NI is I+1,clearRowProb(V,X,Y,NI,S,C).
clearRowProb(V,X,Y,I,_,C):- cell(I,Y,R,_),contain(V,R),removeElement(V,[],R,Res),retract(cell(I,Y,_,_)),assert(cell(I,Y,Res,0)),NI is I+1,clearRowProb(V,X,Y,NI,1,C).
clearRowProb(V,X,Y,I,S,C):- NI is I+1,clearRowProb(V,X,Y,NI,S,C).
% remove V from probability list in the selected column (V is the value want to remove,[X,Y] position of the cell to ignore,I is an index to start from)
clearColumnProb(_,_,_,10,1,1).
clearColumnProb(_,_,_,10,0,0).
clearColumnProb(V,X,Y,I,S,C):- I =:=Y,NI is I+1,clearColumnProb(V,X,Y,NI,S,C).
clearColumnProb(V,X,Y,I,_,C):- cell(X,I,R,_),contain(V,R),removeElement(V,[],R,Res),retract(cell(X,I,_,_)),assert(cell(X,I,Res,0)),NI is I+1,clearColumnProb(V,X,Y,NI,1,C).
clearColumnProb(V,X,Y,I,S,C):- NI is I+1,clearColumnProb(V,X,Y,NI,S,C).
% remove V from probability list in the selected Square (V is the value want to remove,[X,Y] position of the cell to ignore,
%[SX,EX,SY,EY] is the boundary of the square indexes)
clearSquareProb(_,_,_,_,_,SY,EY,1,1):-SY>EY.
clearSquareProb(_,_,_,_,_,SY,EY,0,0):-SY>EY.
clearSquareProb(V,X,Y,SX,EX,SY,EY,S,C):-SX=<EX,SX=:=X,SY=:=Y,NSX is SX + 1 ,clearSquareProb(V,X,Y,NSX,EX,SY,EY,S,C).
clearSquareProb(V,X,Y,SX,EX,SY,EY,_,C):-SX=<EX,cell(SX,SY,R,_),contain(V,R),removeElement(V,[],R,Res),retract(cell(SX,SY,_,_)),assert(cell(SX,SY,Res,0)),
                                        NSX is SX+1,clearSquareProb(V,X,Y,NSX,EX,SY,EY,1,C).
clearSquareProb(V,X,Y,SX,EX,SY,EY,S,C):-SX=<EX,NSX is SX + 1 ,clearSquareProb(V,X,Y,NSX,EX,SY,EY,S,C).
clearSquareProb(V,X,Y,SX,EX,SY,EY ,S,C):-SX>EX, NSX is EX-2,NSY is SY+1,clearSquareProb(V,X,Y,NSX,EX,NSY,EY,S,C). 
% clear the value of the cell from the probability in the column, row and square of the selected cell(X,Y is the position of the selected cell
%[SX,EX,SY,EY] is the boundary of the square indexes that the cell in located in)
%! the cell should contain one probability
clearOneProbCell(X,Y,SX,EX,SY,EY,C):-cell(X,Y,[H|_],0),clearColumnProb(H,X,Y,1,0,CC),clearRowProb(H,X,Y,1,0,CR),clearSquareProb(H,X,Y,SX,EX,SY,EY,0,CS),
                                   retract(cell(X,Y,_,_)),assert(cell(X,Y,[H],1)),C is CC+CR+CS.
clearOneProbCell(X,Y,_,_,_,_,0):-cell(X,Y,_,1).
% remove list of probability from cell in selected row (L is the list to remove,[X1,X2] indexes of cells to ignore,y is the number of line,I is an index)
clearRowTwoProb(_,_,_,_,10,0,0).
clearRowTwoProb(_,_,_,_,10,1,1).
clearRowTwoProb(L,X1,X2,Y,I,S,C):- (I =:= X1;I =:= X2),NI is I+1,clearRowTwoProb(L,X1,X2,Y,NI,S,C).
clearRowTwoProb(L,X1,X2,Y,I,_,C):- cell(I,Y,R,_),containList(L,R),removeElements(L,R,Res),retract(cell(I,Y,_,_)),
                                   assert(cell(I,Y,Res,0)),NI is I+1,clearRowTwoProb(L,X1,X2,Y,NI,1,C).
clearRowTwoProb(L,X1,X2,Y,I,S,C):- NI is I+1,clearRowTwoProb(L,X1,X2,Y,NI,S,C).
% remove list of probability from cell in selected column (L is the list to remove,[Y1,Y2] indexes of cells to ignore,X is the number of column,I is an index)
clearColumnTwoProb(_,_,_,_,10,0,0).
clearColumnTwoProb(_,_,_,_,10,1,1).
clearColumnTwoProb(L,Y1,Y2,X,I,S,C):- (I =:= Y1;I =:= Y2),NI is I+1,clearColumnTwoProb(L,Y1,Y2,X,NI,S,C).
clearColumnTwoProb(L,Y1,Y2,X,I,_,C):- cell(X,I,R,_),containList(L,R),removeElements(L,R,Res),retract(cell(X,I,_,_)),
                                      assert(cell(X,I,Res,0)),NI is I+1,clearColumnTwoProb(L,Y1,Y2,X,NI,1,C).
clearColumnTwoProb(L,Y1,Y2,X,I,S,C):- NI is I+1,clearColumnTwoProb(L,Y1,Y2,X,NI,S,C).
% remove list of probability from cell in selected Square (L is the list to remove,[X1,X2,Y1,Y2] indexes of cells to ignore,[SX,EX,SY,EY] is the boundary of the square )
clearSquareTwoProb(_,_,_,_,_,_,_,SY,EY,0,0)      :- SY > EY.
clearSquareTwoProb(_,_,_,_,_,_,_,SY,EY,1,1)      :- SY > EY.
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY,S,C):- (SX=:=X1,SY=:=Y1;SX=:=X2,SY=:=Y2),NSX is SX+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,SY,EY,S,C).
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY,_,C):- SX=<EX,cell(SX,SY,R,_),containList(L,R),removeElements(L,R,Res),retract(cell(SX,SY,_,_)),
                                                   assert(cell(SX,SY,Res,0)),NSX is SX+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,SY,EY,1,C).
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY,S,C):- SX=<EX,NSX is SX+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,SY,EY,S,C).
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY,S,C):- SX>EX,NSX is SX-3,NSY is SY+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,NSY,EY,S,C).
% scan row to check if there any cell contain the same two probability of the cell in index [X,Y] and if there any cell remove this two probability from the other cells in this row
%[X,Y] is the index of the cell to compare with its probability , I is an index
%! this cell should contain just two probability
scanHorizontallyForTwoProbCell(_,_,10,0).
scanHorizontallyForTwoProbCell(X,Y,I,C):-I=\=X,cell(X,Y,R,_),cell(I,Y,TR,_),isEqual(R,TR),clearRowTwoProb(R,X,I,Y,1,0,C).
scanHorizontallyForTwoProbCell(X,Y,I,C):-NI is I+1,scanHorizontallyForTwoProbCell(X,Y,NI,C).
% scan column to check if there any cell contain the same two probability of the cell in index [X,Y] and if there any cell remove this two probability from the other cells in this column
%[X,Y] is the index of the cell to compare with its probability , I is an index
%! this cell should contain just two probability
scanVerticallyForTwoProbCell(_,_,10,0).
scanVerticallyForTwoProbCell(X,Y,I,C):- I=\=Y,cell(X,Y,R,_),cell(X,I,TR,_),isEqual(R,TR),clearColumnTwoProb(R,Y,I,X,1,0,C).
scanVerticallyForTwoProbCell(X,Y,I,C):-NI is I+1,scanVerticallyForTwoProbCell(X,Y,NI,C).
% scan square to check if there any cell contain the same two probability of the cell in index [X,Y] and if there any cell remove this two probability from the other cells in this column
%[X,Y] is the index of the cell to compare with its probability ,[SX,EX,SY,EY] is the boundary of the square
scanSquarelyForTwoProbCell(_,_,_,_,SY,EY,0):-SY>EY.
scanSquarelyForTwoProbCell(X,Y,SX,EX,SY,EY,C):-SX=<EX,SX=:=X,SY=:=Y,NSX is SX+1,scanSquarelyForTwoProbCell(X,Y,NSX,EX,SY,EY,C).
scanSquarelyForTwoProbCell(X,Y,SX,EX,SY,EY,C):-SX=<EX,cell(X,Y,R,_),cell(SX,SY,TR,_),isEqual(R,TR),TSX is EX-2,TSY is EY-2,
                                           clearSquareTwoProb(R,X,Y,SX,SY,TSX,EX,TSY,EY,0,C).
scanSquarelyForTwoProbCell(X,Y,SX,EX,SY,EY,C):-SX=<EX,NSX is SX+1,scanSquarelyForTwoProbCell(X,Y,NSX,EX,SY,EY,C).
scanSquarelyForTwoProbCell(X,Y,SX,EX,SY,EY,C):-SX>EX,NSX is EX-2,NSY is SY+1,scanSquarelyForTwoProbCell(X,Y,NSX,EX,NSY,EY,C).
% clear the two of the cell probability from the probability in the column, row and square of the selected cell(X,Y is the position of the selected cell
%[SX,EX,SY,EY] is the boundary of the square indexes that the cell in located in)
%! the cell should contain just two probability
clearTwoCellProb(X,Y,SX,EX,SY,EY,C):-scanVerticallyForTwoProbCell(X,Y,1,CV),scanHorizontallyForTwoProbCell(X,Y,1,CH),scanSquarelyForTwoProbCell(X,Y,SX,EX,SY,EY,CS),C is CV+CH+CS.
% check if v is founded in the other cells in row prob and return false if founded and true if not [X,Y index of the cell to ignore , I is an index]
checkIfValueIsUniqHorizontally(_,_,_,10).
checkIfValueIsUniqHorizontally(X,Y,V,I):-I=:=X,NI is I+1,checkIfValueIsUniqHorizontally(X,Y,V,NI).
checkIfValueIsUniqHorizontally(_,Y,V,I):-cell(I,Y,R,_),contain(V,R),1=:=2.
checkIfValueIsUniqHorizontally(X,Y,V,I):-cell(I,Y,R,_),not(contain(V,R)),NI is I+1,checkIfValueIsUniqHorizontally(X,Y,V,NI).
% check if v is founded in the other cells in column prob and return false if founded and true if not [X,Y index of the cell to ignore , I is an index]
checkIfValueIsUniqVertically(_,_,_,10).
checkIfValueIsUniqVertically(X,Y,V,I):-I=:=Y,NI is I+1,checkIfValueIsUniqVertically(X,Y,V,NI).
checkIfValueIsUniqVertically(X,_,V,I):-cell(X,I,R,_),contain(V,R),1=:=2.
checkIfValueIsUniqVertically(X,Y,V,I):-cell(X,I,R,_),not(contain(V,R)),NI is I+1,checkIfValueIsUniqVertically(X,Y,V,NI).
% check if v is founded in the other cells in square prob and return false if founded and true if not [X,Y index of the cell to ignore , I is an index,(SX,SY,EX,EY) is the boundary of the square]
checkIfValueIsUniqSquarely(_,_,_,_,_,SY,EY):-SY>EY.
checkIfValueIsUniqSquarely(X,Y,V,SX,EX,SY,EY):-SX=<EX,X=:=SX,Y=:=SY,NSX is SX+1,checkIfValueIsUniqSquarely(X,Y,V,NSX,EX,SY,EY).
checkIfValueIsUniqSquarely(_,_,V,SX,EX,SY,_):-SX=<EX,cell(SX,SY,R,_),contain(V,R),1=:=2.
checkIfValueIsUniqSquarely(X,Y,V,SX,EX,SY,EY):-SX=<EX,cell(SX,SY,R,_),not(contain(V,R)),NSX is SX+1,checkIfValueIsUniqSquarely(X,Y,V,NSX,EX,SY,EY).
checkIfValueIsUniqSquarely(X,Y,V,SX,EX,SY,EY):-SX>EX,NSY is SY+1,NSX is EX-2,checkIfValueIsUniqSquarely(X,Y,V,NSX,EX,NSY,EY).
%scan to check if the any uniq prob in the cell and delete the other probabilities
scanForUniqCell(_,_,_,_,_,_,[],0).                       
scanForUniqCell(X,Y,SX,EX,SY,EY,[V|_],1):-checkIfValueIsUniqSquarely(X,Y,V,SX,EX,SY,EY),retract(cell(X,Y,_,_)),assert(cell(X,Y,[V],0)).
scanForUniqCell(X,Y,_,_,_,_,[V|_],1):-checkIfValueIsUniqVertically(X,Y,V,1),retract(cell(X,Y,_,_)),assert(cell(X,Y,[V],0)).
scanForUniqCell(X,Y,_,_,_,_,[V|_],1):-checkIfValueIsUniqHorizontally(X,Y,V,1),retract(cell(X,Y,_,_)),assert(cell(X,Y,[V],0)).
scanForUniqCell(X,Y,SX,EX,SY,EY,[_|T],S):-scanForUniqCell(X,Y,SX,EX,SY,EY,T,S).
% solve the cells that contain uniq probability
solveUniqCellProb(X,Y,SX,EX,SY,EY,C):- cell(X,Y,R,_),scanForUniqCell(X,Y,SX,EX,SY,EY,R,C).
% scan square and solve cells that can be solved [SX,EX,SY,EY] is the boundary of the square
solveSquareInPuzzle(_,_,SY,EY,0,0):-SY>EY.
solveSquareInPuzzle(_,_,SY,EY,S,1):-SY>EY,S>0.
solveSquareInPuzzle(SX,EX,SY,EY,S,C):-SX=<EX,cell(SX,SY,_,1),NSX is SX+1,solveSquareInPuzzle(NSX,EX,SY,EY,S,C).
solveSquareInPuzzle(SX,EX,SY,EY,_,C):-SX=<EX,cell(SX,SY,R,0),length(R,L),L=:=1,TSX is EX-2,TSY is EY-2,
                                  clearOneProbCell(SX,SY,TSX,EX,TSY,EY,CO),NSX is SX+1,solveSquareInPuzzle(NSX,EX,SY,EY,CO,C).
solveSquareInPuzzle(SX,EX,SY,EY,_,C):-SX=<EX,cell(SX,SY,R,0),length(R,L),L=:=2,TSX is EX-2,TSY is EY-2,NSX is SX+1,
                                  clearTwoCellProb(SX,SY,TSX,EX,TSY,EY,CT),TTSX is EX-2,TTSY is EY-2,solveUniqCellProb(SX,SY,TTSX,EX,TTSY,EY,CU),NS is CT+CU,solveSquareInPuzzle(NSX,EX,SY,EY,NS,C).
solveSquareInPuzzle(SX,EX,SY,EY,_,C):-SX=<EX,NSX is SX+1,TTSX is EX-2,TTSY is EY-2,solveUniqCellProb(SX,SY,TTSX,EX,TTSY,EY,SU),solveSquareInPuzzle(NSX,EX,SY,EY,SU,C).
solveSquareInPuzzle(SX,EX,SY,EY,S,C):-SX>EX,NSX is EX-2,NSY is SY+1,solveSquareInPuzzle(NSX,EX,NSY,EY,S,C).
% check if the puzzle is solved or not
isSolved(_,10).
isSolved(X,Y):- X=<9,cell(X,Y,R,_),length(R,L),L=:=1,NX is X+1,isSolved(NX,Y).
isSolved(X,Y):- X>9,NX is 1, NY is Y+1,isSolved(NX,NY).
%scan tha puzzle to search for solve
scan(0,R):-        isSolved(1,1),R is 1,!.
scan(0,0).
scan(_,R):-        solveSquareInPuzzle(1,3,1,3,0,C1),solveSquareInPuzzle(4,6,1,3,0,C2),solveSquareInPuzzle(7,9,1,3,0,C3),
                   solveSquareInPuzzle(1,3,4,6,0,C4),solveSquareInPuzzle(4,6,4,6,0,C5),solveSquareInPuzzle(7,9,4,6,0,C6),
                   solveSquareInPuzzle(1,3,7,9,0,C7),solveSquareInPuzzle(4,6,7,9,0,C8),solveSquareInPuzzle(7,9,7,9,0,C9),
                   NC is (C1+C2+C3+C4+C5+C6+C7+C8+C9),scan(NC,R),!.
% solve the puzzle
solvePuzzle(R):-fillPuzzleProb(1,1),scan(1,R),!.