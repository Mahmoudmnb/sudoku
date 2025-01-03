% merge two list into one
append([],L2,L2).
append([H|T],L2,[H|L3]):-append(T,L2,L3).
% remove element from list (v is the element,L should be empty in the call, [H|T] is the list that we want to remove the element from ,F the list after the delete)
removeElement(V,L,[H|T],F):-H =\= V ,append(L,[H],NL), removeElement(V,NL,T,F).
removeElement(_,L,[_|T],F):- append(L,T,F).
removeElement(_,L,[],F):- append(L,[],F).
% remove elements from the list (the first parameters is the list of element to remove, the second one the the list to remove from, the third is the result)
removeElements([],R,F):-append(R,[],F).
removeElements([H],R,F):-removeElement(H,[],R,F).
removeElements([H|T],R,F):-removeElement(H,[],R,F1),removeElements(T,F1,F).
% check if the two list are equal
isEqual([],[]).
isEqual([H|T],[H1|T1]):-H=:=H1,isEqual(T,T1).
% fill the cell with all probability(1 -> 9) (X,Y the position of the cell,N is an index should be one , L is list of all probability)
fillProb(X,Y,10,L):-assert(cell(X,Y,L,0)).
fillProb(X,Y,N,L) :- append(L,[N],R), NN is N+1, fillProb(X,Y,NN,R).
% fill all cells with all probability (1->9),(X,Y the first cell position)
fillPuzzleProb(_,10).
fillPuzzleProb(X,Y):- cell(X,Y,_,_),NX is X+1, fillPuzzleProb(NX,Y).
fillPuzzleProb(X,Y):- X<10, fillProb(X,Y,1,[]),NX is X+1, fillPuzzleProb(NX,Y).
fillPuzzleProb(_,Y):- NY is Y+1, fillPuzzleProb(1,NY).
% reomve V from probability list in the selected row (V is the value want to remove,[X,Y] position of the cell to ignore,I is an index to start from)
clearRowProb(_,_,_,10).
clearRowProb(V,X,Y,I):- I =\=X,cell(I,Y,R,_),removeElement(V,[],R,Res),retract(cell(I,Y,_,_)),assert(cell(I,Y,Res,0)),NI is I+1,clearRowProb(V,X,Y,NI).
clearRowProb(V,X,Y,I):- I =:= X,NI is I+1,clearRowProb(V,X,Y,NI).
% reomve V from probability list in the selected column (V is the value want to remove,[X,Y] position of the cell to ignore,I is an index to start from)
clearColumnProb(_,_,_,10).
clearColumnProb(V,X,Y,I):- I =\=Y,cell(X,I,R,_),removeElement(V,[],R,Res),retract(cell(X,I,_,_)),assert(cell(X,I,Res,0)),NI is I+1,clearColumnProb(V,X,Y,NI).
clearColumnProb(V,X,Y,I):- I =:=Y,NI is I+1,clearColumnProb(V,X,Y,NI).
% reomve V from probability list in the selected Square (V is the value want to remove,[X,Y] position of the cell to ignore,
%[SX,EX,SY,EY] is the boundary of the square indexes)
clearSquareProb(_,_,_,_,_,SY,EY):-SY>EY.
clearSquareProb(V,X,Y,SX,EX,SY,EY):-SX=<EX,cell(SX,SY,R,_),removeElement(V,[],R,Res),retract(cell(SX,SY,_,_)),assert(cell(SX,SY,Res,0)),
                                    NSX is SX+1,clearSquareProb(V,X,Y,NSX,EX,SY,EY).
clearSquareProb(V,X,Y,_,EX,SY,EY):-SY=<EY, NSX is EX-3,NSY is SY+1,clearSquareProb(V,X,Y,NSX,EX,NSY,EY). 
% clear the value of the cell from the probalility in the column, row and square of the selected cell(X,Y is the position of the selected cell
%[SX,EX,SY,EY] is the boundary of the square indexes that the cell in located in)
%! the cell should contain one probability
clearOneProbCell(X,Y,SX,EX,SY,EY):-cell(X,Y,[H|_],0),clearColumnProb(H,X,Y,1),clearRowProb(H,X,Y,1),clearSquareProb(H,X,Y,SX,EX,SY,EY),
                                   retract(cell(X,Y,_,_)),assert(cell(X,Y,[H],1)).
clearOneProbCell(X,Y,_,_,_,_):-cell(X,Y,_,1).
% remove list of probability from cell in selected row (L is the list to remove,[X1,X2] indexes of cells to ignore,y is the number of line,I is an index)
clearRowTwoProb(_,_,_,_,10).
clearRowTwoProb(L,X1,X2,Y,I):- I =\=X1,I=\=X2,cell(I,Y,R,_),removeElements(L,R,Res),retract(cell(I,Y,_,_)),
                               assert(cell(I,Y,Res,0)),NI is I+1,clearRowTwoProb(L,X1,X2,Y,NI).
clearRowTwoProb(L,X1,X2,Y,I):- (I =:= X1;I =:= X2),NI is I+1,clearRowTwoProb(L,X1,X2,Y,NI).
% remove list of probability from cell in selected column (L is the list to remove,[Y1,Y2] indexes of cells to ignore,X is the number of column,I is an index)
clearColumnTwoProb(_,_,_,_,10).
clearColumnTwoProb(L,Y1,Y2,X,I):- I =\=Y1,I=\=Y2,cell(X,I,R,_),removeElements(L,R,Res),retract(cell(X,I,_,_)),
                                  assert(cell(X,I,Res,0)),NI is I+1,clearColumnTwoProb(L,Y1,Y2,X,NI).
clearColumnTwoProb(L,Y1,Y2,X,I):- (I =:= Y1;I =:= Y2),NI is I+1,clearColumnTwoProb(L,Y1,Y2,X,NI).
% remove list of probability from cell in selected Square (L is the list to remove,[X1,X2,Y1,Y2] indexes of cells to ignore,[SX,EX,SY,EY] is the boundary of the square )
clearSquareTwoProb(_,_,_,_,_,_,_,SY,EY)      :- SY > EY.
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY):- (SX=:=X1,SY=:=Y1;SX=:=X2,SY=:=Y2),NSX is SX+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,SY,EY).
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY):- SX=<EX,cell(SX,SY,R,_),removeElements(L,R,Res),retract(cell(SX,SY,_,_)),
                                                assert(cell(SX,SY,Res,0)),NSX is SX+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,SY,EY).
clearSquareTwoProb(L,X1,Y1,X2,Y2,SX,EX,SY,EY):- SX>EX,NSX is SX-3,NSY is SY+1,clearSquareTwoProb(L,X1,Y1,X2,Y2,NSX,EX,NSY,EY).
% scan row to check if there any cell contain the same two probabilty of the cell in index [X,Y] and if there any cell reomve this two probability from the other cells in this row
%[X,Y] is the index of the cell to compare with its probability , I is an index
%! this cell should contain just two probability
scanHorizontallyForTwoProbCell(_,_,10).
scanHorizontallyForTwoProbCell(X,Y,I):-I=\=X,cell(X,Y,R,_),cell(I,Y,TR,_),isEqual(R,TR),clearRowTwoProb(R,X,I,Y,1).
scanHorizontallyForTwoProbCell(X,Y,I):-NI is I+1,scanHorizontallyForTwoProbCell(X,Y,NI).
% scan column to check if there any cell contain the same two probabilty of the cell in index [X,Y] and if there any cell reomve this two probability from the other cells in this column
%[X,Y] is the index of the cell to compare with its probability , I is an index
%! this cell should contain just two probability
scanVerticallyForTwoProbCell(_,_,10).
scanVerticallyForTwoProbCell(X,Y,I):- I=\=Y,cell(X,Y,R,_),cell(X,I,TR,_),isEqual(R,TR),clearColumnTwoProb(R,Y,I,X,1).
scanVerticallyForTwoProbCell(X,Y,I):-NI is I+1,scanVerticallyForTwoProbCell(X,Y,NI).
% scan square to check if there any cell contain the same two probabilty of the cell in index [X,Y] and if there any cell reomve this two probability from the other cells in this column
%[X,Y] is the index of the cell to compare with its probability ,[SX,EX,SY,EY] is the boundary of the square
scanSquarlyorTwoProbCell(_,_,_,_,SY,EY):-SY>EY.
scanSquarlyorTwoProbCell(X,Y,SX,EX,SY,EY):-SX=<EX,SX=:=X,SY=:=Y,NSX is SX+1,scanSquarlyorTwoProbCell(X,Y,NSX,EX,SY,EY).
scanSquarlyorTwoProbCell(X,Y,SX,EX,SY,EY):-SX=<EX,cell(X,Y,R,_),cell(SX,SY,TR,_),isEqual(R,TR),TSX is EX-2,TSY is EY-2,
                                           clearSquareTwoProb(R,X,Y,SX,SY,TSX,EX,TSY,EY), NSX is SX+1,scanSquarlyorTwoProbCell(X,Y,NSX,EX,SY,EY).
scanSquarlyorTwoProbCell(X,Y,SX,EX,SY,EY):-SX=<EX,NSX is SX+1,scanSquarlyorTwoProbCell(X,Y,NSX,EX,SY,EY).
scanSquarlyorTwoProbCell(X,Y,SX,EX,SY,EY):-SX>EX,NSX is SX-3,NSY is SY+1,scanSquarlyorTwoProbCell(X,Y,NSX,EX,NSY,EY).
