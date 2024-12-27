% merge two list into one
append([],L2,L2).
append([H|T],L2,[H|L3]):-append(T,L2,L3).
% remove element from list (v is the element,L should be empty in the call, [H|T] is the list that we want to remove the element from ,F the list after the delete)
removeElement(V,L,[H|T] ,F):-H =\= V ,append(L,[H],NL), removeElement(V,NL,T,F).
removeElement(_,L,[_|T],F):- append(L,T,F).
% fill the cell with all probability(1 -> 9) (X,Y the position of the cell,N is an index should be one , L is list of all probability)
fillProb(X,Y,10,L):-assert(cell(X,Y,L,0)).
fillProb(X,Y,N,L) :- append(L,[N],R), NN is N+1, fillProb(X,Y,NN,R).
% fill all cells with all probability (1->9),(X,Y the first cell position)
fillPuzzleProb(_,10).
fillPuzzleProb(X,Y):-cell(X,Y,_,1),NX is X+1, fillPuzzleProb(NX,Y).
fillPuzzleProb(X,Y):- X<10, fillProb(X,Y,1,[]),NX is X+1, fillPuzzleProb(NX,Y).
fillPuzzleProb(_,Y):- NY is Y+1, fillPuzzleProb(1,NY).

% reomve V from probability list in the selected row (V is the value want to remove,[X,Y] posistion of the cell to ignore,I is an index to start from)
clearRowProb(_,_,_,10).
clearRowProb(V,X,Y,I):- I =\=X,cell(I,Y,R,_),removeElement(V,[],R,Res),retract(cell(I,Y,_,_)),assert(cell(I,Y,Res,0)),NI is I+1,clearRowProb(V,X,Y,NI).
clearRowProb(V,X,Y,I):- I =:= X,NI is I+1,clearRowProb(V,X,Y,NI).

% reomve V from probability list in the selected column (V is the value want to remove,[X,Y] posistion of the cell to ignore,I is an index to start from)
clearColumnProb(_,_,_,10).
clearColumnProb(V,X,Y,I):- I =\=Y,cell(X,I,R,_),removeElement(V,[],R,Res),retract(cell(X,I,_,_)),assert(cell(X,I,Res,0)),NI is I+1,clearColumnProb(V,X,Y,NI).
clearColumnProb(V,X,Y,I):- I =:=Y,NI is I+1,clearColumnProb(V,X,Y,NI).

% reomve V from probability list in the selected Square (V is the value want to remove,[X,Y] posistion of the cell to ignore,
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

