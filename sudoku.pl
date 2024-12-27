% generate a new cell or override an old one
generateCell(X,Y,V):-retract(cell(X,Y,_)),random(1,10,V),assert(cell(X,Y,V)).
generateCell(X,Y,V):-random(1,10,V),assert(cell(X,Y,V)).

%check validation of cell in square (X and Y is the position of the cell),
%(MX is the max index of the square horizontally),(SX,SY start of the square horizontally and vertically)
%It will return a true if the valid and false if it's not
validateSquare(X,Y,_,X,Y).
validateSquare(X,Y,MX,SX,SY):- SX=<MX,cell(X,Y,V),cell(SX,SY,V1),V1=\= V, NX is SX+1 ,validateSquare(X,Y,MX,NX,SY).
validateSquare(X,Y,MX,SX,SY):- SX=:=MX+1, NY is SY+1, NX is SX-3,validateSquare(X,Y,MX,NX,NY).

% check validation of column 
% !!!! every cell above this cell should be filled
% return true if it's valid and false if it's not
validateColumn(_,1,_).
validateColumn(X,Y,V):- NY is Y-1,cell(X,NY,V1),V=\=V1,validateColumn(X,NY,V).
% check validation of row 
% !!!! every cell before this cell should be filled
% return true if it's valid and false if it's not
validateRow(1,_,_).
validateRow(X,Y,V):- NX is X-1,cell(NX,Y,V1),V=\=V1,validateRow(NX,Y,V).
     
% fill a square with unique numbers(X,Y are the start position of the square),(MX,MY are the end posistion of the square)
fillSquare(_,Y,_,MY):- Y =:= MY+1.
fillSquare(X,Y,MX,MY):-X=<MX,generateCell(X,Y,V),SX is (MX-2),SY is (MY-2),validateSquare(X,Y,MX,SX,SY),
                       SX is (MX-2),SY is (MY-2), validateRow(SX,Y,V),validateColumn(X,SY,V),NX is X+1,fillSquare(NX,Y,MX,MY).
fillSquare(X,Y,MX,MY):-X=<MX,retract(cell(X,Y,_)), fillSquare(X,Y,MX,MY).
fillSquare(X,Y,MX,MY):-X=:=MX+1, NY is Y+1, NX is X-3,fillSquare(NX,NY,MX,MY).

% fill the hole puzzle(9*9)
fillPuzzle():-fillSquare(1,1,3,3),fillSquare(4,1,6,3),fillSquare(7,1,9,3),
              fillSquare(1,4,3,6),fillSquare(4,4,6,6),fillSquare(7,4,9,6),
              fillSquare(1,7,3,9),fillSquare(4,7,6,9),fillSquare(7,7,9,9).

% fillRadius(10).
% fillRadius(Sq):- NSq is Sq+3,fillRadius(NSq),fillSquare(Sq,Sq,Sq+2,Sq+2).
