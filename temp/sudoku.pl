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
% fill a square with unique numbers(X,Y are the start position of the square),(MX,MY are the end position of the square)
% fillSquare(_,_,MX,MY,_,10):-SX is MX-2,SY is MY-5,NMY is MY-3,clearSquare(SX,MX,SY,NMY),NSX is MX-2,NSY is MY-5,fillSquare(NSX,NSY,MX,NMY,0,0),
                            % NSX is MX-2,NSY is MY-2,clearSquare(NSX,MX,NSY,MY),NSX is MX-2,NSY is MY-2,fillSquare(NSX,NSY,MX,MY,0,0).
% fillSquare(_,_,MX,MY,25,J):-write(" "),write(J),SX is MX -2,SY is MY -2,clearSquare(SX,MX,SY,MY),NSX is MX-2,NSY is MY-2,NJ is J +1,fillSquare(NSX,NSY,MX,MY,0,NJ).
fillSquare(_,_,_,_,25,_):-not(clearPuzzle()),fillPuzzle().
fillSquare(_,Y,_,MY,_,_)  :-Y > MY.
fillSquare(X,Y,MX,MY,_,J) :-X=<MX,generateCell(X,Y,V),SX is (MX-2),SY is (MY-2),validateSquare(X,Y,MX,SX,SY),
                            validateRow(X,Y,V),validateColumn(X,Y,V),NX is X+1,fillSquare(NX,Y,MX,MY,0,J).
fillSquare(X,Y,MX,MY,I,J) :-X=<MX,retract(cell(X,Y,_)),NI is I +1 ,fillSquare(X,Y,MX,MY,NI,J).
fillSquare(X,Y,MX,MY,_,J) :-X>MX, NY is Y+1, NX is MX-2,fillSquare(NX,NY,MX,MY,0,J).
% fill a square with unique numbers(X,Y are the start position of the square),(MX,MY are the end position of the square)
% but this for radius I want him to fill radius Square
fillRadiusSquare(_,Y,_,MY):- Y =:= MY+1.
fillRadiusSquare(X,Y,MX,MY):-X=<MX,generateCell(X,Y,_),SX is (MX-2),SY is (MY-2),validateSquare(X,Y,MX,SX,SY),NX is X+1,fillRadiusSquare(NX,Y,MX,MY).
fillRadiusSquare(X,Y,MX,MY):-X=<MX,retract(cell(X,Y,_)), fillRadiusSquare(X,Y,MX,MY).
fillRadiusSquare(X,Y,MX,MY):-X=:=MX+1, NY is Y+1, NX is X-3,fillRadiusSquare(NX,NY,MX,MY).
% fill just the radius
fillRadius():-fillRadiusSquare(1,1,3,3),fillRadiusSquare(4,4,6,6),fillRadiusSquare(7,7,9,9).
% fill the hole puzzle(9*9)
clearSquare(_,_,SY,EY):-SY>EY.
clearSquare(SX,EX,SY,EY):-SX=<EX,retract(cell(SX,SY,_)),NX is SX +1 ,clearSquare(NX,EX,SY,EY).
clearSquare(SX,EX,SY,EY):-SX=<EX,NX is SX +1 ,clearSquare(NX,EX,SY,EY).
clearSquare(SX,EX,SY,EY):- SX>EX,NY is SY +1,NX is EX -2,clearSquare(NX,EX,NY,EY).
fillPuzzle():-  fillSquare(1,1,3,3,0,0),fillSquare(4,1,6,3,0,0),fillSquare(7,1,9,3,0,0),
                fillSquare(1,4,3,6,0,0),fillSquare(4,4,6,6,0,0),fillSquare(7,4,9,6,0,0),
                fillSquare(1,7,3,9,0,0),fillSquare(4,7,6,9,0,0),fillSquare(7,7,9,9,0,0).
clearPuzzle():-cell(_,_,_),retract(cell(_,_,_)),clearPuzzle().
