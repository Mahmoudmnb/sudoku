% generate a new cell or override an old one
generateCell(X,Y,[V]):-retract(cell(X,Y,_,_)),random(1,10,V),assert(cell(X,Y,[V],0)).
generateCell(X,Y,[V]):-random(1,10,V),assert(cell(X,Y,[V],0)).
%check validation of cell in square (X and Y is the position of the cell to validate its value),
%(MX is the max index of the square horizontally),(SX,SY start of the square horizontally and vertically)
%It will return a true if the value is valid and false if it's not
validateSquare(X,Y,_,X,Y):-!.
validateSquare(X,Y,MX,SX,SY):- SX=<MX,cell(X,Y,V,_),cell(SX,SY,V1,_),not(isEqual(V,V1)), NX is SX+1,validateSquare(X,Y,MX,NX,SY),!.
validateSquare(X,Y,MX,SX,SY):- SX=:=MX+1, NY is SY+1, NX is SX-3,validateSquare(X,Y,MX,NX,NY),!.
% check validation of cell in column,return true if it's valid and false if it's not[X,Y is the indexes of the cell,I is an index]
validateColumn(_,_,10):-!.
validateColumn(X,Y,I):- I=:=Y,NI is I+1,validateColumn(X,Y,NI),!.
validateColumn(X,Y,I):- not(cell(X,I,_,_)),NI is I+1,validateColumn(X,Y,NI),!.
validateColumn(X,Y,I):- cell(X,I,[H|_],_),cell(X,Y,[H1|_],_),H=\=H1,NI is I+1,validateColumn(X,Y,NI),!.
% check validation of cell in row,return true if it's valid and false if it's not[X,Y is the indexes of the cell,I is an index]
validateRow(_,_,10):-!.
validateRow(X,Y,I):- I=:=X,NI is I+1,validateRow(X,Y,NI),!.
validateRow(X,Y,I):- not(cell(I,Y,_,_)),NI is I+1,validateRow(X,Y,NI),!.
validateRow(X,Y,I):- cell(I,Y,[H|_],_),cell(X,Y,[H1|_],_),H=\=H1,NI is I+1,validateRow(X,Y,NI),!.
% fill a square with unique numbers(X,Y are the start indexes of the square),(MX,MY are the end indexes of the square)
% I is count of times the cell has been build ,J is count of times the square has been build  
% refill the square if the cell has been built 20 times
% refill the the last square or the above square if the square has been built 20 times
fillSquare(_,_,MX,MY,_,10):-MX>3,NSX is MX -5,NSY is MY-2,NMX is MX-3,clearSquare(NSX,NMX,NSY,MY),SX is MX -5,SY is MY-2,NNMX is MX-3,fillSquare(SX,SY,NNMX,MY,0,0),
                            TSX is MX -2,TSY is MY -2,clearSquare(TSX,MX,TSY,MY),TNSX is MX-2,TNSY is MY-2,fillSquare(TNSX,TNSY,MX,MY,0,0).  
fillSquare(_,_,MX,MY,_,10):-MY>3,NSX is MX -2,NSY is MY-5,NMY is MY-3,clearSquare(NSX,MX,NSY,NMY),SX is MX -2,SY is MY-5,NNMY is MY-3,fillSquare(SX,SY,MX,NNMY,0,0),
                            TSX is MX -2,TSY is MY -2,clearSquare(TSX,MX,TSY,MY),TNSX is MX-2,TNSY is MY-2,fillSquare(TNSX,TNSY,MX,MY,0,0).
fillSquare(_,_,MX,MY,20,J):- SX is MX -2,SY is MY -2,clearSquare(SX,MX,SY,MY),NSX is MX-2,NSY is MY-2,NJ is J+1,fillSquare(NSX,NSY,MX,MY,0,NJ).
fillSquare(_,Y,_,MY,_,_) :- Y > MY.
fillSquare(X,Y,MX,MY,_,J):-X=<MX,generateCell(X,Y,_),SX is (MX-2),SY is (MY-2),validateSquare(X,Y,MX,SX,SY),
                            validateRow(X,Y,1),validateColumn(X,Y,1),NX is X+1,fillSquare(NX,Y,MX,MY,0,J).
fillSquare(X,Y,MX,MY,I,J):-X=<MX,retract(cell(X,Y,_,_)),NI is I+1,fillSquare(X,Y,MX,MY,NI,J).
fillSquare(X,Y,MX,MY,_,J):-X>MX, NY is Y+1, NX is MX-2,fillSquare(NX,NY,MX,MY,0,J).
% clear all cells in square
clearSquare(_,_,SY,EY):-SY>EY.
clearSquare(SX,EX,SY,EY):-SX=<EX,retract(cell(SX,SY,_,_)),NX is SX +1 ,clearSquare(NX,EX,SY,EY).
clearSquare(SX,EX,SY,EY):-SX=<EX,NX is SX +1 ,clearSquare(NX,EX,SY,EY).
clearSquare(SX,EX,SY,EY):- SX>EX,NY is SY +1,NX is EX -2,clearSquare(NX,EX,NY,EY).
% build solved puzzle
fillPuzzle():-  fillSquare(1,1,3,3,0,0),fillSquare(4,1,6,3,0,0),fillSquare(7,1,9,3,0,0),
                fillSquare(1,4,3,6,0,0),fillSquare(4,4,6,6,0,0),fillSquare(7,4,9,6,0,0),
                fillSquare(1,7,3,9,0,0),fillSquare(4,7,6,9,0,0),fillSquare(7,7,9,9,0,0).
% reset all cell to default value (make the fourth parameter 0)
resetCell(_,10):-!.
resetCell(X,Y):-X<10,cell(X,Y,R,_),retract(cell(X,Y,_,_)),assert(cell(X,Y,R,0)),NX is X+1,resetCell(NX,Y),!.
resetCell(X,Y):-X<10,NX is X+1,resetCell(NX,Y),!.
resetCell(X,Y):-X>9,NY is Y+1,NX is 1,resetCell(NX,NY),!.
% remove some cells from puzzle according to indexes from lists[first list is the Xs, and the second is Ys]
removeCells([],[]):-!.
removeCells([H1|T1],[H2|T2]):-retract(cell(H1,H2,_,_)),removeCells(T1,T2),!.
removeCells([_|T1],[_|T2]):-removeCells(T1,T2),!.
% remove numbers from the puzzle
clearProb(_,10,_,_).
clearProb(X,Y,LX,LY):-X<10,cell(X,Y,R,_),assert(cell(-1,-1,R,0)),retract(cell(X,Y,_,_)),solvePuzzle(Res),Res=:=1,append([X],LX,NLX),append([Y],LY,NLY),
                      retract(cell(-1,-1,_,_)),resetCell(1,1),removeCells(NLX,NLY),NX is X+1,clearProb(NX,Y,NLX,NLY).
clearProb(X,Y,LX,LY):-X<10,cell(-1,-1,R,_),retract(cell(X,Y,_,_)),assert(cell(X,Y,R,0)),retract(cell(-1,-1,_,_)),resetCell(1,1),removeCells(LX,LY),NX is X+1,clearProb(NX,Y,LX,LY).
clearProb(X,Y,LX,LY):-X<10,cell(-1,-1,R,_),assert(cell(X,Y,R,0)),retract(cell(-1,-1,_,_)),resetCell(1,1),removeCells(LX,LY),NX is X+1,clearProb(NX,Y,LX,LY).
clearProb(X,Y,LX,LY):-X>9,NY is Y+1,NX is 1,clearProb(NX,NY,LX,LY).
% create the puzzle
createPuzzle():-fillPuzzle(),clearProb(1,1,[],[]).