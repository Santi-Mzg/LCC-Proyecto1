:- module(proylcc,
	[
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids)
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía.
*/

join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [N | Ns],
	RGrids = [RGrid1, RGrid2],
	remove_and_add_new(Grid, NumOfColumns, Path, RGrid1),
	fall_and_generate_news(RGrid1, NumOfColumns, Path, RGrid2).

% Caso base donde ya se recorrió toda la grilla.
remove_and_add_new(Grid, NumOfColumns, Path, RGrid1) :-
	Pos = 0,
	Val = 0,
	remove(Grid, NumOfColumns, Pos, Path, Val, FVal, RG1),
	next_power_of_2(FVal, Res),
	change_element(1, Res, RG1, RGrid1).

remove(Grid, NumOfColumns, Pos, Path, Val, FVal, RGrid1):-
	Grid = [],
	RGrid1 = [],
	FVal is Val.

% Caso recursivo en donde para cada elemento de la grilla se hace 0 si forma parte del Path y se mantiene su valor en el caso contrario.
remove(Grid, NumOfColumns, Pos, Path, Val, FVal, RGrid1):-
	remove_aux(Grid, NumOfColumns, Pos, Path, ValN, RGrid1),
	Grid = [G | Gs],
	RGrid1 = [R | Rs],
	PosSig is Pos+1,
	Sum is Val+ValN,
	remove(Gs, NumOfColumns, PosSig, Path, Sum, FVal, Rs).


% Caso base donde el square en cuestión no forma parte del Path y se mantiene igual.
remove_aux(Grid, NumOfColumns, Pos, Path, Val, RGrid1) :-
	Path = [],
	Val = 0,
	Grid = [G | Gs],
	RGrid1 = [G | _].

% Caso base donde el square en cuestión es el último del Path y dónde debe modificarse su valor luego, no hacerse 0.
remove_aux(Grid, NumOfColumns, Pos, Path, Val, RGrid1) :-
	Path = [[F, C]],
	Pos is F*NumOfColumns + C,
	Grid = [Val | Gs],
	RGrid1 = [1 | _]. % 1 representa la posición donde termina el Path y debe calcularse su nuevo valor.

% Caso base donde el square en cuestión forma parte del Path y se cambia a 0.
remove_aux(Grid, NumOfColumns, Pos, Path, Val, RGrid1) :-
	Path = [[F, C] | Ps], % F=nro Fila y C=nro Columna.
	Ps \= [],
	Pos is F*NumOfColumns + C, % Este cálculo permite determinar si el square del Path es el square en cuestión en la posición Pos de la lista Grid.
	Grid = [Val | _],
	RGrid1 = [0 | _].

% Caso recursivo donde se recorren los squares del Path para ver si alguno coincide con el square en cuestión.
remove_aux(Grid, NumOfColumns, Pos, Path, Val, RGrid1) :-
	Path = [P | Ps],
	remove_aux(Grid, NumOfColumns, Pos, Ps, Val, RGrid1). 
	
next_power_of_2(FVal, Res) :-
	P = 2,
	next_power_of_2_aux(FVal, P, Res).
next_power_of_2_aux(FVal, P, Res) :-
	Res is 2^P,
	Res >= FVal.
next_power_of_2_aux(FVal, P, Res) :-
	PS is P+1,
	next_power_of_2_aux(FVal, PS, Res).

change_element(_, [], []).
change_element(E1, E2, [E1|L], [E2|L]).
change_element(E1, E2, [X|L], [X|LR]) :- E1\=X, change_element(E1, E2, L, LR).

%
%
%
%
%
fall_and_generate_news(Grid, NumOfColumns, Path, RGrid2) :-
	split_grid(Grid, Cols),
	Cols = [C | _Cs],
	length(C, NumOfRows),
	fallOnColumns(Cols, NumOfRows, RCols),
	concatColumns(RCols, RGrid2).


split_grid(Grid, Columns) :-
	Grid = [],
	Columns = [[], [], [], [], []].

split_grid(Grid, Columns) :-
	Grid = [G0, G1, G2, G3, G4 | Gs],
	Columns = [[G0 | C0s], [G1 | C1s], [G2 | C2s], [G3 | C3s], [G4 | C4s]],
	split_grid(Gs, [C0s, C1s, C2s, C3s, C4s]).

fallOnColumns(Cols, NumOfRows, RCols) :-
	Cols = [],
	RCols = [].

fallOnColumns(Cols, NumOfRows, RCols) :-
	Cols = [C | Cs],
	RCols = [RC | RCs],
	findall(X, (member(X, C), X \= 0), Rta),
	addRandoms(Rta, NumOfRows, RC),
	fallOnColumns(Cs, NumOfRows, RCs).

addRandoms(Col, NumOfRows, RCol) :-
	length(Col, Length),
	RandomsToAdd is NumOfRows - Length,
	Options = [2, 4, 8, 16, 32, 64],
	addRandomsAux(Col, RandomsToAdd, Options, RCol).

addRandomsAux(Col, RandomsToAdd, Options, RCol) :-
	RandomsToAdd = 0,
	RCol = Col.

addRandomsAux(Col, RandomsToAdd, Options, RCol) :-
	RandomsToAdd > 0,
	RCol = [R | Rs],
	random_member(R, Options),
	RandomsToAddN is RandomsToAdd - 1,
	addRandomsAux(Col, RandomsToAddN, Options, Rs).

concatColumns(Cols, RGrid) :-
	Cols = [[], [], [], [], []],
	RGrid = [].

concatColumns(Cols, RGrid) :-
	Cols = [[C0 | C0s], [C1 | C1s], [C2 | C2s], [C3 | C3s], [C4 | C4s]],
	RGrid = [C0, C1, C2, C3, C4 | Rs],
	concatColumns([C0s, C1s, C2s, C3s, C4s], Rs).
