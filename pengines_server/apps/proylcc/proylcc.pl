:- module(proylcc,
	[
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids)
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía.


join(Grid, _NumOfColumns, _Path, RGrids):-
	Grid = [N | Ns],	% La implementación actual es simplemente a modo de muestra, y no tiene sentido, debe reepmplazarla
	N2 is N * 2,		% por una implementación válida.
	RGrids = [[0 | Ns], [N2 | Ns]].
*/

/*
join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [N | Ns],
	RGrids = [RGrid1, RGrid1],
	find(Grid, NumOfColumns, Path, Pos, RGrid1).


copiar_resto([], []).

copiar_resto(Grid, RGrid1) :-
	Grid = [N | Ns],
	RGrid1 = [N | Gs],
	copiar_resto(Ns, Gs).

copiar_resto(Grid, RGrid1) :-
	Grid = [N | Ns],
	RGrid1 = [0 | Gs],
	copiar_resto(Ns, Gs).

find(Grid, NumOfColumns, Path, Pos, RGrid1) :-
	Path = [],
	copiar_resto(Grid, RGrid1).

find(Grid, NumOfColumns, Path, Pos, RGrid1) :-
	Path = [P | Ps],
	Pos = 0,
	find_each(Grid, NumOfColumns, P, Pos, RGrid1),
	find(Grid, NumOfColumns, Ps, Pos, RGrid1).

% Caso donde RGrid se encuentra la posición a borrar y se la define en 0.
find_each(Grid, NumOfColumns, Square, Pos, RGrid1) :-
	Square = [L, C],
	Pos is L*NumOfColumns + C,
	RGrid1 = [0 | _].
	

% Caso donde se encuentra la posición a borrar ya definida en otro valor y se la redefine en 0.
find_each(Grid, NumOfColumns, Square, Pos, RGrid1) :-
	Square = [L, C],
	Pos is L*NumOfColumns + C,
	RGrid1 = [G | Gs],
	concat([0], Gs, RGrid1).
	

% Caso de recorrido donde RGrid1 no tiene definido ningún elemento y se copian los de Grid.
% O también cuando se recorren posiciones que no fueron alteradas.
find_each(Grid, NumOfColumns, Square, Pos, RGrid1) :-
	Grid = [N | Ns],
	RGrid1 = [N | Gs],
	PosSig is Pos+1,
	find_each(Ns, NumOfColumns, Square, PosSig, Gs).

% Caso de recorrido donde RGrid1 ya está definido parcialmente y se le borró al menos una posición (se hizo 0).
find_each(Grid, NumOfColumns, Square, Pos, RGrid1) :-
	RGrid1 = [G | Gs],
	G=0,
	Grid = [N | Ns],
	PosSig is Pos+1,
	find_each(Ns, NumOfColumns, Square, PosSig, Gs).

*/

% Otra implementacion.

join(Grid, NumOfColumns, Path, RGrids):-
	Grid = [N | Ns],
	RGrids = [RGrid1, RGrid1],
	remove_and_add_new(Grid, NumOfColumns, Path, RGrid1).
	%fall(Grid, NumOfColumns, Pos, RGrid2).

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