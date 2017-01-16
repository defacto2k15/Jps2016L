% Projekt z Języków programowania symbolicznego 2016L
% Tworzą: Bartłomiej Czerwiński oraz
%         Jakub Szymanowski

%   InitState -- stan początkowy
%   Goals -- lista celów
%   Plan -- skonstruowany plan
%   FinalState -- stan końcowy
%   Goal -- cel wybrany z listy celów
%   RestGoals -- pozostałe cele
%   Action -- akcja osiągająca zadany cel
%   CondGoals -- warunki dla akcji, które stają się nowymi celami
%   Conditions -- warunki dla akcji do sprawdzenia w stanie,
%   w którym -- akcja bedzie wykonywana
%   PrePlan -- skonstruowany preplan
%   State1 -- stan pośredni 1, osiągany po wykonaniu preplanu
%   InstAction -- akcja ukonkretniona przed wykonaniem
%   State2 -- stan pośredni 2, osiągany po wykonaniu akcji
%   w stanie -- pośrednim 1
%   PostPlan -- skonstruowany postplan
%   AchievedGoals - cele jak na razie osiągnięte, których nie należy niszczyć



plan(State, Goals, _,  [], State, Level ) :-
   goals_achieved(Goals, State) ,
   writeInLine(Level, ['Cele ', Goals, ' są spełnione już w obecnym stanie, ', State, ' nie ma potrzeby tworzyć planu']).

plan(InitState, Goals, Limit, Plan, FinalState, Level) :-
   Limit > 0,
   writeInLine(Level, ['Wywyołana procedura plan z argumentami: ']),
   writeInLine(Level+2,[ 'Stan początkowy = ',InitState]),
   writeInLine(Level+2,[ 'Goals = ',Goals ]),
   writeInLine(Level+2,[ 'Limit = ', Limit ]),
   writeInLine(Level+2,[ 'Plan = ', Plan]),
   writeInLine(Level+2,[ 'FinalState = ', FinalState]),

   writeInLine(Level+2, ['Wybieramy cel z listy ',Goals]), 
   askUserToChoose( x(XGoal, XRestGoals), choose_goal(XGoal, Goals,XRestGoals, InitState), x(Goal, RestGoals), Level+2),
   writeInLine(Level+2, ['Wybrany cel ', Goal ]),

   achieves( Goal, Action),
   writeInLine(Level+2, ['Aby wykonać cel ',Goal,' Należy wykonać akcję ', Action]),

   requires(Action, CondGoals, Conditions),
   writeInLine(Level+2, ['Wykonanie tej akcji wymaga spełnienia warunku ',Conditions,' oraz celów ',CondGoals]),

   %generate_limit(0, Limit, LimitPre),
    
   writeInLine(Level+2, ['Próba ustalenia preplanu']),
   plan(InitState, CondGoals, 100, PrePlan, State1, Level+4),
   writeInLine(Level+2, ['Wyjściowy preplan: ',PrePlan,' stan po wykonaniu preplanu ',State1]),

   inst_action(Action, Conditions,State1, InstAction),
   writeInLine(Level+2, ['Zainicjowana akcja: ', InstAction]),

   perform_action(State1, InstAction, State2),
   writeInLine(Level+2, ['Po wykonaniu akcji stan jest równy ',State2]),
   
   %   LimitPost is Limit - LimitPre - 1,
   writeInLine(Level+2, ['Próba ustalenia postplanu']),
   plan(State2, RestGoals,  100, PostPlan, FinalState, Level+4 ),
   writeInLine(Level+2, ['Wyjściowy postplan: ',PostPlan,' stan końcowy: ', FinalState]),

   writeInLine(Level+2, ['Sprawdzamy czy w stanie końcowym znajduje się cel',Goal]),
   checkIsSuccessCall( contains( FinalState),Goal, Level),
   writeInLine(Level+2, 'CONTAINS!!!!!'),

   conc(PrePlan, [ InstAction | PostPlan ], Plan) .


% procedura pomocnicza do konkatenacji listy
conc([], L, L).
conc( [X|L1], L2, [X|L3]) :-
   conc(L1, L2, L3).

% procedura pomocnicza contains(Elem, Lista) - sprawdza czy Elem znajduje się w Lista
contains( Lista, Elem) :- conc(_, [Elem|_], Lista).

remove( Elem, InList, OutList ):-
   conc(Start, [Elem|End], InList),
   conc(Start, End, OutList).

  
%przykladowe zapytanie: initState( InitState), goals( Goals), plan(InitState), Goals, Plam, FinalState).

goals_achieved([], _).
goals_achieved([FirstGoal | RestGoals], State) :- 
   goal_achieved( FirstGoal, State),
   goals_achieved(RestGoals, State).

%sprawdzenie czy pojedynczy cel zostal spelniony
goal_achieved( clear(X/Y), State) :- 
   nonvar(Y),
   goal_achieved(Y, State),
   contains( State, clear(X)).
goal_achieved( clear(X), State ) :-  
   is_without_condition(X),
   contains( State, clear(X) ).

goal_achieved( on(X, Y/Z), State) :-
   nonvar(Z), !,
   goal_achieved(Z, State),
   contains(State, on(X,Y)).
goal_achieved( on(X,Y), State) :-
   is_without_condition(Y),
   contains( State, on(X,Y)).

goal_achieved(diff(X, Y/W), State) :- 
   goal_achieved( W, State),
   X \= Y.

is_without_condition(X) :- var(X).
is_without_condition(X) :-  X \= _/_.

choose_goal( Goal, [Goal | RestGoals], RestGoals, InitState ) :- 
   not( goal_achieved(Goal, InitState)).
choose_goal( Goal, [ FirstGoal | RestGoals], [FirstGoal | OutRestGoals], InitState) :-
   choose_goal(Goal, RestGoals, OutRestGoals, InitState).

achieves( on(X,Y), move(X, Z/on(X, Z), Y )). 
achieves( clear(T), move(X/on(X,T), T, _)).

requires( move(X/on(X, Y), Y, Z), [clear(X/on(X, Y))], [clear(Z), diff( Z, X/on(X,Y))]).
requires( move(X, Y/on(X,Y), Z), [ clear(X), clear(Z) ], [on(X,Y)]).

inst_action(move(X,Y,Z), Conditions, State1, move(X1,Y1,Z)):-
   goals_achieved(Conditions, State1),
   remove_condition(X, X1),
   remove_condition(Y, Y1).

remove_condition(X, X) :- X \= _/_ .
remove_condition(X/_, X).

perform_action(State1, move(X, Y, Z), [on(X,Z), clear(Y) | State2]) :-
   remove( on(X,Y), State1, TempState),
   remove( clear(Z), TempState, State2 ).

check_action( move(X,Y,Z), AchievedGoals):-
   not( contains(AchievedGoals, clear(Z))),
   not( contains(AchievedGoals, on(X,Y))).

% procedura pomocnicza generujaca niedeterministycznie wartosci (1-szy arg) z zakresu <0-MAX)
generate_limit(Curr, Max, Curr):-
   Curr < Max.
generate_limit(Curr, Max, Res):-
   Curr < Max,
   NewCurr is Curr + 1,
   generate_limit(NewCurr, Max, Res).


initState( [ on(b4, p1), on(b1, b4), on(b3, b1), on(b2, p3),
    clear(b3), clear(b2), clear(p2), clear(p4)]).
goals( [ on(b3,b2), on(b1,b3)]).

writeInLine( []) :- nl.
writeInLine( [Elem|Rest]) :-
   writeOneElement(Elem), write(' '), writeInLine(Rest).
writeInLine(N, Arr) :- 
  not( var(N)), !, Number is  N*2, tab(Number), writeInLine(Arr).

writeOneElement( Elem ) :-
   var(Elem), !, write('!Niezwiązana!').
writeOneElement( Elem ) :- write(Elem).


askUserToChoose( InputVariables, Function, OutVariable, Level ) :-
   findall( InputVariables, Function, PossibilitiesList),
   include(canBeAssigned(OutVariable), PossibilitiesList, AssignableList),
   assertIsNotEmpty( AssignableList, Level),
   writeInLine( Level, ['Wszystkie  możliwe przypisania do wartości wyjściowej']),
   writePosibilitiesToScreen( AssignableList, 1 , Level),
   askUserToChoose2( AssignableList, Level, SelectedElement),
   OutVariable = SelectedElement.

assertIsNotEmpty([], Level) :- !,
   writeInLine(Level, ['Lista pusta, nie można przypisać, NAWRÓT']), fail.
assertIsNotEmpty(_, _).

askUserToChoose2( [], Level, _) :-!,
   writeInLine(Level, ['Nie można znaleźć poprawnego przypisania. Nawrót']), fail.
askUserToChoose2( [OnlyElem | [] ], Level, OnlyElem) :-!,
   writeInLine(Level, ['Jest tylko jedna możliwość "', OnlyElem, '" więc jest wybrana.']).
askUserToChoose2( PossibilitiesList, Level, SelectedElement) :-
   length(PossibilitiesList, ListLength),
   getUserInput(ListLength, SelectedIndex, Level),
   From0Index is SelectedIndex - 1,
   nth0(From0Index, PossibilitiesList, SelectedElement).
askUserToChoose2( [PosFirst | PosRest], Level, SelElem):-
   write('QWEQWEQWEQWEQWEQWEQWEQWE'), fail.

writePosibilitiesToScreen([], _, _).
writePosibilitiesToScreen( [First|Rest], Index, Level ) :-
   IndexNum is Index,
   writeInLine(Level,['[',IndexNum,'] = ', First]),
   writePosibilitiesToScreen( Rest, Index+1, Level).

getUserInput( ListLength, SelectedElement, Level) :-
   writeInLine(Level, ['Podaj indeks wybranego elementu: ']),
   read(X), assertInputIsGood( ListLength, SelectedElement,X, Level).

assertInputIsGood( ListLength, InElem, InElem, _) :-
   number(InElem),
   InElem >= 1,
   InElem =< ListLength,!.

assertInputIsGood( ListLength, SelectedElement, InElem, Level) :- % gdy user podał zła liczbę
   writeInLine(Level, ['Podany indeks ', InElem, ' Jest zły. Musi być pomiędzy 1 a ', ListLength]),
   getUserInput( ListLength, SelectedElement, Level).

selOne( [Out | _] , Out).
selOne( [_| Rest], Out ) :-
   selOne( Rest, Out).

canBeAssigned(X, Y):-
   not( internalCanBeAssigned(X, Y) ).
internalCanBeAssigned( X, X) :- !, fail.
internalCanBeAssigned(_, _ ). 


checkIsSuccessCall( Func, Arg, _ ) :-
   call(Func, Arg), !.
checkIsSuccessCall(_, _ , Level) :-
   writeInLine( Level, ['Nie można. NAWRÓT']).
