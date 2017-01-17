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


plan(State, Goals, Limit, Plan, FinalState, Level ) :-
   plan(State, Goals,[], Limit, Plan, FinalState, Level).

plan(State, Goals, _, Limit, [], State, Level ) :-
   Limit >= 0, 
   goals_achieved(Goals, State),
   writeInLine(Level, ['Cele ',Goals,' są już spełnione w obecnym stanie ',State,' więc plan jest pusty']).

plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState, Level) :-
   Limit > 0, 
   writeInLine(Level, ['Wywołana procedura plan z argumentami: ']),
   writeInLine(Level+2,[ 'Stan początkowy = ',InitState]),
   writeInLine(Level+2,[ 'Cele = ',Goals ]),
   writeInLine(Level+2,[ 'Osiągnięte cele do ochrony = ', AchievedGoals]),
   writeInLine(Level+2,[ 'Limit rekurencji = ', Limit ]),

   askUserToChoose( XLimitPre, generate_limit(XLimitPre, Limit), LimitPre, Level+2,'Wybierz limit rekurencji dla preplanu' ),

   writeInLine(Level+2, ['Określanie celu z listy celów',Goals]), 
   askUserToChoose( x(XGoal, XRestGoals), choose_goal(XGoal, Goals, XRestGoals, InitState), x(Goal, RestGoals), Level+2, 'Wybierz cel z listy celów'),
   writeInLine(Level+2, ['Wybrany cel ', Goal ]),

   achieves( Goal, Action),
   writeInLine(Level+2, ['Aby wykonać cel ',Goal,' Należy wykonać akcję ', Action]),

   requires(Action, CondGoals, Conditions),
   writeInLine(Level+2, ['Wykonanie tej akcji wymaga spełnienia warunku ',Conditions,' oraz celów ',CondGoals]),

   writeInLine(Level+2, ['Próba ustalenia preplanu']),
   plan(InitState, CondGoals, LimitPre, PrePlan, State1, Level+4),
   writeInLine(Level+2, ['Wyjściowy preplan: ',PrePlan,' stan po wykonaniu preplanu ',State1]),


   writeInLine(Level+2, ['Określamy kształt zainicjowanej akcji: ']),
   askUserToChoose( XInstAction, inst_action(Action, Conditions,State1, XInstAction), InstAction, Level, 'Wybierz formę zainicjowanej akcji'),
   writeInLine(Level+2, ['Zainicjowana akcja: ', InstAction]),

   writeInLine(Level+2, ['Sprawdzamy, czy akcja ',InstAction,' nie niszczy osiągniętych celów ',AchievedGoals]),

   checkIsSuccessCall(check_action(InstAction), AchievedGoals, Level,'Akcja niszczy osiągnięte cele. NAWRÓT'),
   writeInLine(Level+2, ['Akcja ',InstAction,' nie niszczy osiągniętych celów!']),

   perform_action(State1, InstAction, State2),
   writeInLine(Level+2, ['Po wykonaniu akcji stan jest równy ',State2]),

   LimitPost is Limit - LimitPre - 1,

   writeInLine(Level+2, ['Próba ustalenia postplanu']),
   plan(State2, RestGoals,  [Goal | AchievedGoals],  LimitPost, PostPlan, FinalState, Level+4 ),
   writeInLine(Level+2, ['Wyjściowy postplan: ',PostPlan,' stan końcowy: ', FinalState]),

   conc(PrePlan, [ InstAction | PostPlan ], Plan) .


plan(State, Goals, _, Limit, [], State, Level ) :-
   writeInLine(Level, ['Limit rekurencji przekroczony. NAWRÓT']), fail.

% procedura pomocnicza do konkatenacji listy
conc([], L, L).
conc( [X|L1], L2, [X|L3]) :-
   conc(L1, L2, L3).

% procedura pomocnicza contains(Elem, Lista) - sprawdza czy Elem znajduje się w Lista
contains( Lista, Elem) :- conc(_, [Elem|_], Lista).

containsAny( ListToSearchiInto, [Elem|_]) :-
   contains( ListToSearchiInto, Elem).
containsAny( ListToSearchiInto, [_|Rest]) :-
   containsAny(ListToSearchiInto, Rest).

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
   not( containsAny(AchievedGoals, [on(X,Y), clear(Z)])).


% procedura pomocnicza generujaca niedeterministycznie wartosci (1-szy arg) z zakresu <0-MAX)
generate_limit(X, Max):-
   Max > 0,   
   X is Max - 1.

generate_limit(X, Max):-
   Max > 0,
   NewMax is Max - 1,
   generate_limit(X, NewMax).




initState([ on(b4, p1), on(b1, b4),
            on(b3, b1), on(b2, p3),
                        clear(b3), clear(b2),
                                    clear(p2), clear(p4) ]).
                                  
                                 initGoals([on(b4,b2)]).







writeInLine( []) :- nl.
writeInLine( [Elem|Rest]) :-
   writeOneElement(Elem), write(' '), writeInLine(Rest).
writeInLine(N, Arr) :- 
  not( var(N)), !, Number is  N*2, tab(Number), writeInLine(Arr).

writeOneElement( Elem ) :-
   var(Elem), !, write('!Niezwiązana!').
writeOneElement( Elem ) :- write(Elem).


askUserToChoose( InputVariables, Function, OutVariable, Level, SelDescription ) :-
   findall( InputVariables, Function, PossibilitiesList),
   include(canBeAssigned(OutVariable), PossibilitiesList, AssignableList),
   assertIsNotEmpty( AssignableList, Level),
   writeInLine( Level, ['Wszystkie  możliwe przypisania do wartości wyjściowej']),
   askUserToChoose2( AssignableList, Level, SelectedElement, SelDescription),
   OutVariable = SelectedElement.

assertIsNotEmpty([], Level) :- !,
   writeInLine(Level, ['Lista pusta, nie można przypisać, NAWRÓT']), fail.
assertIsNotEmpty(_, _).

askUserToChoose2( [], Level, _, SelDescription) :-!,
   writeInLine(Level, [SelDescription]), 
   writeInLine(Level, ['Nie można znaleźć poprawnego przypisania. Nawrót']), fail.
askUserToChoose2( [OnlyElem | [] ], Level, OnlyElem, SelDescription) :-!,
   writeInLine(Level, [SelDescription]), 
   writeInLine(Level, ['Jest tylko jedna możliwość "', OnlyElem, '" więc jest wybrana.']).
askUserToChoose2( PossibilitiesList, Level, SelectedElement, SelDescription) :-
   length(PossibilitiesList, ListLength),
   writeInLine(Level, [SelDescription]), 
   writePosibilitiesToScreen( PossibilitiesList, 1 , Level),
   getUserInput(ListLength, SelectedIndex, Level),
   From0Index is SelectedIndex - 1,
   nth0(From0Index, PossibilitiesList, XSelectedElement),
   askUserToChoose3( XSelectedElement, PossibilitiesList, SelectedElement, Level, SelDescription).

askUserToChoose3( SelectedElement, _, SelectedElement, _, _). % wersja domyślna, gdy nie ma nawrotów
askUserToChoose3( XSelectedElement, PossibilitiesList, SelectedElement, Level, SelDescription) :-
   writeInLine(Level, ['NAWRÓT']),
   delete( PossibilitiesList, XSelectedElement, ShortenedPossibilitiesList),
   askUserToChoose2( ShortenedPossibilitiesList, Level, SelectedElement, SelDescription). 


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


checkIsSuccessCall( Func, Arg, _, _ ) :-
   call(Func, Arg) .
checkIsSuccessCall(_, _ , Level, NegativeOutput ) :-
   writeInLine( Level, [NegativeOutput]).
