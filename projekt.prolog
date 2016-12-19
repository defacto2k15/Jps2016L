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


plan(State, Goals, Limit, Plan, FinalState ) :-
   plan(State, Goals,[], Limit, Plan, FinalState).

plan(State, Goals, _, _, [], State ) :-
   goals_achieved(Goals, State) .

plan(InitState, Goals, AchievedGoals, Limit, Plan, FinalState) :-
   generateLimit(LimitPre, Limit),
   choose_goal(Goal, Goals, RestGoals, InitState),
   achieves( Goal, Action),
   requires(Action, CondGoals, Conditions),
   plan(InitState, CondGoals, AchievedGoals, LimitPre, PrePlan, State1),
   inst_action(Action, Conditions,State1, InstAction),
   check_action(InstAction, AchievedGoals),
   perform_action(State1, InstAction, State2),
   LimitPost is Limit - LimitPre - 1,
   plan(State2, RestGoals, [Goal | AchievedGoals], LimitPost, PostPlan, FinalState  ),
   conc(PrePlan, [ InstAction | PostPlan ], Plan) .


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

initState( [ on(b4, p1), on(b1, b4), on(b3, b1), on(b2, p3),
  clear(b3), clear(b2), clear(p2), clear(p4)]).
goals( [ on(b3,b2), on(b1,b3)]).

gList( [ [on(b3, b2), on(b2, b1)],
[clear(p1), clear(p2), clear(p2)],
[clear(b1), clear(b2), clear(b3), clear(b4)],
[on(b1,b4), on(b2,b3), clear(b2)],
[clear(b1), clear(p1)],
[on(b1,b2), clear(p3), clear(p4)]]).
testX( [Case | OtherCases] ) :-
   initState(InitState),
   plan(InitState, Case, 10, _, _),
   write('PLAN'),nl,
   testX(OtherCases).
testX([]).
  
%przykladowe zapytanie: initState( InitState), goals( Goals), plan(InitState), Goals, Plam, FinalState).

goals_achieved([], _).
goals_achieved([FirstGoal | RestGoals], State) :- 
   goal_achieved( FirstGoal, State),
   goals_achieved(RestGoals, State).

%sprawdzenie czy pojedynczy cel zostal spelniony
goal_achieved( clear(X), State ) :-  
   contains( State, clear(X) ).
goal_achieved( clear(X/Y), State) :- 
   at_least_one_non_var(X,Y),
   goal_achieved(Y, State),
   contains( State, clear(X)).

goal_achieved( on(X,Y), State) :-
   contains( State, on(X,Y)).
goal_achieved( on(X, Y/Z), State) :-
   at_least_one_non_var(Y, Z), !,
   goal_achieved(Z, State),
   contains(State, on(X,Y)).

goal_achieved(diff(X, Y/W), State) :- 
   at_least_one_non_var(Y,W), !,
   goal_achieved( W, State),
   X \= Y.


%sprawdzenie czy przynajmniej jeden z dwóch argumentów został zainicjowany
at_least_one_non_var(X, _) :- 
   nonvar(X).
at_least_one_non_var(_, Y) :- 
   nonvar(Y).

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

remove( Elem, InList, OutList ):-
   conc(Start, [Elem|End], InList),
   conc(Start, End, OutList).

% procedura pomocnicza generujaca niedeterministycznie wartosci (1-szy arg) z zakresu <0-MAX)
generateLimit(X, Max):-
   Max > 0,   
   X is Max - 1.
generateLimit(X, Max):-
   Max > 0,
   NewMax is Max - 1,
   generateLimit(X, NewMax).
