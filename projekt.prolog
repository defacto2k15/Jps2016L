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

plan(State, Goals, [], State) :-
   goals_achieved(Goals, State) .

plan(InitState, Goals, Plan, FinalState) :-
   choose_goal(Goal, Goals, RestGoals, InitState),
   achieves( Goal, Action),
   requires(Action, CondGoals, Conditions),
   plan(InitState, CondGoals, PrePlan, State1),
   inst_action(Action, Conditions,State1, InstAction),
   perform_action(State1, InstAction, State2),
   plan(State2, RestGoals, PostPlan, FinalState),
   conc(PrePlan, [ InstAction | PostPlan ], Plan) .

% procedura pomocnicza do konkatenacji listy
conc([], L, L).
conc( [X|L1], L2, [X|L3]) :-
   conc(L1, L2, L3).

% procedura pomocnicza contains(Elem, Lista) - sprawdza czy Elem znajduje się w Lista
contains( Lista, Elem) :- conc(_, [Elem|_], Lista).

initState( [ on(b4, p1), on(b1, b4), on(b3, b1), on(b2, p3),
  clear(b3), clear(b2), clear(p2), clear(p4)]).
goals( [ on(b3,b2), on(b1,b3)]).

  
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
goal_achieved( on(X/Y,Z), State) :-
   at_least_one_non_var(X, Y), !,
   goal_achieved(Y, State),
   contains(State, on(X,Z)).

goal_achieved(diff(X/Z, Y/W), State) :- 
   at_least_one_non_var(X,Z),
   at_least_one_non_var(Y,W), !,
   goal_achieved( Z, State), 
   goal_achieved( W, State),
   X \= Y.

goal_achieved(diff(X, Y/W), State) :- 
   at_least_one_non_var(Y,W), !,
   goal_achieved( W, State),
   X \= Y.

goal_achieved(diff(X/Z, Y), State) :-  
   at_least_one_non_var(X,Z), !,
   goal_achieved( Z, State), 
   X \= Y.

goal_achieved( diff(X,Y), _) :-
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
   goals_achieved(Conditions, State1).
   remove_condition(X, X1),
   remove_condition(Y, Y1).

perform_action(State1, move(X, Y, Z), [on(X,Z), clear(Y) | State2] ) :-
   remove( on(X,Y), State1, TempState),
   remove( clear(Z), TempState, State2 ).

remove_condition(X, X) :- X \= _\_.
remove_condition(X/Y, X).

