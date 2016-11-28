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

conc([], L, L).
conc( [X|L1], L2, [X|L3]) :-
   conc(L1, L2, L3).

initState( [ on(b4, p1), on(b1, b4), on(b3, b1), on(b2, p3),
  clear(b3), clear(b2), clear(p2), clear(p4)]).
goals( [ on(b3,b2), on(b1,b3)]).

  
%przykladowe zapytanie: initState( InitState), goals( Goals), plan(InitState), Goals, Plam, FinalState).
