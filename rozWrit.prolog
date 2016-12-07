%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pawel Kubik
% Robert Kluz
%
% IDFS Planner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plan(State, Goals, [], State, L) :-
    write(L), write(' PLAN_SHORT: '), nl,
    write(L), tab(4), write(' State '), write(State), nl,
    write(L), tab(4), write(' Goals '), write(Goals), nl,
    goals_achieved(Goals, State),
    write(L), write(' Goals Achieved!!'), nl,
    write(L), tab(4),write(' Goals '), write(Goals), nl.

    plan(InitState, Goals, Plan, FinalState, L) :-
    write(L), write(' PLAN: '), nl, 
    write(L),tab(4), write(' InitState '), write(InitState), nl,
    write(L),tab(4),write(' Goals '), write(Goals), nl,

    choose_goal(Goal, Goals, RestGoals, InitState),
    write(L), write('CHOOSE_GOAL: '), nl,
    write(L), tab(4),write(' Goal '), write(Goal),  nl,
    write(L), tab(4),write(' RestGoals '), write(RestGoals), nl,

    achieves(Goal, Action),
    write(L), write('ACHIEVES'), nl,
    write(L), tab(4),write(' Action '), write(Action), nl,

    requires(Action, CondGoals, Conditions),
    write(L), write('REQUIRES:'), nl,
    write(L), tab(4),write(' CondGoals '), write(CondGoals), nl,
    write(L), tab(4),write(' Conditions '), write(Conditions), nl,

    plan(InitState, CondGoals, PrePlan, State1, L+1),
    write(L), write('PLAN:'), nl,
    write(L), tab(4),write(' CondGoals '), write(CondGoals), nl,
    write(L), tab(4),write(' PrePlan '), write(PrePlan), nl,
    write(L), tab(4),write(' State1 '), write(State1), nl,

    %goals_achieved(CondGoals, State1), // po co to
    %write(L), write('Achieved '), nl, // bez sensu

    
    inst_action(Action, Conditions, State1, InstAction),
    write(L), write('INST_ACTION:'), nl,
    write(L), tab(4),write(' InstAction '), write(InstAction), nl,

    perform_action(State1, InstAction, State2),
    write(L), write('PREFORM_ACTION'), nl,
    write(L), tab(4),write(' State2 '), write(State2), nl,

    plan(State2, RestGoals, PostPlan, FinalState, L+1),
    write(L), write('PLAN'), nl,
    write(L), tab(4),write(' PostPlan '), write(PostPlan), nl, 
    write(L), tab(4),write(' FinalState '), write(FinalState),  nl,
    conc(PrePlan, [ InstAction | PostPlan ], Plan).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goals_achieved([], _).
goals_achieved([G|Gs], State) :-
    goal_achieved(G, State),
    goals_achieved(Gs, State).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
choose_goal(Goal, Goals, Rest, InitState) :-
    del(Goal, Goals, Rest),
    \+ goal_achieved(Goal, InitState).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
achieves(on(X,Y), move(X, Z/ on(X,Z), Y)). %z
achieves(clear(X), move(Y/ on(Y,X), X, _)). %z
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
requires(move(X, Y, Z), [clear(X), clear(Z)], []) :-
    X \= _/_,
    Y \= _/_.
requires(move(X, Y/on(X,Y), Z), [clear(X), clear(Z)], [on(X,Y)]).
requires(move(X/on(X,Y), Y, Z), [clear(X/on(X,Y))], [clear(Z), diff(Z, X/on(X,Y))]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
inst_action(move(X, Y, Z), Conditions, State, move(X1, Y1, Z1)) :-
    goals_achieved(Conditions, State),
    simplify(X, X1),
    simplify(Y, Y1),
    simplify(Z, Z1).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
perform_action(State1, move(X,Y,Z), [clear(Y), on(X, Z)|State2]) :-
    subtract(State1, [on(X,Y), clear(Z)], State2).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
goal_achieved(clear(X/ Y), State) :-
    nonvar(Y),
    goal_achieved(Y, State),
    member(clear(X), State).
goal_achieved(on(X,Y/ Z), State) :-
    nonvar(Z),
    goal_achieved(Z, State),
    member(on(X,Y), State).
%%
goal_achieved(clear(X),State) :-
    (X \= _/_; var(X)),
    member(clear(X),State).
goal_achieved(on(X,Y),State) :-
    (Y \= _/_; var(Y)),
    member(on(X,Y),State).
%%
goal_achieved(diff(X, Y), _) :-
    (X \= _/_; var(X)),
    (Y \= _/_; var(Y)),
    X \= Y.
goal_achieved(diff(X/Z, Y), State) :-
    X \= Y,
    goal_achieved(Z, State).
goal_achieved(diff(X, Y/Z), State) :-
    X \= Y,
    goal_achieved(Z, State).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
member(X, [X|_]).
member(X, [Y|Ys]) :- X \== Y, member(X,Ys).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
del(X, [X|Y], Y).
del(X, [Y|Xs], [Y|Ys]) :- del(X, Xs, Ys).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conc([], L, L).
conc([H|L1], L2, [H|L3]):-
     conc(L1, L2, L3).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simplify(A, A) :-
    A \= _/_.
simplify(A/_, A) :-
    A \= _/_.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loopinf(N, K) :- K = N.
loopinf(N, K) :- N1 is N + 1, loopinf(N1, K).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%TEST%%%%%%%%%%%%%%%%%%%%%%%%%%%
initState([ on(b4, p1), on(b1, b4),
            on(b3, b1), on(b2, p3),
            clear(b3), clear(b2),
            clear(p2), clear(p4) ]).
 
initGoals([on(b4,b2)]).
