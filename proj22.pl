% Predicate to plan the shifts, initializing with the list of all employees
plan(plan(UpdatedMorning, UpdatedEvening, UpdatedNight)) :-
    all_employees(AllEmployees),
    assign_employees(morning, AllEmployees, Morning, RemainingAfterMorning),%assign the minimum number to each workstation
    assign_employees(evening, RemainingAfterMorning, Evening, RemainingAfterEvening),
    assign_employees(night, RemainingAfterEvening, Night, RemainingAfterNight),
    fill_shift_workstations(Morning, RemainingAfterNight, UpdatedMorning, NewRemainingAfterMorning, morning),%then assign any leftover employees wherever they fit.
    fill_shift_workstations(Evening, NewRemainingAfterMorning, UpdatedEvening, NewRemainingAfterEvening, evening),
    fill_shift_workstations(Night, NewRemainingAfterEvening, UpdatedNight, _, night).

assign_employees(Shift, AvailableEmployees, Schedule, RemainingEmployees) :-%assign the minimum number of employees to each workstation
    findall((W, Min, Max), (workstation(W, Min, Max), \+ workstation_idle(W,Shift)), Workstations),
    assign_employees_helper(Shift, Workstations, AvailableEmployees, [], Schedule, UsedEmployees),
    subtract(AvailableEmployees, UsedEmployees, RemainingEmployees).

assign_employees_helper(_, [], _, UsedEmployees, [], UsedEmployees).
assign_employees_helper(Shift, [(Workstation, Min, Max) | RestWorkstations], AvailableEmployees, AccUsed, [(Workstation, Emps) | RestSchedule], UsedEmployees) :-
    findall(E, (member(E, AvailableEmployees), \+ member(E, AccUsed), \+ (avoid_shift(E, Shift) ; avoid_workstation(E, Workstation))), EligibleEmployees),
    combination(Min,EligibleEmployees, Emps),
    append(AccUsed, Emps, NewAccUsed),
    assign_employees_helper(Shift, RestWorkstations, AvailableEmployees, NewAccUsed, RestSchedule, UsedEmployees).

% Helper to generate combinations of elements
combination(0, _, []).
combination(N, [X|T], [X|R]) :-
    N > 0,
    N1 is N - 1,
    combination(N1, T, R).
combination(N, [_|T], R) :-
    N > 0,
    combination(N, T, R).

% Subtract second list from first, to handle remaining employees
subtract([], _, []).
subtract([E|Tail], L2, Result) :-
    member(E, L2), !,
    subtract(Tail, L2, Result).
subtract([E|Tail], L2, [E|Result]) :-
    \+ member(E, L2),
    subtract(Tail, L2, Result).

fill_shift_workstations([], Remaining, [], Remaining, _).
fill_shift_workstations([(W, Es) | RestShift], Remaining, [(W, NewEs) | UpdatedRestShift], RemainingAfter, ShiftType) :-%go through each shift and assign the remaining employees
    workstation(W, _, Max),
    length(Es, CurrentCount),
    SpaceAvailable is Max - CurrentCount,  % How many more can be added
    take_up_to(SpaceAvailable, Remaining, W, ShiftType, Candidates, NewRemaining),
    append(Es, Candidates, NewEs),
    fill_shift_workstations(RestShift, NewRemaining, UpdatedRestShift, RemainingAfter, ShiftType).

take_up_to(0, Remaining, _, _, [], Remaining).
take_up_to(N, [Head | Tail], Workstation, ShiftType, [Head | Taken], Remaining) :-
    N > 0,
    \+ avoid_workstation(Head, Workstation),
    \+ avoid_shift(Head, ShiftType),
    N1 is N - 1,
    take_up_to(N1, Tail, Workstation, ShiftType, Taken, Remaining).%add employees until the workstation is full.
take_up_to(N, [Head | Tail], Workstation, ShiftType, Taken, Remaining) :-
    N > 0,
    (avoid_workstation(Head, Workstation) ; avoid_shift(Head, ShiftType)),
    take_up_to(N, Tail, Workstation, ShiftType, Taken, [Head|Remaining]).%if current cannot be assigned here add it to the remaining list
take_up_to(_, [], _, _, [], []).
