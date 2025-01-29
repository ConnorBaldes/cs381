% Connor Baldes HW7

/* course(course_number, course_name, credits) */

course(cs101,python, 2).
course(mth210, calculusI, 5).
course(cs120, web_design, 3).
course(cs200, data_structures, 4).
course(cs210, algorithms, 4).
course(wrt101, basic_writing, 3).

/* section(CRN, course_number) */

section(1522,cs101).
section(1122,cs101).
section(2322,mth210).
section(2421,cs120).
section(8522,mth210).
section(1621,cs200).
section(7822,mth210).
section(2822,cs210).
section(3522,wrt101).

/* place( CRN, building, time) */

place(1522,owen102,10).
place(1122,dear118,11).
place(2322,with210,11).
place(2421,cov216,15).
place(8522,kec1001,13).
place(1621,cov216,14).
place(7822,kec1001,14).
place(2822,owen102,13).
place(3522,with210,15).

/* enroll(sid, CRN) */

enroll(122,1522).
enroll(122,8522).
enroll(150,1522).
enroll(150,2421).
enroll(212,7822).
enroll(300,2822).
enroll(300,8522).
enroll(310,3522).
enroll(310,8522).
enroll(310,1621).
enroll(175,2822).
enroll(175,7822).
enroll(175,3522).
enroll(410,1621).
enroll(410,7822).
enroll(113,3522).

/* student(sid, student_name, major) */

student(122, mary, cs).
student(150, john, math).
student(212, jim, ece).
student(300, lee, cs).
student(310, pat, cs).
student(175, amy, math).
student(410, john, cs).
student(113, zoe, ece).

% Problem 1a +
schedule(SID, CourseName, Building, Time) :-
enroll(SID, CRN),
section(CRN, CourseNumber),
course(CourseNumber, CourseName, _),
place(CRN, Building, Time).

% Problem 1b +
schedule(SID, StudentName, CourseName) :-
student(SID, StudentName, _),
enroll(SID, CRN),
section(CRN, CourseNumber),
course(CourseNumber, CourseName, _).

% Problem 1c +
offer(CourseNumber, CourseName, CRN, Time) :-
course(CourseNumber, CourseName, _),
section(CRN, CourseNumber),
place(CRN, _, Time).

% Problem 1d +/-
conflict(SID, CRN1, CRN2) :-
enroll(SID, CRN1),
enroll(SID, CRN2),
CRN1 = CRN2,
place(CRN1, _, Time),
place(CRN2, _, Time).

% Problem 1e +/-
meet(SID1, SID2) :-
enroll(SID1, CRN1),
enroll(SID2, CRN2),
(
CRN1 = CRN2;
(
place(CRN1, Building, Time1),
place(CRN2, Building, Time2),
abs(Time1 - Time2) =:= 1
)
).

% Problem 1f +
roster(CRN, StudentName) :-
enroll(SID, CRN),
student(SID, StudentName, _).

% Problem 1g +
highCredits(CourseName) :-
course(_, CourseName, Credits),
Credits >= 4.

%Problem 2a +
rdup([], []).                               
rdup([X], [X]).                             
rdup([X, X | T], M) :-                     
    rdup([X | T], M).                       
rdup([X, Y | T], [X | M]) :-                
    X \= Y,                                
    rdup([Y | T], M).                      

%Problem 2b +
flat([], []).                               
flat([H|T], F) :-                           
    flat(H, FH),                            
    flat(T, FT),                            
    append(FH, FT, F).                      
flat(X, [X]) :-                             
    X \= [],                                
    \+ is_list(X).                          

% Problem 2c +/-
project(Positions, Elements, Result) :-
    project_helper(Positions, Elements, 1, Result).

project_helper([], _, _, []).
project_helper([P|Ps], [E|Es], Index, [E|Rs]) :-
    P =:= Index,
    NextIndex is Index + 1,
    project_helper(Ps, Es, NextIndex, Rs).
project_helper(Positions, [_|Es], Index, Result) :-
    NextIndex is Index + 1,
    project_helper(Positions, Es, NextIndex, Result).

