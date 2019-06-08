%
%
%

not(X) :- X, !, fail.
not(_).
reverse([],Z,Z).
reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).
% haversine formula following provided perl code
haversine(Lat1, Long1, Lat2, Long2, Distance) :-
    Dlon is Long2 - Long1,
    Dlat is Lat2 - Lat1,
    TmpA is sin(Dlat / 2) ** 2 + cos(Lat1) * cos(Lat2) 
    * sin(Dlon / 2) ** 2,
    UnitDistance is 2 * atan2(sqrt(TmpA), sqrt(1-TmpA)),
    Distance is UnitDistance * 3961.

   
to_upper(Lower, Upper) :-
   atom_chars(Lower, Lowerlist),
   maplist(lower_upper, Lowerlist, Upperlist),
   atom_chars(Upper, Upperlist).

floatToHr(Hours, Hr, Min):-
   Hr is floor(Hours),
   Decimal is Hours - Hr,
   Min is floor(Decimal*60).

hrMinToHr(time(Hours, Mins), HoursFloat) :-
    SubHours is Mins / 60,
    HoursFloat is Hours + SubHours.

degToRadian(degmin(Degrees,Minutes), Radians ) :- 
    Degs is Degrees + Minutes / 60,
    Radians is Degs * pi / 180.

distanceBetween(Depart, Arrive, DistanceBetween) :-
    airport(Depart, _, Lat1, Long1),
    airport(Arrive, _, Lat2, Long2),
    degToRadian(Lat1, Rlat1),
    degToRadian(Long1, Rlong1),
    degToRadian(Lat2, Rlat2),
    degToRadian(Long2, Rlong2),
    haversine(Rlat1, Rlong1, Rlat2, Rlong2, DistanceBetween).

timeTaken(Depart, Arrive, TimeTaken) :-
    distanceBetween(Depart, Arrive, Distance),
    TimeTaken is Distance / 500.

depart([flight(D,A, time(H, M))|_]):-
    write('depart  '),
    to_upper(D, Code),
    write(Code), write('  '),
    airport(D, City, _, _),
    write(City),
    (H < 10 -> write('0'), write(H);
    write(H)),
    %formatZeroHr(H),
    %write(H),
    write(':'),
    (M < 10 -> write('0'), write(M);
    write(M)).

arrive([flight(D, A, time(H, M))|Next]):-
    write('arrive  '),
    to_upper(A, Code),
    write(Code), write('  '),
    airport(A, City, _, _),
    write(City),
    timeTaken(D, A, Time),
    hrMinToHr(time(H,M), Hrs),
    NewTimee is Time + Hrs,
    floatToHr(NewTimee, Hr, Min),
    (Hr < 10 -> write('0'), write(Hr);
    write(Hr)),
    write(':'),
    (Min < 10 -> write('0'), write(Min);
    write(Min)).

printpath([]).

printpath([flight(D, A, time(H,M))|[]]):-
    depart([flight(D, A, time(H,M))|_]),nl,
    arrive([flight(D, A, time(H,M))|_]).

printpath([flight(D, A, time(H,M))|Next]):-
    %write('printpath'), nl,
    depart([flight(D, A, time(H,M))|Next]), nl,
    arrive([flight(D, A, time(H,M))|Next]), nl,
    %write('printnext'),nl,
    printpath(Next).

findpath(Start, End) :-
    flight(Start, Next, DepartTime),
    timeTaken(Start, Next, TimeTaken),
    hrMinToHr(DepartTime, DepartTimeDecimal),
    ArrivalTime is TimeTaken + DepartTimeDecimal,

    findpath(Next, End, [flight(Start, Next, DepartTime)], [Start, Next], ArrivalTime).
%hrMinToHr(time(Hours, Mins), HoursFloat) :-

findpath(End, End, Flown, _, _) :-
    reverse(Flown, Inorder, []),
    printpath(Inorder).

findpath(Start, End, Flown, Visited, CurrentTime) :- 
    flight(Start, Next, DepartTime),
    timeTaken(Start, Next, TimeTaken),
    hrMinToHr(DepartTime, DepartTimeDecimal),
    timeValid(CurrentTime, DepartTimeDecimal, TimeTaken),
    % ListForm = [Start, Next, DepartTime],

    not(member(Next, Visited)),
  %  findpath(Next, End, [[Start, Next, DepartTime] | Flown]).
    %Updated = append()
    NewTime is DepartTimeDecimal + TimeTaken,
    findpath(Next, End, [flight(Start, Next, DepartTime)| Flown], [Next | Visited], NewTime).
timeValid(Current, Depart, Duration) :-
   
    
    RealTime is Current + 0.5,
    RealTime =< Depart,
    Arrive is Depart + Duration,
    Arrive =< 24.

%fly(Depart, Depart) :-
    write('cant fly to the same airport').

fly(Depart, Arrive) :-
    findpath(Depart, Arrive).


    







/* plan 
    findpath needs to take into account:
        depart time needs to be at least 30 minutes after preceding arrival
        cannot exceed 24 hours
    

*/
/*
 
* Airport Database.
* For each airport:
* - three-letter airport code
* - name of city
* - north latitude: degrees and minutes
* - west longitude: degrees and minutes
* North latitudes and West longitudes are in degrees, minutes.
*/




/*
 sub haversine_distance ($$$$) {
   # Latitude1, longitude1 in radians.
   # Latitude2, longitude2 in radians.
   my ($lat1, $lon1, $lat2, $lon2) = @_;
   my $dlon = $lon2 - $lon1;
   my $dlat = $lat2 - $lat1;
   my $tmpa = (sin ($dlat / 2)) ** 2
            + cos ($lat1) * cos ($lat2) * (sin ($dlon / 2)) ** 2;
   my $unit_distance = 2 * atan2 (sqrt ($tmpa), sqrt (1 - $tmpa));
   my $distance_miles = $EARTH_RADIUS_MILES * $unit_distance;
   return $distance_miles;
}






 */
