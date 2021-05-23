solution(Solutions) :-  	%% after you get all the Solutions then to write it to a file  
  		open('A2poker1out100000.txt', append, OutStream),
  		write(OutStream, Solutions),
		close(OutStream).


play(N) :-
	print('In Play'), nl,
	F = [0,0,0,0,0,0,0,0,0,0], print(F), nl,
	loop(N, F).

percentage(Sum, P, []):- 
	print('Probability : '), print(P),
	solution('Probability : '), solution(P),  solution('\n').
	
percentage(Sum, P, [H|T]):- 
	Percent is H * 100 / Sum, 
	append(P, [Percent], P1),
	percentage(Sum, P1, T).

list_sum([], 0).
list_sum([H| T], Sum) :-
    list_sum(T, Sum1),
    Sum is H + Sum1.

loop(0, F):- 
	print('Frequency : '),print(F), nl,
	solution('Frequency : '), solution(F),  solution('\n'),
	list_sum(F, Sum),
	P = [],
	percentage(Sum, P, F).
	
loop(N, F) :- 
	solution('Loop : '), solution(N), solution('\n'), solution('\n'),
	N>0, 
	N1 is N-1,
	play2(F,F1), 
	loop(N1,F1).


play2(F,F1) :- 
	Deck =[[ace,heart],[ace,diamond],[ace,club],[ace,spade],
	[2,heart],[2,diamond],[2,club],[2,spade],
	[3,heart],[3,diamond],[3,club],[3,spade],
	[4,heart],[4,diamond],[4,club],[4,spade],
	[5,heart],[5,diamond],[5,club],[5,spade],
	[6,heart],[6,diamond],[6,club],[6,spade],
	[7,heart],[7,diamond],[7,club],[7,spade],
	[8,heart],[8,diamond],[8,club],[8,spade],
	[9,heart],[9,diamond],[9,club],[9,spade],
	[10,heart],[10,diamond],[10,club],[10,spade],
	[jack,heart],[jack,diamond],[jack,club],[jack,spade],
	[queen,heart],[queen,diamond],[queen,club],[queen,spade],
	[king,heart],[king,diamond],[king,club],[king,spade]],
	random_select(Card1,Deck,Deck1), random_select(Card2,Deck1,Deck2), random_select(Card3,Deck2,Deck3), random_select(Card4,Deck3,Deck4), random_select(Card5,Deck4,Deck5),
	H1 = [Card1, Card2, Card3, Card4, Card5],
	print('Cards of player1 : '),print(H1), nl,
	solution('Cards of player1 : '), solution(H1), solution('\n'),
	sort_hand(H1, Sorted_Hand1), print('Sorted Cards of player1 : '),print(Sorted_Hand1), nl, nl,
	solution('Sorted Cards of player1 : '), solution(Sorted_Hand1),  solution('\n'),
	determine_hand(Sorted_Hand1,  X1), print('Determine Hand of H1 : '), print(X1), nl, nl, nl,
	solution('Determine Hand of H1 : '), solution(X1),  solution('\n'),  solution('\n'), solution('\n'),
	count(X1, F, F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Count Frequency	

count(royal_flush, F, F1):- replace_Element(F, 0, F1).
count(straight_flush, F, F1):- replace_Element(F, 1, F1).
count(four_of_a_kind, F, F1):- replace_Element(F, 2, F1).
count(full_house, F, F1):- replace_Element(F, 3, F1).
count(flush, F, F1):- replace_Element(F, 4, F1).
count(straight, F, F1):- replace_Element(F, 5, F1).
count(three_of_a_kind, F, F1):- replace_Element(F, 6, F1).
count(two_pair, F, F1):- replace_Element(F, 7, F1).
count(pair, F, F1):- replace_Element(F, 8, F1).
count(high_card, F, F1):- replace_Element(F, 9, F1).

replace_Element(F, I, F1):- nth0(I, F, A), add1(A, A1), replace(F, I, A1, F1).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


add1(A, A1):- A1 is A + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hand determination
determine_hand([[10,X],[jack,X],[queen,X],[king,X],[ace,X]], royal_flush).

determine_hand([[A,X],[B,X],[C,X],[D,X],[E,X]], straight_flush) :-
  successor(E,D), successor(D,C), successor(C,B), successor(B,A).

determine_hand([[C,_],[A,_],[A,_],[A,_],[B,_]], four_of_a_kind) :-
  C = A ; B = A.

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], full_house) :-
  A = B, D = E, (C = D ; C = B).

determine_hand([[_,X],[_,X],[_,X],[_,X],[_,X]], flush).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], straight) :-
  successor(E,D), successor(D,C), successor(C,B), successor(B,A).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], three_of_a_kind) :-
  (A = B, B = C); (B = C, C = D); (C = D, D = E).

determine_hand([[A,_],[A,_],[B,_],[B,_],[_,_]], two_pair).
determine_hand([[_,_],[A,_],[A,_],[B,_],[B,_]], two_pair).
determine_hand([[A,_],[A,_],[_,_],[B,_],[B,_]], two_pair).

determine_hand([[A,_],[B,_],[C,_],[D,_],[E,_]], pair) :-
  A = B; B = C; C = D; D = E.

determine_hand(_,high_card).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Hand sorting (for easier pattern matching).

sort_hand([], []).
sort_hand([H|T], Sorted) :-
  filter_by_high_card(H,T,Lower,Higher),
  sort_hand(Lower,SortedLower),
  sort_hand(Higher,SortedHigher),
  append(SortedLower, [H|SortedHigher], Sorted).


filter_by_high_card(_, [], [], []).  
filter_by_high_card(Pivot, [H|T], [H|Lower], Higher) :-
  beats(Pivot,H,Z),
  (Z = Pivot ; Z = tie),
  filter_by_high_card(Pivot, T, Lower, Higher).
filter_by_high_card(Pivot, [H|T], Lower, [H|Higher]) :-
  beats(Pivot,H,H),
  filter_by_high_card(Pivot, T, Lower, Higher).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Card and Hand Precedence
beats([V,_],[V,_],tie).
beats([V1,S],[V2,_],[V1,S]) :- value_greater_than(V1,V2).
beats([V1,_],[V2,S],[V2,S]) :- value_greater_than(V2,V1).

beats(X,X,tie).
beats(X,Y,X) :- value_greater_than(X,Y).
beats(X,Y,Y) :- value_greater_than(Y,X).

successor(royal_flush, straight_flush).   successor(straigh_flush, four_of_a_kind).
successor(four_of_a_kind, full_house).    successor(full_house, flush).
successor(flush, straight).               successor(straight, three_of_a_kind).
successor(three_of_a_kind, two_pair).     successor(two_pair, pair).
successor(pair, high_card).

successor(ace,king).     successor(king,queen).   successor(queen,jack).
successor(jack,10).      successor(10,9).         successor(9,8).
successor(8,7).          successor(7,6).          successor(6,5).
successor(5,4).          successor(4,3).          successor(3,2).

value_greater_than(X,Y) :-
  successor(X,P),
  (Y = P;
  value_greater_than(P,Y)).


