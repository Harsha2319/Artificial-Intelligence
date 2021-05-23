solution(Solutions) :-  	%% after you get all the Solutions then to write it to a file  
  		open('A2poker2out10.txt', append, OutStream),
  		write(OutStream, Solutions),
		close(OutStream).


play(N) :-
	print('In Play'), nl,
	F = [0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,0],
	W = [0,0,0,0,0,0,0,0,0,0],
	loop(N, F, W).
	
percentage(Sum, P, []):- 
	print('In percentage base case : '), print(P),
	solution('Probability : '), solution(P),  solution('\n').
	
percentage(Sum, P, [H|T]):- 
	Percent is H * 100 / Sum, 
	append(P, [Percent], P1),
	percentage(Sum, P1, T).

list_sum([], 0).
list_sum([H| T], Sum) :-
    list_sum(T, Sum1),
    Sum is H + Sum1.

loop(0, F, W):- 
	print('In Base case loop'), nl, print(W), nl, print(F),
	solution('Player1 win Frequency : '), solution(W),  solution('\n'), solution('\n'),
	solution('Player1 vs Player2 : '), solution(F),  solution('\n'), solution('\n'),
	list_sum(W, Sum),
	print(Sum), nl, nl,
	P = [],
	percentage(Sum, P, W).
	
loop(N, F, W) :- 
	solution('Loop : '), solution(N), solution('\n'), solution('\n'),
	N>0, 
	N1 is N-1,
	play2(F, W, W1, F1), 
	loop(N1, F1, W1).


play2(F, W, W1, F1) :- 
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
	random_select(Card6,Deck5,Deck6), random_select(Card7,Deck6,Deck7), random_select(Card8,Deck7,Deck8), random_select(Card9,Deck8,Deck9), random_select(Card10,Deck9,Deck10),
	H1 = [Card1, Card2, Card3, Card4, Card5],
	H2 = [Card6, Card7, Card8, Card9, Card10],
	print(H1), nl, print(H2), nl,
	solution('Cards of player1 : '), solution(H1), solution('\n'),
	solution('Cards of player2 : '), solution(H2), solution('\n'), solution('\n'),
	sort_hand(H1, Sorted_Hand1), print(Sorted_Hand1), nl,
	sort_hand(H2, Sorted_Hand2), print(Sorted_Hand2), nl, nl,
	solution('Sorted Cards of player1 : '), solution(Sorted_Hand1),  solution('\n'),
	solution('Sorted Cards of player2 : '), solution(Sorted_Hand2),  solution('\n'), solution('\n'),
	determine_hand(Sorted_Hand1,  X1), print('Determine Hand of H1 : '), print(X1), nl,
	determine_hand(Sorted_Hand2,  X2), print('Determine Hand of H2 : '), print(X2), nl, nl, nl,
	solution('Determine Hand of H1 : '), solution(X1),  solution('\n'), 
	solution('Determine Hand of H2 : '), solution(X1),  solution('\n'),  solution('\n'), 
	count(X1, X2, F, F1), 
	winner(H1,H2, Winner),!,
	nl, print('win='), print(Winner), nl, nl,
	solution('Winner : '), solution(Winner), solution('\n'),
	count_win(Winner, H1, X1, W, W1),
	print('W1 : '), print(W1), nl, nl, nl,
	solution('Win Frequence : '), solution(W1), solution('\n'), solution('\n'), solution('\n').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Count Frequency	- player1 win

count_win(Winner, H1, royal_flush, W, W1):- same(Winner, H1) -> replace_Element(W, 0, W1); W1 = W.
count_win(Winner, H1, straight_flush, W, W1):- same(Winner, H1) -> replace_Element(W, 1, W1); W1 = W.
count_win(Winner, H1, four_of_a_kind, W, W1):- same(Winner, H1) -> replace_Element(W, 2, W1); W1 = W.
count_win(Winner, H1, full_house, W, W1):- same(Winner, H1) -> replace_Element(W, 3, W1); W1 = W.
count_win(Winner, H1, flush, W, W1):- same(Winner, H1) -> replace_Element(W, 4, W1); W1 = W.
count_win(Winner, H1, straight, W, W1):- same(Winner, H1) -> replace_Element(W, 5, W1); W1 = W.
count_win(Winner, H1, three_of_a_kind, W, W1):- same(Winner, H1) -> replace_Element(W, 6, W1); W1 = W.
count_win(Winner, H1, two_pair, W, W1):- same(Winner, H1) -> replace_Element(W, 7, W1); W1 = W.
count_win(Winner, H1, pair, W, W1):- same(Winner, H1) -> replace_Element(W, 8, W1); W1 = W.
count_win(Winner, H1, high_card, W, W1):- same(Winner, H1) -> replace_Element(W, 9, W1); W1 = W.

same([ ], [ ]).   
same([H1|R1], [H2|R2]):-
    H1 == H2,
    same(R1, R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Count Frequency	- player1 vs player2

%% player1 is royal_flush

count(royal_flush, royal_flush, F, F1):- replace_Element(F, 0, F1).
count(royal_flush, straight_flush, F, F1):- replace_Element(F, 1, F1).
count(royal_flush, four_of_a_kind, F, F1):- replace_Element(F, 2, F1).
count(royal_flush, full_house, F, F1):- replace_Element(F, 3, F1).
count(royal_flush, flush, F, F1):- replace_Element(F, 4, F1).
count(royal_flush, straight, F, F1):- replace_Element(F, 5, F1).
count(royal_flush, three_of_a_kind, F, F1):- replace_Element(F, 6, F1).
count(royal_flush, two_pair, F, F1):- replace_Element(F, 7, F1).
count(royal_flush, pair, F, F1):- replace_Element(F, 8, F1).
count(royal_flush, high_card, F, F1):- replace_Element(F, 9, F1).


%% player1 is straight_flush

count(straight_flush, royal_flush, F, F1):- replace_Element(F, 10, F1).
count(straight_flush, straight_flush, F, F1):- replace_Element(F, 11, F1).
count(straight_flush, four_of_a_kind, F, F1):- replace_Element(F, 12, F1).
count(straight_flush, full_house, F, F1):- replace_Element(F, 13, F1).
count(straight_flush, flush, F, F1):- replace_Element(F, 14, F1).
count(straight_flush, straight, F, F1):- replace_Element(F, 15, F1).
count(straight_flush, three_of_a_kind, F, F1):- replace_Element(F, 16, F1).
count(straight_flush, two_pair, F, F1):- replace_Element(F, 17, F1).
count(straight_flush, pair, F, F1):- replace_Element(F, 18, F1).
count(straight_flush, high_card, F, F1):- replace_Element(F, 19, F1).


%% player1 is four_of_a_kind

count(four_of_a_kind, royal_flush, F, F1):- replace_Element(F, 20, F1).
count(four_of_a_kind, straight_flush, F, F1):- replace_Element(F, 21, F1).
count(four_of_a_kind, four_of_a_kind, F, F1):- replace_Element(F, 22, F1).
count(four_of_a_kind, full_house, F, F1):- replace_Element(F, 23, F1).
count(four_of_a_kind, flush, F, F1):- replace_Element(F, 24, F1).
count(four_of_a_kind, straight, F, F1):- replace_Element(F, 25, F1).
count(four_of_a_kind, three_of_a_kind, F, F1):- replace_Element(F, 26, F1).
count(four_of_a_kind, two_pair, F, F1):- replace_Element(F, 27, F1).
count(four_of_a_kind, pair, F, F1):- replace_Element(F, 28, F1).
count(four_of_a_kind, high_card, F, F1):- replace_Element(F, 29, F1).


%% player1 is full_house

count(full_house, royal_flush, F, F1):- replace_Element(F, 30, F1).
count(full_house, straight_flush, F, F1):- replace_Element(F, 31, F1).
count(full_house, four_of_a_kind, F, F1):- replace_Element(F, 32, F1).
count(full_house, full_house, F, F1):- replace_Element(F, 33, F1).
count(full_house, flush, F, F1):- replace_Element(F, 34, F1).
count(full_house, straight, F, F1):- replace_Element(F, 35, F1).
count(full_house, three_of_a_kind, F, F1):- replace_Element(F, 36, F1).
count(full_house, two_pair, F, F1):- replace_Element(F, 37, F1).
count(full_house, pair, F, F1):- replace_Element(F, 38, F1).
count(full_house, high_card, F, F1):- replace_Element(F, 39, F1).


%% player1 is flush

count(flush, royal_flush, F, F1):- replace_Element(F, 40, F1).
count(flush, straight_flush, F, F1):- replace_Element(F, 41, F1).
count(flush, four_of_a_kind, F, F1):- replace_Element(F, 42, F1).
count(flush, full_house, F, F1):- replace_Element(F, 43, F1).
count(flush, flush, F, F1):- replace_Element(F, 44, F1).
count(flush, straight, F, F1):- replace_Element(F, 45, F1).
count(flush, three_of_a_kind, F, F1):- replace_Element(F, 46, F1).
count(flush, two_pair, F, F1):- replace_Element(F, 47, F1).
count(flush, pair, F, F1):- replace_Element(F, 48, F1).
count(flush, high_card, F, F1):- replace_Element(F, 49, F1).


%% player1 is straight

count(straight, royal_flush, F, F1):- replace_Element(F, 50, F1).
count(straight, straight_flush, F, F1):- replace_Element(F, 51, F1).
count(straight, four_of_a_kind, F, F1):- replace_Element(F, 52, F1).
count(straight, full_house, F, F1):- replace_Element(F, 53, F1).
count(straight, flush, F, F1):- replace_Element(F, 54, F1).
count(straight, straight, F, F1):- replace_Element(F, 55, F1).
count(straight, three_of_a_kind, F, F1):- replace_Element(F, 56, F1).
count(straight, two_pair, F, F1):- replace_Element(F, 57, F1).
count(straight, pair, F, F1):- replace_Element(F, 58, F1).
count(straight, high_card, F, F1):- replace_Element(F, 59, F1).


%% player1 is three_of_a_kind

count(three_of_a_kind, royal_flush, F, F1):- replace_Element(F, 60, F1).
count(three_of_a_kind, straight_flush, F, F1):- replace_Element(F, 61, F1).
count(three_of_a_kind, four_of_a_kind, F, F1):- replace_Element(F, 62, F1).
count(three_of_a_kind, full_house, F, F1):- replace_Element(F, 63, F1).
count(three_of_a_kind, flush, F, F1):- replace_Element(F, 64, F1).
count(three_of_a_kind, straight, F, F1):- replace_Element(F, 65, F1).
count(three_of_a_kind, three_of_a_kind, F, F1):- replace_Element(F, 66, F1).
count(three_of_a_kind, two_pair, F, F1):- replace_Element(F, 67, F1).
count(three_of_a_kind, pair, F, F1):- replace_Element(F, 68, F1).
count(three_of_a_kind, high_card, F, F1):- replace_Element(F, 69, F1).


%% player1 is two_pair

count(two_pair, royal_flush, F, F1):- replace_Element(F, 70, F1).
count(two_pair, straight_flush, F, F1):- replace_Element(F, 71, F1).
count(two_pair, four_of_a_kind, F, F1):- replace_Element(F, 72, F1).
count(two_pair, full_house, F, F1):- replace_Element(F, 73, F1).
count(two_pair, flush, F, F1):- replace_Element(F, 74, F1).
count(two_pair, straight, F, F1):- replace_Element(F, 75, F1).
count(two_pair, three_of_a_kind, F, F1):- replace_Element(F, 76, F1).
count(two_pair, two_pair, F, F1):- replace_Element(F, 77, F1).
count(two_pair, pair, F, F1):- replace_Element(F, 78, F1).
count(two_pair, high_card, F, F1):- replace_Element(F, 79, F1).


%% player1 is pair

count(pair, royal_flush, F, F1):- replace_Element(F, 80, F1).
count(pair, straight_flush, F, F1):- replace_Element(F, 81, F1).
count(pair, four_of_a_kind, F, F1):- replace_Element(F, 82, F1).
count(pair, full_house, F, F1):- replace_Element(F, 83, F1).
count(pair, flush, F, F1):- replace_Element(F, 84, F1).
count(pair, straight, F, F1):- replace_Element(F, 85, F1).
count(pair, three_of_a_kind, F, F1):- replace_Element(F, 86, F1).
count(pair, two_pair, F, F1):- replace_Element(F, 87, F1).
count(pair, pair, F, F1):- replace_Element(F, 88, F1).
count(pair, high_card, F, F1):- replace_Element(F, 89, F1).


%% player1 is high_card

count(high_card, royal_flush, F, F1):- replace_Element(F, 90, F1).
count(high_card, straight_flush, F, F1):- replace_Element(F, 91, F1).
count(high_card, four_of_a_kind, F, F1):- replace_Element(F, 92, F1).
count(high_card, full_house, F, F1):- replace_Element(F, 93, F1).
count(high_card, flush, F, F1):- replace_Element(F, 94, F1).
count(high_card, straight, F, F1):- replace_Element(F, 95, F1).
count(high_card, three_of_a_kind, F, F1):- replace_Element(F, 96, F1).
count(high_card, two_pair, F, F1):- replace_Element(F, 97, F1).
count(high_card, pair, F, F1):- replace_Element(F, 98, F1).
count(high_card, high_card, F, F1):- replace_Element(F, 99, F1).

replace_Element(F, I, F1):- nth0(I, F, A), add1(A, A1), replace(F, I, A1, F1).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).


add1(A, A1):- A1 is A + 1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Playing the game.
winner(H1, H2, Winner) :-
  sort_hand(H1, Sorted_Hand1),
  sort_hand(H2, Sorted_Hand2),
  determine_hand(Sorted_Hand1,  X1), 
  determine_hand(Sorted_Hand2,  X2), 
  beats(X1, X2, Verdict),
  (Verdict = X1, Winner = H1;
   Verdict = X2, Winner = H2;
   Verdict = tie, tiebreak(X1, Sorted_Hand1, Sorted_Hand2, SortedWinner),
   (SortedWinner = left, Winner = H1 ;
    SortedWinner = right, Winner = H2)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Tiebreaks
tiebreak(straight_flush, H1, H2, Winner)  :- higher_last_card(H1, H2, Winner).
tiebreak(four_of_a_kind, H1, H2, Winner)  :- higher_middle_card(H1, H2, Winner).
tiebreak(full_house, H1, H2, Winner)      :- higher_middle_card(H1, H2, Winner).
tiebreak(flush, H1, H2, Winner)           :- tiebreak(high_card, H1, H2, Winner).
tiebreak(straight, H1, H2, Winner)        :- higher_last_card(H1, H2, Winner).
tiebreak(three_of_a_kind, H1, H2, Winner) :- higher_middle_card(H1, H2, Winner).

tiebreak(two_pair, H1, H2, Winner) :-
  isolate_pairs(H1, [HighCard1,_], [LowCard1,_], Last1),
  isolate_pairs(H2, [HighCard2,_], [LowCard2,_], Last2),
  (beats_with_hand(H1, HighCard1, H2, HighCard2, Winner),
   Winner \= tie;
   beats_with_hand(H1, LowCard1, H2, LowCard2, Winner),
   Winner \= tie;
   beats_with_hand(H1, Last1, H2, Last2, Winner)).
     
tiebreak(pair, H1, H2, Winner) :-
  isolate_pair(H1, [PairCard1,_], Rst1),
  isolate_pair(H2, [PairCard2,_], Rst2),
  (beats_with_hand(H1, PairCard1, H2, PairCard2, Winner), Winner \= tie ;
   tiebreak(high_card, Rst1, Rst2, Winner)).

tiebreak(high_card, H1, H2, X) :- 
  reverse(H1, RevH1),
  reverse(H2, RevH2),
  highest_card_chain(RevH1, RevH2, X).




beats_with_hand(H1, C1, H2, C2, X) :-
  beats(C1, C2, C1), X = left ;
  beats(C1, C2, C2), X = right ;
  X = tie.

% Really ugly.  How to better do this?
isolate_pairs(Hand, High_Pair, Low_Pair, Last) :-
  [[V1,S1],[V2,S2],[V3,S3],[V4,S4],[V5,S5]] = Hand,
  (V5 = V4, High_Pair = [[V4,S4],[V5,S5]],
    (V3 = V2, Low_Pair = [[V3,S3],[V2,S2]], Last = [V1,S1] ;
     V1 = V2, Low_Pair = [[V1,S1],[V2,S2]], Last = [V3,S3])) ;
  (Low_Pair = [[V1,S1],[V2,S2]], 
   High_Pair = [[V3,S3],[V4,S4]],
   Last = [V5,S5]).

isolate_pair(Hand, Pair, Rst) :-
  [[V1,S1],[V2,S2],[V3,S3],[V4,S4],[V5,S5]] = Hand,
  (V1 = V2, Pair = [[V1,S1],[V2,S2]], Rst = [[V3,S3],[V4,S4],[V5,S5]] ;
   V2 = V3, Pair = [[V3,S3],[V2,S2]], Rst = [[V1,S1],[V4,S4],[V5,S5]] ;
   V4 = V3, Pair = [[V3,S3],[V4,S4]], Rst = [[V1,S1],[V2,S2],[V5,S5]] ;
   V4 = V5, Pair = [[V5,S5],[V4,S4]], Rst = [[V1,S1],[V2,S2],[V3,S3]]).
  

highest_card_chain([H1|T1], [H2|T2], X) :-
  beats(H1,H2,Verdict),
  (Verdict = H1, X = left ;
   Verdict = H2, X = right ;
   Verdict = tie, highest_card_chain(T1,T2,X)).

higher_last_card(H1,H2,Winner) :-
  H1 = [_,_,_,_,[V1,_]],
  H2 = [_,_,_,_,[V2,_]],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left ;
   Higher = V2, Winner = right).

higher_middle_card(H1, H2, Winner) :-
  H1 = [_,_,[V1,_],_,_],
  H2 = [_,_,[V2,_],_,_],
  beats(V1,V2,Higher),
  (Higher = V1, Winner = left;
   Higher = V2, Winner = right).



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
