solution(Solutions) :-  	%% after you get all the Solutions then to write it to a file  
  		open('A2poker3out1000-delete.txt', append, OutStream),
  		write(OutStream, Solutions),
		close(OutStream).

play(N) :-
	print('In Play'), nl,
	T = [0,0,0,0,0,0,0,0,0,0],
	TP = [0,0,0,0,0,0,0,0,0,0],
	TN = [0,0,0,0,0,0,0,0,0,0],
	loop(N, T, TP, TN).

list_sum([], 0).
list_sum([H| T], Sum) :-
    list_sum(T, Sum1),
    Sum is H + Sum1.

loop(0, T, TP, TN):- 
	print('In Base case loop'), nl, 
	print(T), nl,
	solution('Total : '), solution(T),  solution('\n'),	
	print(TP), nl, 
	solution('True Positive : '), solution(TP),  solution('\n'),
	print(TN), nl,
	solution('True Negative : '), solution(TN),  solution('\n'), solution('\n'),
	list_sum(T, TSum), list_sum(TP, TPSum), list_sum(TN, TNSum),
	Precision is (TPSum + TNSum) / TSum, 
	print('precision rate : '), print(Precision).
	
loop(N, T, TP, TN) :- 
	solution('Loop : '), solution(N), solution('\n'), solution('\n'),
	N>0, 
	N1 is N-1,
	play2(T, TP, TN, T1, TP1, TN1), 
	loop(N1, T1, TP1, TN1).


play2(T, TP, TN, T1, TP1, TN1) :- 
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
	winner(H1,H2, Winner),!,
	print(H1), nl, print(H2), nl, nl, print('win='), print(Winner), nl, nl,
	solution('Winner : '), solution(Winner), solution('\n'), solution('\n'),
	winner_list(Winner, H1, W1),
	predict_list(X1, P1),
	count_total(X1, T, T1),
	solution('Total : '), solution(T1),  solution('\n'),
	count_tp(P1, W1, X1, TP, TP1),
	solution('True Positive : '), solution(TP1),  solution('\n'),
	count_tn(P1, W1, X1, TN, TN1),
	solution('True Negative : '), solution(TN1),  solution('\n'), solution('\n').

predict_list(X1, P1):- X1 \== high_card, P1 = win.
predict_list(high_card, P1):- P1 = loss.

winner_list(Winner, H1, W1):-
	same(Winner, H1) -> W1 = win; W1 = loss.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Count Frequency	- player1 win

count_total(royal_flush, T, T1):- same(Winner, H1) -> replace_Element(T, 0, T1); T1 = T.
count_total(straight_flush, T, T1):- same(Winner, H1) -> replace_Element(T, 1, T1); T1 = T.
count_total(four_of_a_kind, T, T1):- same(Winner, H1) -> replace_Element(T, 2, T1); T1 = T.
count_total(full_house, T, T1):- same(Winner, H1) -> replace_Element(T, 3, T1); T1 = T.
count_total(flush, T, T1):- same(Winner, H1) -> replace_Element(T, 4, T1); T1 = T.
count_total(straight, T, T1):- same(Winner, H1) -> replace_Element(T, 5, T1); T1 = T.
count_total(three_of_a_kind, T, T1):- same(Winner, H1) -> replace_Element(T, 6, T1); T1 = T.
count_total(two_pair, T, T1):- same(Winner, H1) -> replace_Element(T, 7, T1); T1 = T.
count_total(pair, T, T1):- same(Winner, H1) -> replace_Element(T, 8, T1); T1 = T.
count_total(high_card, T, T1):- same(Winner, H1) -> replace_Element(T, 9, T1); T1 = T.

count_tp(P1, W1, royal_flush, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 0, TP1); TP1 = TP.
count_tp(P1, W1, straight_flush, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 1, TP1); TP1 = TP.
count_tp(P1, W1, four_of_a_kind, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 2, TP1); TP1 = TP.
count_tp(P1, W1, full_house, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 3, TP1); TP1 = TP.
count_tp(P1, W1, flush, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 4, TP1); TP1 = TP.
count_tp(P1, W1, straight, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 5, TP1); TP1 = TP.
count_tp(P1, W1, three_of_a_kind, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 6, TP1); TP1 = TP.
count_tp(P1, W1, two_pair, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 7, TP1); TP1 = TP.
count_tp(P1, W1, pair, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 8, TP1); TP1 = TP.
count_tp(P1, W1, high_card, TP, TP1):- P1==W1, P1==win, same(Winner, H1) -> replace_Element(TP, 9, TP1); TP1 = TP.

count_tn(P1, W1, royal_flush, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 0, TN1); TN1 = TN.
count_tn(P1, W1, straight_flush, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 1, TN1); TN1 = TN.
count_tn(P1, W1, four_of_a_kind, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 2, TN1); TN1 = TN.
count_tn(P1, W1, full_house, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 3, TN1); TN1 = TN.
count_tn(P1, W1, flush, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 4, TN1); TN1 = TN.
count_tn(P1, W1, straight, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 5, TN1); TN1 = TN.
count_tn(P1, W1, three_of_a_kind, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 6, TN1); TN1 = TN.
count_tn(P1, W1, two_pair, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 7, TN1); TN1 = TN.
count_tn(P1, W1, pair, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 8, TN1); TN1 = TN.
count_tn(P1, W1, high_card, TN, TN1):- P1==W1, P1==loss, same(Winner, H1) -> replace_Element(TN, 9, TN1); TN1 = TN.


same([ ], [ ]).   
same([H1|R1], [H2|R2]):-
    H1 == H2,
    same(R1, R2).


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
