play :-
	print('In Play'), nl,
	P1 = 100, MoneyP1 = [],
	P2 = 100, MoneyP2 = [],
	loop(20, P1, P2, MoneyP1, MoneyP2).

loop(0, P1, P2, MoneyP1, MoneyP2):- print(MoneyP1), nl, print(MoneyP2), nl.
	
loop(N, P1, P2, MoneyP1, MoneyP2) :- 
	N>0, 
	N1 is N-1,
	play2(P1, P2, NewP1, NewP2), 
	append(MoneyP1, [NewP1], NewMoneyP1),
	append(MoneyP2, [NewP2], NewMoneyP2),
	loop(N1, NewP1, NewP2, NewMoneyP1, NewMoneyP2).


play2(P1, P2, NewP1, NewP2) :- 
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
	sort_hand(H1, Sorted_Hand1), print(Sorted_Hand1), nl,
	sort_hand(H2, Sorted_Hand2), print(Sorted_Hand2), nl, nl,
	determine_hand(Sorted_Hand1,  X1), print('Determine Hand of H1 : '), print(X1), nl,
	determine_hand(Sorted_Hand2,  X2), print('Determine Hand of H2 : '), print(X2), nl, nl,
	winner(H1,H2, Winner),!,
	print('win='), print(Winner), nl, nl,
	same(Winner, H1) -> player1_win(P1, P2, NewP1, NewP2); player2_win(P1, P2, NewP1, NewP2),
	print('Amount Player1 : '),print(NewP1), print('Amount Player2 : '), print(NewP2), nl, nl.

same([ ], [ ]).   
same([H1|R1], [H2|R2]):-
    H1 == H2,
    same(R1, R2).

player1_win(P1, P2, NewP1, NewP2):- NewP1 is P1 + 5, NewP2 is P2 - 5.
player2_win(P1, P2, NewP1, NewP2):- NewP1 is P1 - 5, NewP2 is P2 + 5.

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
