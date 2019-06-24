module Quartets
where 

import DEMO

--TODO:
--Make 4 players

deals = [((n0,n1),(n2,n3),(n4,n5),(n6,n7)) |  n0 <- [1..8] :: [Int], 
                                      n1 <- [1..8] \\ [n0], 
                                      n2 <- [1..8] \\ [n0,n1], 
                                      n3 <- [1..8] \\ [n0,n1,n2], 
                                      n4 <- [1..8] \\ [n0,n1,n2,n3],
                                      n5 <- [1..8] \\ [n0,n1,n2,n3,n4], 
									  n6 <- [1..8] \\ [n0,n1,n2,n3,n4,n5],
									  n7 <- [1..8] \\ [n0,n1,n2,n3,n4,n5,n6],
                                      n0 < n1, 
                                      n2 < n3,
                                      n4 < n5,
									  n6 < n7] 

indexed_deals = zip [0..2519] deals

--Initialize the model. 
-- First two cards are for p1, next two for p2 and so on 
-- !!!For every player cards: First input lowest card than highest
-- THUS: (1,2,3,4,5,6,7,8) AND NOT (1,2,4,3,5,6,7,8)
quar_init :: (Int,Int,Int,Int,Int,Int,Int,Int) -> EpistM 
quar_init (c1,c2,c3,c4,c5,c6,c7,c8) = Pmod [0..2519] val acc n
    where 
      val = [ (i, [P n0, P n1, Q n2, Q n3, R n4, R n5, S n6, S n7]) | 
              (i, ((n0,n1),(n2,n3),(n4,n5),(n6,n7))) <- indexed_deals ]
      acc = [ (a,w,v) | (w,(x,_,_,_)) <- indexed_deals, 
                        (v,(y,_,_,_)) <- indexed_deals, x == y ]
            ++
            [ (b,w,v) | (w,(_,x,_,_)) <- indexed_deals, 
                        (v,(_,y,_,_)) <- indexed_deals, x == y ]
            ++
            [ (c,w,v) | (w,(_,_,x,_)) <- indexed_deals, 
                        (v,(_,_,y,_)) <- indexed_deals, x == y ] 
            ++
            [ (d,w,v) | (w,(_,_,_,x)) <- indexed_deals, 
                        (v,(_,_,_,y)) <- indexed_deals, x == y ] 
      n   = [ j | 
              (j, ((d1,d2),(d3,d4),(d5,d6),(d7,d8))) <- indexed_deals, d1 == c1, d2 == c2, d3 == c3, d4 == c4, d5 == c5, d6 == c6, d7 == c7, d8 == c8]
      
    
--Main asking function
--Given a model, an asking player, an asked player and a card asked, returns the updated model 
ask_card :: EpistM -> (Int -> Prop) -> (Int -> Prop) -> Int -> EpistM
ask_card old_model asking_player asked_player card
    |check_have_card old_model asked_player card = change_card (upd (upd  old_model have_set_update) have_card_update) asking_player asked_player card
    |otherwise = upd (upd  old_model have_set_update) have_card_update
        where
          have_set_update = announce_having_set asking_player card
          have_card_update
              |check_have_card old_model asked_player card = have_card asked_player card
              |otherwise = dont_have_card asked_player card

                        
                        
--Announcement: If asking card from set, I have one of the other 2      
announce_having_set :: (Int -> Prop) -> Int  -> PoAM
announce_having_set player_asking  card_number 
    |card_number == 1 = public(Conj[Disj[Prop(player_asking 2),Prop(player_asking 3),Prop(player_asking 4)],Neg(Prop(player_asking 1))])
    |card_number == 2 = public(Conj[Disj[Prop(player_asking 1),Prop(player_asking 3),Prop(player_asking 4)],Neg(Prop(player_asking 2))])
    |card_number == 3 = public(Conj[Disj[Prop(player_asking 1),Prop(player_asking 2),Prop(player_asking 4)],Neg(Prop(player_asking 3))])
    |card_number == 4 = public(Conj[Disj[Prop(player_asking 1),Prop(player_asking 2),Prop(player_asking 3)],Neg(Prop(player_asking 4))])
    |card_number == 5 = public(Conj[Disj[Prop(player_asking 6),Prop(player_asking 7),Prop(player_asking 8)],Neg(Prop(player_asking 5))])
    |card_number == 6 = public(Conj[Disj[Prop(player_asking 5),Prop(player_asking 7),Prop(player_asking 8)],Neg(Prop(player_asking 6))])
    |card_number == 7 = public(Conj[Disj[Prop(player_asking 5),Prop(player_asking 6),Prop(player_asking 8)],Neg(Prop(player_asking 7))])
    |otherwise = public(Conj[Disj[Prop(player_asking 5),Prop(player_asking 6),Prop(player_asking 7)],Neg(Prop(player_asking 8))])
    
    
--Used to check if a player has a card
check_have_card :: EpistM -> (Int -> Prop) -> Int -> Bool
check_have_card model player card
    |elem (player card) (get_current_state model) = True
    |otherwise = False


--Public announcement that a player does have a card
have_card :: (Int -> Prop) -> Int -> PoAM
have_card asked_player card_number = public(Prop(asked_player card_number)) 
    
    
--Public announcement that a player does not have a card
dont_have_card :: (Int -> Prop) -> Int -> PoAM
dont_have_card asked_player card_number = public(Neg(Prop(asked_player card_number)))
    

--Change a card from players. We do this by changing the parameter in every world Ic to Jc 
--(every world will have Ic, because it was publicly announced that Ic)
change_card :: EpistM -> (Int -> Prop) -> (Int -> Prop) -> Int -> EpistM
change_card current_model@(Pmod worlds oldval acc points) receiving_player giving_player card = Pmod worlds val acc points
    where
      val = [ (i, [(receiving_player card), c2, c3, c4, c5, c6, c7, c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c1 == (giving_player card)]
            ++
            [ (i, [c1, (receiving_player card), c3, c4, c5, c6, c7, c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c2 == (giving_player card)]
            ++
            [ (i, [c1, c2, (receiving_player card), c4, c5, c6, c7, c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c3 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, (receiving_player card), c5, c6, c7, c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c4 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, c4, (receiving_player card), c6, c7, c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c5 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, c4, c5, (receiving_player card), c7, c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c6 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, c4, c5, c6, (receiving_player card),c8]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c7 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, c4, c5, c6, c7,(receiving_player card)]) | 
              (i, [c1,c2,c3,c4,c5,c6, c7, c8]) <- oldval, c8 == (giving_player card)]
              
--function that returns the current state
get_current_state :: EpistM -> [Prop]
get_current_state model@(Pmod worlds val acc points) = snd(val!!fromInteger(points!!0))


            
                
      
      
      
      
      