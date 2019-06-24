module Triplets
where 

import DEMO

deals = [((n0,n1),(n2,n3),(n4,n5)) | n0 <- [1..6] :: [Int], 
                                      n1 <- [1..6] \\ [n0], 
                                      n2 <- [1..6] \\ [n0,n1], 
                                      n3 <- [1..6] \\ [n0,n1,n2], 
                                      n4 <- [1..6] \\ [n0,n1,n2,n3],
                                      n5 <- [1..6] \\ [n0,n1,n2,n3,n4], 
                                      n0 < n1, 
                                      n2 < n3,
                                      n4 < n5] 

indexed_deals = zip [0..(89)] deals


--Initialize the model. 
-- First two cards are for p1, next two for p2 and so on 
-- !!!For every player cards: First input lowest card than highest
-- THUS: (1,2,3,4,5,6) AND NOT (1,2,4,3,5,6)
quar_init :: (Int,Int,Int,Int,Int,Int) -> EpistM 
quar_init (c1,c2,c3,c4,c5,c6) = Pmod [0..89] val acc n
    where 
      val = [ (i, [P n0, P n1, Q n2, Q n3, R n4, R n5]) | 
              (i, ((n0,n1),(n2,n3),(n4,n5))) <- indexed_deals ]
      acc = [ (a,w,v) | (w,(d,_,_)) <- indexed_deals, 
                        (v,(e,_,_)) <- indexed_deals, d == e ]
            ++
            [ (b,w,v) | (w,(_,d,_)) <- indexed_deals, 
                        (v,(_,e,_)) <- indexed_deals, d == e ]
            ++
            [ (c,w,v) | (w,(_,_,d)) <- indexed_deals, 
                        (v,(_,_,e)) <- indexed_deals, d == e ] 
      n   = [ j | 
              (j, ((d1,d2),(d3,d4),(d5,d6))) <- indexed_deals, d1 == c1, d2 == c2, d3 == c3, d4 == c4, d5 == c5, d6 == c6]
      
    
--Main asking function
--Given a model, an asking player, an asked player and a card asked, returns the updated model 
ask_card :: EpistM -> (Int -> Prop) -> (Int -> Prop) -> Int -> EpistM
ask_card old_model asking_player asked_player card
    |check_have_card old_model asked_player card = have_set2(have_set1 (change_card (upd (upd  old_model have_set_update) have_card_update) asking_player asked_player card))
    |otherwise = upd (upd  old_model have_set_update) have_card_update
        where
          have_set_update = announce_having_set asking_player card
          have_card_update
              |check_have_card old_model asked_player card = have_card asked_player card
              |otherwise = dont_have_card asked_player card
          have_set1 m
              |check_triplet1 asking_player (get_current_state m) = upd m (public(Conj[Prop(asking_player 1),Prop(asking_player 2),Prop(asking_player 3)]))
              |otherwise = upd m (public(Disj[Neg(Prop(asking_player 1)),Neg(Prop(asking_player 2)),Neg(Prop(asking_player 3))]))
          have_set2 m2
              |check_triplet2 asking_player (get_current_state m2) = upd m2 (public(Conj[Prop(asking_player 4),Prop(asking_player 5),Prop(asking_player 6)]))
              |otherwise = upd m2 (public(Disj[Neg(Prop(asking_player 4)),Neg(Prop(asking_player 5)),Neg(Prop(asking_player 6))]))
                  

                        
                        
--Announcement: If asking card from set, I have one of the other 2      
announce_having_set :: (Int -> Prop) -> Int  -> PoAM
announce_having_set player_asking  card_number 
    |card_number == 1 = public(Conj[Disj[Prop(player_asking 2),Prop(player_asking 3)],Neg(Prop(player_asking 1))])
    |card_number == 2 = public(Conj[Disj[Prop(player_asking 1),Prop(player_asking 3)],Neg(Prop(player_asking 2))])
    |card_number == 3 = public(Conj[Disj[Prop(player_asking 1),Prop(player_asking 2)],Neg(Prop(player_asking 3))])
    |card_number == 4 = public(Conj[Disj[Prop(player_asking 5),Prop(player_asking 6)],Neg(Prop(player_asking 4))])
    |card_number == 5 = public(Conj[Disj[Prop(player_asking 4),Prop(player_asking 6)],Neg(Prop(player_asking 5))])
    |otherwise = public(Conj[Disj[Prop(player_asking 4),Prop(player_asking 5)],Neg(Prop(player_asking 6))])
    
    
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

--Public announcement that a player has  a triplet (1 = (1,2,3), 2 = (4,5,6))
have_triplet :: (Int -> Prop) -> Int -> PoAM
have_triplet player set_number
    |set_number == 1 = public(Conj[Prop(player 1),Prop(player 2),Prop(player 3)])
    |set_number == 2 = public(Conj[Prop(player 4),Prop(player 5),Prop(player 6)])

--Change a card from players. We do this by changing the parameter in every world Ic to Jc 
--(every world will have Ic, because it was publicly announced that Ic)
change_card :: EpistM -> (Int -> Prop) -> (Int -> Prop) -> Int -> EpistM
change_card current_model@(Pmod worlds oldval acc points) receiving_player giving_player card = Pmod worlds val acc points
    where
      val = [ (i, [(receiving_player card), c2, c3, c4, c5, c6]) | 
              (i, [c1,c2,c3,c4,c5,c6]) <- oldval, c1 == (giving_player card)]
            ++
            [ (i, [c1, (receiving_player card), c3, c4, c5, c6]) | 
              (i, [c1,c2,c3,c4,c5,c6]) <- oldval, c2 == (giving_player card)]
            ++
            [ (i, [c1, c2, (receiving_player card), c4, c5, c6]) | 
              (i, [c1,c2,c3,c4,c5,c6]) <- oldval, c3 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, (receiving_player card), c5, c6]) | 
              (i, [c1,c2,c3,c4,c5,c6]) <- oldval, c4 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, c4, (receiving_player card), c6]) | 
              (i, [c1,c2,c3,c4,c5,c6]) <- oldval, c5 == (giving_player card)]
            ++
            [ (i, [c1, c2, c3, c4, c5, (receiving_player card)]) | 
              (i, [c1,c2,c3,c4,c5,c6]) <- oldval, c6 == (giving_player card)]
              
--function that returns the current state
get_current_state :: EpistM -> [Prop]
get_current_state model@(Pmod worlds val acc points) = snd(val!!fromInteger(points!!0))


--------------------------------------------------------
---ONLY NEEDED FOR THE MAIN LOOP OF SIMULATING A GAME---
--------------------------------------------------------
--Function to simulate a game of triplets
main = do  
    let current_player = "P"
    let t1 = False
    let t2 = False
    putStrLn "Hey, let us play! Well met, let's play Triplets!\n (Deal will be of the form (p1,p2,q1,q2,r1,r2). Make sure a1<a2,b1<b2,c1<2, and all numbers are used)\n Input the real deal (e.g. (1,2,3,4,5,6))" 
    deal <- getLine  
    let dealint = read deal :: (Int,Int,Int,Int,Int,Int)
    putStrLn "What a nice deal! We will use this as the real world. Making the initial model."
    let m = quar_init dealint
    putStrLn "Done! Do you want to see the model written out?(y/n)" 
    yn <- getLine
    (if yn == "y" then showM m else putStrLn "That is fine")
    putStrLn ("Starting SIMULATION NOW!")
    main_loop m current_player t1 t2
    
main_loop  m current_player t1 t2 = do
    putStrLn ("---NEW MOVE---")
    let current_state = get_current_state m
    putStrLn ("Asking player: " ++ current_player)
    putStrLn ("Current state: " ++ show current_state)
    putStrLn ("Which Player do you want " ++ current_player ++ " to ask a card from?(P,Q,R (do not choose asking player))")  
    asked_player <- getLine
    putStrLn ("Which card do you want " ++ current_player ++ " to ask from " ++ asked_player ++ "?(1..6(choose card asking player does not have, but has a card of the set.(sets:(1,2,3),(4,5,6))")
    card <- getLine
    let cardint = read card :: (Int)
    let askingV = check_PQR current_player
    let askedV = check_PQR asked_player
    putStrLn ("Updating model with: " ++ current_player ++ " asking " ++ asked_player ++ " for card " ++ card ++ ".") 
    let new_m = ask_card m askingV askedV cardint
    putStrLn ("---UPDATES---")
    putStrLn ("Updates done:") 
    putStrLn ("    -Everybody now knows that " ++ current_player ++ " has a card of set " ++ (get_set cardint) ++ " and that it is not card " ++ card ++ ".")
    let check = check_have_card m askedV cardint
    (if check then announce_got_card current_player asked_player card else announce_not_got_card current_player asked_player card)
    let newstate = get_current_state new_m 
    let nt1 = or[t1 ,(check_triplet1 askingV newstate)]
    let nt2 = or[t2,(check_triplet2 askingV newstate)]
    (if not(t1 == nt1) then announce_have_triplet current_player "(1,2,3)" else if not(t2 == nt2) then announce_have_triplet current_player "(4,5,6)" else announce_not_have_triplet current_player)
    let new_current_player = get_next_player askingV newstate t1 nt1 t2 nt2 check
    (if check && (not(new_current_player == current_player)) then putStrLn ("Because " ++ current_player ++ " can not make any more moves " ++ new_current_player ++ " may move now.") else putStrLn "...") 
    writeP "graph" new_m
    putStrLn "graph.dot can now be ran by Graphiz to show the new graphstructure"
    putStrLn "Do you want to see the model written out?(y/n)" 
    yn <- getLine
    (if yn == "y" then showM new_m else putStrLn "That is fine")
    let end = and[nt1,nt2]
    (if end then ended m else main_loop new_m new_current_player nt1 nt2)

get_next_player p current_state t1 nt1 t2 nt2 check
    |and[not(t1 == nt1),check_new_player_needed p current_state 1] = get_new_current_player p p False
    |and[not(t2 == nt2),check_new_player_needed p current_state 2] = get_new_current_player p p False
    |check = get_new_current_player p p True
    |otherwise = get_new_current_player p p False
    
ended m = do
    putStrLn "The game has ended. The final state is: "
    putStrLn (show (get_current_state m))
    putStrLn "I had fun, let's do another run!"

check_new_player_needed :: (Int -> Prop) -> [Prop] -> Int -> Bool
check_new_player_needed current_player state set
    |set == 1 = (if or[elem (current_player 4) state,elem (current_player 5) state,elem (current_player 6) state] then False else True)
    |otherwise = (if or[elem (current_player 1) state,elem (current_player 2) state,elem (current_player 3) state] then False else True)

announce_new_player_needed player newplayer = do
    putStrLn ("Because " ++ player ++ " just completed a set and has no additional cards, he can not ask for any cards anymore.")
    putStrLn ("The turn goes to " ++ newplayer ++ ".")
    
announce_have_triplet askingV setname = do
    putStrLn "    -Quartet!"
    putStrLn ("        -" ++ askingV ++ " publicly announces he has set " ++ setname)

announce_not_have_triplet askingV = do
   putStrLn (askingV ++ " has no new triplets. The others now know this.")
    
announce_got_card asking asked card = do
    putStrLn ("    -" ++ asked ++ " got card " ++ card ++ ":")
    putStrLn ("        -" ++ asked ++ " publicly gives card " ++ card ++ " to " ++ asking ++ ".")
    putStrLn ("        -Because " ++ asked ++ " had the card, " ++ asking ++ " may ask again for a card.")
    
    
announce_not_got_card asking asked card = do
    putStrLn ("    -" ++ asked ++ " does not have card " ++ card ++ ":")
    putStrLn ("        -" ++ asked ++ " publicly announces he has not got card " ++ card ++ ";")
    putStrLn ("        -Because " ++ asked ++ " did not have card " ++ asked ++ " may now ask for a card.")

    
check_triplet1 :: (Int -> Prop) -> [Prop] -> Bool
check_triplet1 player state
    |elem (player 1) state && elem (player 2) state && elem (player 3) state = True
    |otherwise = False

check_triplet2 :: (Int -> Prop) -> [Prop] -> Bool
check_triplet2 player state
    |elem (player 4) state && elem (player 5) state && elem (player 6) state = True
    |otherwise = False


get_new_current_player :: (Int -> Prop) -> (Int -> Prop) -> Bool -> String
get_new_current_player asking asked check
    |check = check_PQR_r asking
    |otherwise = next_player(check_PQR_r asking)
    
next_player :: String -> String
next_player player
    |player == "P" = "Q"
    |player == "Q" = "R"
    |otherwise = "P"
    
check_PQR :: String -> (Int -> Prop)
check_PQR letter
    |or[(letter == "P"),(letter == "p")] = P
    |or[(letter == "Q"),(letter == "q")] = Q
    |otherwise = R
    
check_PQR_r :: (Int -> Prop) -> String 
check_PQR_r letter
    |letter 1 == P 1 = "P"
    |letter 1 == Q 1 = "Q"
    |otherwise = "R"
    
get_set :: Int -> String
get_set i
    |elem i [1,2,3] = "(1,2,3)"
    |otherwise = "(4,5,6)"

available_move :: [Prop] -> (Int -> Prop) -> Int
available_move state player
    |2 == 5 = 1
    |otherwise = 2

    
    
    
    
    
