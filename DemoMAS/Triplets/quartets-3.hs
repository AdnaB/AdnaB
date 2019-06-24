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
    let current_player = "A"
    putStrLn "Hey, let us play! Well met, let's play Triplets!\n (Deal will be of the form (a1,a2,b1,b2,c1,c2). Make sure a1<a2,b1<b2,c1<2, and all numbers are used)\n Input the real deal (e.g. (1,2,3,4,5,6))" 
    deal <- getLine  
    let dealint = read deal :: (Int,Int,Int,Int,Int,Int)
    putStrLn "What a nice deal! We will use this as the real world. Making the initial model."
    let m = quar_init dealint
    putStrLn "Done! Do you want to see the model?(y/n)" 
    yn <- getLine
    (if yn == "y" then showM m else putStrLn "That is fine")
    putStrLn ("Starting SIMULATION NOW!")
    main_loop m current_player
    
main_loop  m current_player = do
    putStrLn ("---NEW MOVE---")
    let current_state = get_current_state m
    putStrLn ("Asking player: " ++ current_player)
    putStrLn ("Current state: " ++ show current_state)
    putStrLn ("Which Player do you want " ++ current_player ++ " to ask a card from?(A,B,C (do not choose asking player))")  
    asked_player <- getLine
    putStrLn ("Which card do you want " ++ current_player ++ " to ask from " ++ asked_player ++ "?(1..6(choose card asking player does not have, but has a card of the set.(sets:(1,2,3),(4,5,6))")
    card <- getLine
    let cardint = read card :: (Int)
    let askingV = check_PQR current_player
    let askedV = check_PQR asked_player
    putStrLn ("Updating model with: " ++ current_player ++ " asking " ++ asked_player ++ " for card " ++ card ++ ".") 
    let new_m = ask_card m askingV askedV cardint
    putStrLn ("Updates done:") 
    putStrLn ("    -" ++ current_player ++ " publicly announces he has a card of set " ++ (get_set cardint) ++ " and that it is not card " ++ card ++ ".")
    let check = check_have_card m askedV cardint
    (if check then announce_got_card current_player asked_player card else announce_not_got_card current_player asked_player card)
    let new_current_player = get_new_current_player askingV askedV check
    putStrLn "Do you want to see the model?(y/n)" 
    yn <- getLine
    (if yn == "y" then showM new_m else putStrLn "That is fine")
    main_loop new_m new_current_player

announce_got_card asking asked card = do
    putStrLn ("    -" ++ asked ++ " got card " ++ card ++ ":")
    putStrLn ("        -" ++ asked ++ " publicly announces he has card " ++ card ++ ";")
    putStrLn ("        -" ++ asked ++ " publicly gives card " ++ card ++ " to " ++ asking ++ ".")
    putStrLn ("        -Because " ++ asked ++ " had the card, " ++ asking ++ " may ask again for a card.")
    
announce_not_got_card asking asked card = do
    putStrLn ("    -" ++ asked ++ " does not have card " ++ card ++ ":")
    putStrLn ("        -" ++ asked ++ " publicly announces he has not got card " ++ card ++ ";")
    putStrLn ("        -Because " ++ asked ++ " did not have card " ++ asked ++ " may now ask for a card.")

    
check_triplet :: (Int -> Prop) -> [Prop] -> Int
check_triplet player state
    |elem (player 1) state && elem (player 2) state && elem (player 3) state = 1
    |elem (player 4) state && elem (player 5) state && elem (player 6) state = 2
    |otherwise = 0




get_new_current_player :: (Int -> Prop) -> (Int -> Prop) -> Bool -> String
get_new_current_player asking asked check
    |check = check_PQR_r asking
    |otherwise = check_PQR_r asked
    
check_PQR :: String -> (Int -> Prop)
check_PQR letter
    |or[(letter == "A"),(letter == "a")] = P
    |or[(letter == "B"),(letter == "b")] = Q
    |otherwise = R
    
check_PQR_r :: (Int -> Prop) -> String 
check_PQR_r letter
    |letter 1 == P 1 = "A"
    |letter 1 == Q 1 = "B"
    |otherwise = "C"
    
get_set :: Int -> String
get_set i
    |elem i [1,2,3] = "(1,2,3)"
    |otherwise = "(4,5,6)"

available_move :: [Prop] -> (Int -> Prop) -> Int
available_move state player
    |2 == 5 = 1
    |otherwise = 2

    
    
    
    
    
