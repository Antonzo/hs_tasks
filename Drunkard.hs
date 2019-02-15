module Drunkard where

import Data.List
import Data.Ord

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Ord,Eq, Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq, Show)

data Card = Card {value::Value ,
                  suit:: Suit} deriving Eq


sameSuit :: Card -> Card -> Bool
sameSuit c1 c2 = suit c1 == suit c2 


beats :: Card -> Card -> Ordering
c1 `beats` c2 = comparing (\x -> suit x) c1 c2


gr :: [Card]->[Card]->Int->([Card], [Card])
gr (x:xs) (y:ys) n
        | n >= 1000 = error "Cycled"
        |((xs == []) || (ys == [])) = (xs,ys)
        |(x `beats` y) == GT = (xs ++ [x,y] , ys) 
        |(x `beats` y ) == LT = (xs, ys ++ [x,y])
        |(x `beats` y ) == EQ  =  gr xs ys (n+1)
        |otherwise = error "111"      

game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round (xs,[]) = (xs,[])
game_round ([],ys) = ([],ys)
game_round (xs ,ys) = gr (xs) (ys) 0
                        
                                        

data Winner = First | Second deriving (Show)

game :: ([Card], [Card]) -> (Winner, Int)
game (xs,ys) = game' (0,(xs,ys))
            where game' ( n , ([],ys)) = (Second,n)
                  game' ( n , (xs,[])) = (First,n)
                  game' ( n , (xs,ys)) = if n <= 1000 then game' ((n+1),game_round (xs,ys)) else error "1000 rounds was not enough..."


run_game1 = game ([
      Card {suit = Clubs, value = Ace}
      ,Card {suit = Clubs, value = Four}
      ,Card {suit = Hearts, value = Two}  
      ,Card {suit = Hearts, value = Nine}  
      ,Card {suit = Spades, value = Ten} 
      ,Card {suit = Hearts, value = Six}  
      ,Card {suit = Spades, value = Eight}  
      ,Card {suit = Hearts, value = Five}  
      ,Card {suit = Clubs, value = Nine}  
      ,Card {suit = Clubs, value = Seven} ]
      ,[
      Card {suit = Spades, value = Four}
      ,Card {suit = Hearts, value = Jack}
      ,Card {suit = Diamonds, value = Jack}  
      ,Card {suit = Hearts, value = Eight}  
      ,Card {suit = Diamonds, value = Seven} 
      ,Card {suit = Hearts, value = Ten}  
      ,Card {suit = Clubs, value = Eight}  
      ,Card {suit = Hearts, value = Three}  
      ,Card {suit = Diamonds, value = Queen}  
      ,Card {suit = Spades, value = Six}])
--(First,9)
run_game2 = game ([
      Card {suit = Spades, value = Four}
      ,Card {suit = Hearts, value = Jack}
      ,Card {suit = Diamonds, value = Jack}  
      ,Card {suit = Hearts, value = Eight}  
      ,Card {suit = Diamonds, value = Seven} 
      ,Card {suit = Hearts, value = Ten}  
      ,Card {suit = Clubs, value = Eight}  
      ,Card {suit = Hearts, value = Three}  
      ,Card {suit = Diamonds, value = Queen}  
      ,Card {suit = Spades, value = Six}]
      ,[
      Card {suit = Clubs, value = Ace}
      ,Card {suit = Clubs, value = Four}
      ,Card {suit = Hearts, value = Two}  
      ,Card {suit = Hearts, value = Nine}  
      ,Card {suit = Spades, value = Ten} 
      ,Card {suit = Hearts, value = Six}  
      ,Card {suit = Spades, value = Eight}  
      ,Card {suit = Hearts, value = Six}  
      ,Card {suit = Clubs, value = Nine}  
      ,Card {suit = Clubs, value = Seven} ])


