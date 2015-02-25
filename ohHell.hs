{-# LANGUAGE ViewPatterns, PatternSynonyms #-} -- for pattern matching on sequences
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-} -- for the serializable nonsense
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}
-- {-# LANGUAGE TemplateHaskell #-} -- make lenses maybe
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

import PlayingCards
import OhHellCommon
import OhHellClient
import qualified Data.Set as Z
import Data.Sequence ((|>), (<|))
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad (void)
import Data.Maybe (fromJust)

-- for serialization
-- import Data.Typeable
-- import Data.Binary
-- import GHC.Generics (Generic)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

-- import Async
-- import Control.Distributed.Process

-- import Data.Vector
-- consider replacing all list with sequences
-- import prelude as qualified
-- and importing all of sequence



main :: IO ()
main = do
        p0 <- constructPlayer client
        p1 <- constructPlayer aiclient
        p2 <- constructPlayer aiclient
        p3 <- constructPlayer aiclient
        --_renderer <- constructGUIPlayer
        void $ gameLoop [p0,p1,p2,p3] StartGame

msgClient :: Player -> ServerToClient -> IO ClientToServer
msgClient (inbox, outbox, _) message
    = do
    atomically $ putTMVar inbox message
    atomically $ takeTMVar outbox

gameLoop :: [Player] -> World -> IO World
gameLoop players StartGame
    = do
    --mapM_ (msgClient flip StcGameStart) players
    _ <- msgClient (head players) StcGameStart
    gameLoop players $ StartRound 0 (S.fromList [0,0,0,0]) 1

-- dataflow states, may not need to have them
gameLoop _players (RoundOver scores bids)
    = do
    putStrLn "Round Over"
    -- check for shooting the moon
    let findScore s b = if (==) s b then 10 + b else 0
    let scores' = S.zipWith findScore scores bids
    renderText (BetweenRounds scores')
    return $ RoundOver scores' bids

gameLoop _players (GameOver scores)
    = do
    -- should really be send message to clients
    putStrLn "Game Over"; print scores
    -- msg clients game over
    return $ GameOver scores

-- World controlling events in a round
gameLoop players (StartRound dealer scores roundNumber)
    = do
    deck <- shuffledDeck
    let deal = fmap unorderPile $ S.take 4 $ S.unfoldr (drawExactly roundNumber) $ S.fromList deck
    let trumpCard = F.find (const $ True) $ S.take 1 $ S.drop (4*roundNumber) $ S.fromList deck
    let trump = fmap _suit trumpCard
    RoundOver roundScores _bids <- gameLoop players $ BiddingPhase deal dealer roundNumber trump

    let newScores = S.zipWith (+) roundScores scores
    if checkRound newRoundNumber then return $ GameOver newScores
    else gameLoop players $ StartRound nextDealer newScores newRoundNumber
    where checkRound = (> 5) -- TODO don't hard code maybe use a boolean for up/down
          nextDealer = (dealer + 1) `mod` 4
          newRoundNumber = roundNumber + 1


                -- World when trying to bid
gameLoop players (BiddingPhase board dealer _roundNumber trump)
    = do
    bids <- return (S.fromList [1,1,1,1])  --TODO actually get bids
{-        let getValidatedBid i = 1
              = do
                 candBid <- 0
                 validate candBid
                -- validate $ client (StcGetPassSelection (deal `S.index` i) passDir)
            --validate (CtsPassSelection toPass) = return toPass
            -- TODO would prefer this to be non-stupid
            validate _ = error "need to make this try-catch or somesuch"
        in do
        renderText (Bidding (board `S.index` 0))
        s0 <- getValidatedBid 0
        s1 <- getValidatedBid 1
        s2 <- getValidatedBid 2
        s3 <- getValidatedBid 3
        return (S.fromList [s0,s1,s2,s3])
-}

    let who_starts = (dealer + 1) `mod` 4
    gameLoop players $ InRound board [NewTrick] (TrickInfo who_starts S.empty (S.fromList [0,0,0,0]) trump) bids
                -- World when in middle of round
gameLoop _players (InRound _board [] _info _bids)
    = error "stack is empty"
    -- Fix this case
gameLoop players (InRound board (now:on_stack) info bids)
    = do
    renderText (RenderServerState board info bids)
    let world' = InRound board on_stack info bids
    -- need to guarantee that stack is never empty
    case now of
        NewTrick ->
            gameLoop players $ InRound board (GetInput:GetInput:GetInput:GetInput:ComputeWinner:on_stack) info bids
            -- consider computing winner at end of trick
            -- as new effect so
            -- 4x get_input : computeWinner : NewTrick
        ComputeWinner ->
            -- split new trick into here
            let (w,s) = computeWinner info
                nextTrick = TrickInfo w S.empty s (trumpSuit info)
                nextStep = if (>0) . Z.size $ board `S.index` 0
                    then InRound board (NewTrick:on_stack) nextTrick bids
                    else RoundOver s bids
            in
            gameLoop players nextStep
        GetInput -> do
            let hand = board `S.index` curPlayer info
            move <- msgClient (players!!curPlayer info) (StcGetMove hand info)
            let player_input = validate move
            gameLoop players $ InRound board (player_input:on_stack) info bids
            where validate (CtsMove move) = Effect (play move)
                  validate _ = error "recieved wrong type of message"
        Effect move ->
            gameLoop players $ move world'

curPlayer :: Info -> Int
curPlayer (TrickInfo p _ _ _) = p

trumpSuit :: Info -> Maybe Suit
trumpSuit (TrickInfo _ _ _ s) = s

computeWinner :: Info -> (PlayerID, Scores)
computeWinner (TrickInfo _ played scores trump) =
    let winner = trickWinner played trump
        new_scores  = S.adjust (+ 1) winner scores
    in
        (winner, new_scores)

play :: Card -> World -> World
play card (InRound board _stack (TrickInfo cur_player played scores trump) bids) =
    let new_board = S.adjust (Z.delete card) cur_player board
        new_played = played |> card
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player new_played scores trump) bids
play _ _ = error "world not InRound"
