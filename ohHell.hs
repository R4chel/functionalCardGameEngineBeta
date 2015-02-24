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
    gameLoop players $ StartRound NoPass (S.fromList [0,0,0,0]) 1

-- dataflow states, may not need to have them
gameLoop _players (RoundOver scores)
    = do
    putStrLn "Round Over"
    -- check for shooting the moon
    let moon_shot = 26 `S.elemIndexL` scores
    scores' <-
        case moon_shot of
            Nothing -> return scores
            Just p -> do
                putStrLn $ "Player " ++ show p ++ " shot the moon"
                return $ fmap (26-) scores
    -- send info to clients
    renderText (BetweenRounds scores)
    return $ RoundOver scores'

gameLoop _players (GameOver scores)
    = do
    -- should really be send message to clients
    putStrLn "Game Over"; print scores
    -- msg clients game over
    return $ GameOver scores

-- World controlling events in a round
gameLoop players (StartRound passDir scores roundNumber)
    = do
    deck <- shuffledDeck
    let deal = fmap unorderPile $ S.take 4 $ S.unfoldr (drawExactly roundNumber) $ S.fromList deck
    RoundOver round_scores <- gameLoop players $ PassingPhase deal passDir

    let new_scores = S.zipWith (+) round_scores scores
    if checkScores new_scores then return $ GameOver new_scores
    else gameLoop players $ StartRound next_pass_dir new_scores newRoundNumber
    where checkScores = F.any (>100)
          next_pass_dir = case passDir of
                        PassLeft    -> PassRight
                        PassRight   -> PassAcross
                        PassAcross  -> NoPass
                        NoPass      -> NoPass
          newRoundNumber = roundNumber + 1


                -- World when trying to pass
gameLoop players (PassingPhase deal passDir)
    = do
    board <-
        if passDir == NoPass then return deal else
        let getValidatedSelection i
                = do
                 candCardSet <- msgClient (players!!i) (StcGetPassSelection (deal `S.index` i) passDir)
                 validate candCardSet
                -- validate $ client (StcGetPassSelection (deal `S.index` i) passDir)
            validate (CtsPassSelection toPass) = return toPass
            -- TODO would prefer this to be non-stupid
            validate _ = error "need to make this try-catch or somesuch"
            rotate (x :< xs) =  xs |> x
            rotate (Empty) =  S.empty
            rotate _ = error "this is not a sequence"
        in do
        renderText (Passing (deal `S.index` 0) passDir)
        s0 <- getValidatedSelection 0
        s1 <- getValidatedSelection 1
        s2 <- getValidatedSelection 2
        s3 <- getValidatedSelection 3
        let s = S.fromList [s0,s1,s2,s3]
        let s' = case passDir of
                NoPass      -> s
                PassLeft    -> rotate s
                PassAcross  -> rotate $ rotate s
                PassRight   -> rotate $ rotate $ rotate s
        return $ S.zipWith Z.union s' $ S.zipWith (Z.\\) deal s

    let who_starts = 0 -- TODO make left of dealer
    gameLoop players $ InRound board [NewTrick]
                     $ TrickInfo who_starts S.empty (S.fromList [0,0,0,0])

                -- World when in middle of round
gameLoop _players (InRound _board [] _info)
    = error "stack is empty"
    -- Fix this case
gameLoop players (InRound board (now:on_stack) info)
    = do
    renderText (RenderServerState board info)
    let world' = InRound board on_stack info
    -- need to guarantee that stack is never empty
    case now of
        NewTrick ->
            gameLoop players $ InRound board (GetInput:GetInput:GetInput:GetInput:ComputeWinner:on_stack) info
            -- consider computing winner at end of trick
            -- as new effect so
            -- 4x get_input : computeWinner : NewTrick
        ComputeWinner ->
            -- split new trick into here
            let (w,s) = computeWinner info
                nextTrick = TrickInfo w S.empty s
                nextStep = if (>0) . Z.size $ board `S.index` 0
                    then InRound board (NewTrick:on_stack) nextTrick
                    else RoundOver s
            in
            gameLoop players nextStep
        GetInput -> do
            let hand = board `S.index` curPlayer info
            move <- msgClient (players!!curPlayer info) (StcGetMove hand info)
            let player_input = validate move
            gameLoop players $ InRound board (player_input:on_stack) info
            where validate (CtsMove move) = Effect (play move)
                  validate _ = error "recieved wrong type of message"
        Effect move ->
            gameLoop players $ move world'

curPlayer :: Info -> Int
curPlayer (TrickInfo p _ _) = p

computeWinner :: Info -> (PlayerID, Scores)
computeWinner (TrickInfo _ played scores) =
    let winner = trickWinner played Nothing
        new_scores  = S.adjust (+ 1) winner scores
    in
        (winner, new_scores)

play :: Card -> World -> World
play card (InRound board _stack (TrickInfo cur_player played scores)) =
    let new_board = S.adjust (Z.delete card) cur_player board
        new_played = played |> card
        next_player = (cur_player + 1) `mod` 4
    in
        InRound new_board _stack (TrickInfo next_player new_played scores)
play _ _ = error "world not InRound"


-- Patterns go at end of file since hlint can't parse them
pattern Empty   <- (S.viewl -> S.EmptyL)
pattern x :< xs <- (S.viewl -> x S.:< xs)
-- pattern xs :> x <- (S.viewr -> xs S.:> x)
