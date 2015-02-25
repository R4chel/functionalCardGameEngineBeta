module OhHellClient
    ( constructGUIPlayer
    , constructPlayer
    , client
    , aiclient
    , Player
    , RenderInfo(..)
    , renderText
    )
    where
import OhHellCommon
import PlayingCards
import qualified Data.Set as Z
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Control.Concurrent
import Control.Concurrent.STM
-- import Control.Concurrent.STM.TMVar

import Control.Monad (forever)

import Graphics.Gloss
-- text rendering
import Data.List (intercalate) -- colorize

type Player = (TMVar ServerToClient, TMVar ClientToServer, ThreadId) -- ??

constructPlayer :: (ServerToClient -> IO ClientToServer) -> IO Player
constructPlayer respondTo
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ playerThread inbox outbox
    return (inbox, outbox, thread)

    where playerThread inbox outbox = forever $ do
            message <- atomically $ takeTMVar inbox
            response <- respondTo message
            atomically $ putTMVar outbox response

constructGUIPlayer :: IO Player
constructGUIPlayer
    = do
    inbox  <- newEmptyTMVarIO -- :: (TMVar ServerToClient)
    outbox <- newEmptyTMVarIO -- :: (TMVar ClientToServer)
    thread <- forkIO $ guiThread inbox outbox
    return (inbox, outbox, thread)

guiThread :: TMVar ServerToClient -> TMVar ClientToServer -> IO ()
guiThread _inbox _outbox
    = do play
            window
            white			 -- background color
            100              -- steps per second
            ""               -- world -- move to RenderInfo
            displayText      -- picture to display
            eventHandle      -- event handler
            (\_ world -> world) -- time update
    where displayText outpt = Translate (-170) (-20)
                  $ Scale 0.125 0.125
                  $ Text outpt
          eventHandle event _world = show event
          window = (InWindow
                   "Gloss" 	    -- window title
                                -- title fixed for xmonad
                   (800, 600)   -- window size
                   (10, 10)) 	-- window position


{-- Client Side code
 -- actual mechanism of splitting it as thread to be determined
 --
 -- Should split some validation stuff out so that
 -- it is accessible to both server and client --
 -- my client should always send valid input
 -- if server receives bad messages, it should check them
 --
 -- Also, rendering should go here
 --}

client :: ServerToClient -> IO ClientToServer

client StcGameStart = do
    return CtsAcknowledge

client (StcGetMove hand info) = do
    card <- getMove hand info
    return $ CtsMove card

client (StcGetBid hand) = do
   -- render $ Passing hand passDir
   cardSet <- getMultiCards 3 hand
   -- do client validation here
   return $ CtsPassSelection cardSet

client StcGameOver = return CtsDisconnect

getMove :: Hand -> Info -> IO Card
getMove hand info = do
    card <- getCardFromHand hand
    let TrickInfo _ trick _ _ = info in
        if followsSuit hand trick card
        then return card
        else do
            putStrLn "Illegal move: must follow suit"
            getMove hand info

getMultiCards :: Int -> Hand -> IO (Z.Set Card)
getMultiCards 0 _ = return Z.empty
--getMultiCards _ empty = return Z.empty
getMultiCards i hand = do
    card <- getCardFromHand hand
    others <- getMultiCards (i-1) (Z.delete card hand)
    return $ card `Z.insert` others


getCardFromHand :: Hand -> IO Card
getCardFromHand hand = do
    -- renderHand hand
    card <- getInput
    if card `Z.member` hand
    then return card
    else do
        putStrLn "Error: Card not in hand"
        getCardFromHand hand

getInput :: IO Card
getInput = do
    putStrLn "Choose Card: "
    -- for hearts players only choices in the play are which card to play
    -- We'll check that it's a legal play before constructing the effect
    mv <- getLine
    -- TODO: try parsing meta options and so forth too
    case readCard mv of
        Nothing -> do
                    putStrLn "Could not interpret move!"
                    getInput
        Just c -> return c

{- The trivial ai -}
{- should replace with random choice -}
aiclient :: ServerToClient -> IO ClientToServer
aiclient (StcGetMove hand info@(TrickInfo player trick scores _trump)) = do
    threadDelay 1000000 -- sleep 1 second
    case F.find (followsSuit hand trick) $ Z.toList hand of
        Nothing   -> error $ unlines
                    ["apparently cannot play card"
                    , show hand
                    , show trick
                    , show player
                    , show scores
                    ]
        Just card -> return $ CtsMove card

aiclient (StcGetBid hand) = do
    threadDelay 500000 -- sleep 0.5 second
    let cardSet = Z.fromList $ take 3 $ Z.toList hand
    return $ CtsPassSelection cardSet

aiclient StcGameStart = return CtsAcknowledge
aiclient StcGameOver = return CtsDisconnect


---------------------------------------------
-- Gui stuff
-- Render type
-- and rendering functions that get wrapped up
-- for gloss
data RenderInfo = RenderServerState Board Info Scores
                | Bidding Hand
                | BetweenRounds Scores
                | RenderInRound Hand Trick Scores

_render :: RenderInfo -> Picture
_render _rinfo = undefined
    {-let playArea  = renderPlayArea
        handArea  = renderHand pos dir hand
        debugArea = renderDebugArea
        leftOpp   = renderHand pos dir hand
        rightOpp  = renderHand pos dir hand
        acrossOpp = renderHand pos dir hand
    in
    Pictures [playArea, handArea, leftOpp, rightOpp, acrossOpp, debugArea]-}

--------
-- figure out display
--
-----------------------------------------------
renderText :: RenderInfo -> IO ()
renderText (RenderInRound hand played scores) = do
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    renderPlay played
    renderHand hand
    renderScores scores

renderText (RenderServerState board (TrickInfo curPlayer played scores trump) bids) = do
    -- if we should only be rendering the current players hand then do some checking
    -- the following clears the screen
    putStrLn "\ESC[H\ESC[2J"

    renderTrump trump
    renderPlay played
    renderBoard board curPlayer
    renderBidsAndTricks bids scores

renderText (Bidding hand) = renderHand hand

renderText (BetweenRounds scores) = renderScores scores

renderTrump :: Maybe Suit -> IO ()
renderTrump Nothing = putStrLn "Trump: None"
renderTrump (Just trump) = putStrLn $ "Trump: " ++ show trump

renderBidsAndTricks :: Scores -> Scores  -> IO ()
renderBidsAndTricks bids tricks = mapM_ showScore [0..3]
    where showScore  i = putStrLn $ showPlayer i ++ " Bid:" ++ show (bids `S.index` i) ++ " Tricks:" ++ show (tricks `S.index` i)
          showPlayer i = {- colorize  [44 | i==curPlayer] $ -} "Player " ++ show i

renderScores :: Scores -> IO ()
renderScores scores = mapM_ showScore [0..3]
    where showScore  i = putStrLn $ showPlayer i ++ " Score:" ++ show (scores `S.index` i)
          showPlayer i = {- colorize  [44 | i==curPlayer] $ -} "Player " ++ show i

renderBoard :: Board -> Int -> IO ()
renderBoard board activePlayer = mapM_ printHand [0..3]
    where printHand i = do
                        putStr $ colorize  [44 | i==activePlayer] $ concat ["Player ", show i, " Hand:"]
                        putStr " "
                        renderHand $ board `S.index` i

renderHand :: Hand -> IO ()
renderHand hand = putStrLn $ unwords $ map show $ Z.toList hand

renderPlay :: Trick -> IO ()
renderPlay played = putStrLn $ "Currently:" ++ F.concat (fmap ((' ':).show ) played)


colorize :: [Int] -> String -> String
colorize options str = "\ESC["
                        ++ intercalate ";" [show i | i <-options]
                        ++ "m" ++ str ++ "\ESC[0m"
