module OhHellCommon
    ( Info(..)
    , Effect(..)
    , World(..)
    -- type synonyms
    , Scores
    , PlayerID
    , Board
    -- Communication Related
    , Message(..)
    , ClientToServer(..)
    , ServerToClient(..)
    ) where
import PlayingCards
import Data.Set (Set)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Foldable as F

data Effect = Effect (World -> World)
                | GetInput
                | NewTrick
                | ComputeWinner

-- curPlayer, played so far, scores this round, trump
data Info = TrickInfo PlayerID Trick Scores (Maybe Suit)
data World = InRound Board Stack Info Scores
            | StartGame
            | StartRound PlayerID Scores Int
            | BiddingPhase Board PlayerID Int (Maybe Suit)
            | RoundOver Scores Scores
            | GameOver Scores
type Stack = [Effect]
type Scores = Seq Int
type PlayerID = Int
type Board = Seq Hand

data Message = ClientToServer | ServerToClient
data ClientToServer = CtsMove Card
                    | CtsPassSelection (Set Card)
                    | CtsDisconnect
                    | CtsAcknowledge

data ServerToClient = StcGetMove Hand Info
                    | StcGetBid Hand
                    | StcGameStart
                    | StcGameOver
