module OhHellCommon
    ( Info(..)
    , Effect(..)
    , PassDir(..)
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

data PassDir = PassLeft | PassRight | PassAcross | NoPass deriving (Eq)

-- curPlayer, played so far, scores this round
data Info = TrickInfo PlayerID Trick Scores
data World = InRound Board Stack Info
            | StartGame
            | StartRound PassDir Scores
            | PassingPhase Board PassDir
            | RoundOver Scores
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
                    | StcGetPassSelection Hand PassDir
                    | StcGameStart
                    | StcGameOver
