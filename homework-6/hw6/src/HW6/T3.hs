module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  
  , simulate
  ) where

import System.Random (StdGen, mkStdGen, uniformR, split)
import Control.Comonad (Comonad(extract, extend))
import Control.Monad (liftM2)
import Data.Foldable (Foldable(foldl'))

import Data.Grid (Grid(Grid), gUp, gDown, gLeft, gRight, gWrite)
import Data.ListZipper (lGenerator, lLeft, lRight)

-- | Configuration parameters for the infection model.
data Config = Config
  { probability      :: Double  -- ^ Infection probability (0 <= p <= 1).
  , incubationPeriod :: Int     -- ^ Number of steps for incubation.
  , illnessDuration  :: Int     -- ^ Number of steps a cell remains ill.
  , immunityDuration :: Int     -- ^ Number of steps a cell remains immune after illness.
  } deriving Show

-- | Represents the state of an individual cell in the grid.
data CellState
  = Healthy       -- ^ The cell is healthy (susceptible).
  | Infected Int  -- ^ The cell is in incubation state with a counter of steps.
  | Ill Int       -- ^ The cell is ill with a counter of steps.
  | Immune Int    -- ^ The cell is immune with a counter of steps.
  deriving Show

-- | A 'Cell' holds the current 'CellState' and a random generator.
data Cell = Cell
  { cellState :: CellState  -- ^ The current state of the cell.
  , cellRand :: StdGen      -- ^ Random generator used for infection probability.
  }

-- | Custom 'Show' instance to print the 'CellState' in a single character form.
instance Show Cell where
  show (Cell Healthy      _) = "_"
  show (Cell (Infected _) _) = "i"
  show (Cell (Ill _)      _) = "#"
  show (Cell (Immune _)   _) = "@"

-- | A type alias for the grid that holds 'Cell' values.
type Comonad19Grid = Grid Cell

-- | Creates an infinite list of grids using the given 'Config'.
-- Each element of this list represents one iteration of the infection simulation.
simulate
  :: Int              -- ^ Seed for the random generator.
  -> Config           -- ^ Infection configuration.
  -> [Comonad19Grid]  -- ^ Infinite list of simulated grids.
simulate seed config = iterate (evolve config) (startGrid seed config)

-- | Initializes the simulation grid by placing an infected cell in the center.
startGrid :: Int -> Config -> Comonad19Grid
startGrid seed config = gWrite firstInfected healthyGrid
  where
    firstInfected = Cell (Infected $ incubationPeriod config) (mkStdGen seed)
    healthyGrid   = Grid $ lGenerator lLeft lRight startLZ
    startLZ       = lGenerator goNext goNext centerCell
    centerCell    = Cell Healthy $ mkStdGen seed
    goNext cell   = let (_, rand) = split $ cellRand cell in Cell Healthy rand

-- | Evolves the entire grid by one step according to the 'rule' function.
evolve :: Config -> Comonad19Grid -> Comonad19Grid
evolve config = extend $ rule config

-- | A list of directions to neighbor cells (4 side-neighbors + 4 diagonals).
neighbors :: [Grid a -> Grid a]
neighbors = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [gLeft, gRight]
        verticals   = [gUp,   gDown ]

-- | Infection transition function for a single cell, considering its neighbors.
rule :: Config -> Comonad19Grid -> Cell
rule config grid =
  let currentCell = extract grid
      state       = cellState currentCell
      rng         = cellRand currentCell
  in case state of
    -- Infected -> Ill
    Infected 0 -> Cell (Ill $ illnessDuration config) rng
    Infected n -> Cell (Infected $ n - 1) rng

    -- Ill -> Immune
    Ill 0      -> Cell (Immune $ immunityDuration config) rng
    Ill n      -> Cell (Ill $ n - 1) rng

    -- Immune -> Healthy
    Immune 0   -> Cell Healthy rng
    Immune n   -> Cell (Immune $ n - 1) rng

    -- Healthy -> Infected
    Healthy    ->
      let neighborStates    = map (cellState . extract . ($ grid)) neighbors
          (gotInfected, r2) = checkNeighbors config rng neighborStates
      in if gotInfected
         then Cell (Infected $ incubationPeriod config) r2
         else Cell Healthy r2

-- | Checks whether a 'Healthy' cell should become infected based on its neighbors.
checkNeighbors
  :: Config          -- ^ Simulation parameters
  -> StdGen          -- ^ Current random generator
  -> [CellState]     -- ^ States of the neighboring cells
  -> (Bool, StdGen)  -- ^ Whether the cell becomes infected and the updated RNG
checkNeighbors config initGen =
  foldl' step (False, initGen)
  where
    step (alreadySick, g) st
      | alreadySick = (True, g)
      | otherwise   =
          case st of
            Infected _ -> tryInfect (False, g)
            Ill _      -> tryInfect (False, g)
            _          -> (False, g)

    tryInfect (_, g) =
      let (val, g') = uniformR (0.0, 1.0 :: Double) g
      in (val < probability config, g')
