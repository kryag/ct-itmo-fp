module Main (main) where

import Options.Applicative
import Control.Exception (Exception)
import HW6.T3
import Data.ListZipper (toList)
import Data.Grid

-- | Command-line parameters structure.
data Parameters = Parameters
  { prob       :: Double -- ^ Infection probability (0 <= p <= 1).
  , incub      :: Int    -- ^ Incubation period duration.
  , ill        :: Int    -- ^ Illness duration.
  , immun      :: Int    -- ^ Immunity duration.
  , gridSize   :: Int    -- ^ The size of the grid to be printed (width/height).
  , iterations :: Int    -- ^ Number of simulation iterations to print.
  } deriving Show

-- | Custom exception for invalid parameters.
data InvalidParameterException =
  InvalidProb
  | InvalidIncub
  | InvalidIll
  | InvalidImmun
  | InvalidGridSize
  | InvalidIterations

instance Show InvalidParameterException where
  show InvalidProb       = "prob must be between 0 and 1 (inclusive)"
  show InvalidIncub      = "incub must be >= 0"
  show InvalidIll        = "ill must be >= 0"
  show InvalidImmun      = "immun must be >= 0"
  show InvalidGridSize   = "grid-size must be >= 0"
  show InvalidIterations = "iterations must be >= 0"

instance Exception InvalidParameterException

-- | Validates 'Parameters' and throws an exception if any of them are invalid.
validateParams :: Parameters -> IO ()
validateParams params = do
  let errors = concat
        [ [InvalidProb       | prob params < 0 || prob params > 1]
        , [InvalidIncub      | incub params < 0]
        , [InvalidIll        | ill params   < 0]
        , [InvalidImmun      | immun params < 0]
        , [InvalidGridSize   | gridSize params < 0]
        , [InvalidIterations | iterations params < 0]
        ]
  if null errors
    then return ()
    else mapM_ print errors >> fail "Parameters validation failed"

-- | Parses command-line parameters.
parseParameters :: Parser Parameters
parseParameters = Parameters
  <$> option auto (mconcat
    [ long "prob"
    , metavar "DOUBLE"
    , value 0.5
    , showDefault
    , help "Infection probability"
    ])
  <*> option auto (mconcat
    [ long "incub"
    , metavar "INT"
    , value 2
    , showDefault
    , help "Incubation period duration"
    ])
  <*> option auto (mconcat
    [ long "ill"
    , metavar "INT"
    , value 5
    , showDefault
    , help "Illness duration"
    ])
  <*> option auto (mconcat
    [ long "immun"
    , metavar "INT"
    , value 7
    , showDefault
    , help "Immunity duration"
    ])
  <*> option auto (mconcat
    [ long "grid-size"
    , metavar "INT"
    , value 11
    , showDefault
    , help "Output grid size"
    ])
  <*> option auto (mconcat
    [ long "iterations"
    , metavar "INT"
    , value 10
    , showDefault
    , help "The number of simulation iterations"
    ])

-- | Parser info used by 'execParser'.
opts :: ParserInfo Parameters
opts = info (helper <*> parseParameters) $ mconcat
  [ fullDesc
  , progDesc "Simulation of Covid-19 infection on a 2-dimensional grid"
  , header   "Comonad-19"
  ]

-- | Global seed for the simulation.
genSeed :: Int
genSeed = 61

-- | Program entry point.
main :: IO ()
main = do
  params <- execParser opts
  validateParams params

  let config  = Config (prob params) (incub params) (ill params) (immun params)
      outSize = div (gridSize params - 1) 2
      iters   = iterations params
      grids   = take iters $ simulate genSeed config

  mapM_ (\(Grid rowZipper) -> do
    mapM_ (\row -> putStrLn $ show =<< toList row outSize) (toList rowZipper outSize)
    putStr "\n\n"
    ) grids
