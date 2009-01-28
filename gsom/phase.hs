-- | The GSOM Algorithm can be split up in multiple sequentially executed
-- phases. Each of these phases makes a certain number of passes over
-- the inputs. While doing so the algorithm modifies a given lattice
-- according to a certain set of specified parameters.
-- This module contains all the functionality needed to run one or more 
-- phases of the GSOM algorithm.
module Gsom.Phase(
  module Gsom.Input,
  Gsom.Lattice.newRandom, Gsom.Lattice.newCentered,
  Gsom.Lattice.putWeights,
  Phase(..),
  defaults, 
  phase,
  run
) where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Private Modules
------------------------------------------------------------------------------

import Gsom.Input
import Gsom.Lattice hiding (grow)
import Gsom.Node

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | This datatype encapsulates all the parameters needed to be known to 
-- run one phase of the GSOM algorithm.
data Phase = Phase {
  -- | The number of passes which are to be made over the input vectors.
  -- Since each step of the phase consumes one input vector, the overall
  -- number of steps the phase will have will be: 
  --
  -- * @steps = passes * length inputs@
  passes :: Int,
  -- | The initial size of the neighbourhood affected by weight adaption.
  -- This will decrease linearly while the phase is executed.
  neighbourhoodSize :: Int,
  -- | The function used to calculate the learning rate in each of the 
  -- phase's steps. 
  -- During each step @learninRate currentStep maxSteps@ is fed the 
  -- number (starting from zero) of the current step as the first 
  -- argument and the total number of steps the phase will have as the 
  -- second argument to calculate the learning rate which will be in 
  -- effect for this phase.
  learningRate :: Int -> Int -> Double,
  -- | The kernel function. It is used in conjunction with the learning 
  -- rate to adjust weight adaption. 
  -- @kernel currentNeighbourhoodsize gridDistance@ should take the 
  -- neighbourhood size which is in effect during the current step and
  -- a nodes grid distance from the winning node. 
  -- The neighbourhood size will be a real number due to the linear decrease.
  kernel :: Double -> Int -> Double,
  -- | The @grow@ flag determines whether this is a growing phase or not.
  -- If this is @False@ then no new nodes will be grown.
  grow :: Bool,
  -- | The spread factor is used to calculate the growth threshold according 
  -- to the formula:
  --
  -- * @GT = - sqrt('d')*ln('spreadFactor')@ 
  --
  -- where @d@ is the input dimension.
  spreadFactor  :: Double
}

type Phases = [Phase]

------------------------------------------------------------------------------
-- Running phases
------------------------------------------------------------------------------

-- | @'phase' parameters inputs@ will update the given @lattice@ by 
-- executing one phase of the GSOM algorithm with the given @inputs@ 
-- and @parameters@.
phase :: Phase -> Lattice -> Inputs -> IO Lattice
phase ps lattice is =
  liftM snd $ foldM consume (0, lattice) supply where 
    supply = passes ps `times` is
    gT = growthThreshold ps $ dimension is
    lR x = learningRate ps x steps
    steps = length supply
    fI = fromIntegral
    r x = ((1 - fI x / fI steps ) * fI (neighbourhoodSize ps)) :: Double
    consume (c, l) i = do
      winner <- bmu i lattice
      atomically $ do
        affected <- neighbourhood winner $ round (r c)
        mapM_ (update i (lR c) (kernel ps $ r c)) affected 
        when (grow ps) (updateError winner i >> vent l winner gT)
        nodeCount <- readTVar (count l)
        return $! (c + 1, l)

-- | Since a complete run of the GSOM algorithm means running a number of 
-- @'Phases'@ this is usually the main function used.
-- @run phases lattice inputs@ runs the GSOM algorithm by running the 
-- @phases@ in the order specified, each time making passes over @inputs@
-- and using the produced @'Lattice'@ to as an argument to the next phase.
-- The initial @'Lattice'@, @lattice@ may be constructed with the 
-- @'newRandom'@ and the @'newCentered'@ functions.
run :: Phases -> Lattice -> Inputs -> IO Lattice
run ps lattice is = foldM f lattice ps where
  f l p = putStrLn "Starting a phase..." >> phase p l is

------------------------------------------------------------------------------
-- Predefined Kernels
------------------------------------------------------------------------------

bubble, gaussian :: Double -> Int -> Double
-- | The simplest kernel, which essentially does nothing.
-- It always evaluates to @1@ thus havin no effect on updating weights.
bubble _ _ = 1

-- | A gaussian kernel. The neighbourhood size @s@ currently in effect 
-- controls the bell width while the distance @d@ of the current node 
-- is used as the actual kernel argument. Thus the formula is:
--
-- * @gaussian s d = exp(d^2/(2*s^2))@
gaussian s d = exp $ fromIntegral d ** 2 / (2 * s**2)

------------------------------------------------------------------------------
-- Predefined learning rate updating functions
------------------------------------------------------------------------------

-- | Functions to update the learning rate. Use currying to supply them 
-- with a starting value and use the resulting function to fill in the 
-- @'learningRate'@ field of @'Phase'@.  

linear, inverseAge :: Double -> Int -> Int -> Double

-- | The linear learning rate reduction function. If you supply it with 
-- the initial learning rate @lr@ it uses the following formula where 
-- @step@ is the current step the phase is in and @steps@ is the overall
-- number of steps the phase will take:
--
-- *@linear lr step steps = lr * (1-step/steps)@
linear lr step steps = lr * (1 - fromIntegral step / fromIntegral steps)

-- | The inverse time learning rate reduction function. Given an initial
-- learning rate of @lr@, a maximum number of steps of @steps@ and the 
-- current step number beeing @step@, the formula is:
--
-- *@inverseAge lr step steps = lr * steps / (steps + 100 * step)@
inverseAge lr step steps = let fI = fromIntegral in 
  lr * fI steps / (fI steps + 100 * fI step)

------------------------------------------------------------------------------
-- Predefined phases
------------------------------------------------------------------------------

-- | The three default phases of the GSOM algorithm. They all use the 
-- bubble kernel and a linear learning rate decrease.
defaultFirst, defaultSecond, defaultThird :: Phase

-- | The default first phase is the only growing phase. It makes @5@
-- passes over the input, uses an initial learning rate of @0.1@ and 
-- a starting neighbourhood size of @3@. The @'spreadFactor'@ is set
-- to @0.1@.
defaultFirst = Phase {
  passes = 5,
  neighbourhoodSize = 3,
  learningRate = linear 0.1,
  kernel = bubble,
  grow = True,
  spreadFactor = 0.1 
}

-- | The default for the second phase is a smoothing phase making @50@
-- passes over the input vectors with a learning rate of @0.05@ and an
-- initial neighbourhood size of @2@. Since there is no node growth the
-- @'spreadFactor'@ is ignored and thus set to @0@.
defaultSecond = Phase {
  passes = 50,
  neighbourhoodSize = 2,
  learningRate = linear 0.05,
  kernel = bubble,
  grow = False,
  spreadFactor = 0 
}

-- | The default for the third and last phase is a smoothing phase making 
-- @50@ passes over the input vectors with a learning rate of @0.01@ and 
-- an initial neighbourhood size of @1@. Since there is no node growth the
-- @'spreadFactor'@ is ignored and thus set to @0@.
defaultThird = Phase {
  passes = 50,
  neighbourhoodSize = 1,
  learningRate = linear 0.01,
  kernel = bubble,
  grow = False,
  spreadFactor = 0 
}

-- | This is the list of the three default phases of the GSOM algorithm.
defaults :: Phases
defaults = [defaultFirst, defaultSecond, defaultThird]

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------

-- | Calculates the growth threshold as explained in the documentation
-- for @'Phase'@.
growthThreshold :: Phase -> Int -> Double
growthThreshold ps d = 
  negate $ sqrt (fromIntegral d) * log (spreadFactor ps)

-- | @n `times` l@ returns @l@ repeated @n@ times.
-- @l@ has to be finite and because the length of @l@ is calculated
-- in the process.
times :: Int -> [a] -> [a]
n `times` l = take (n * length l) (cycle l)
