module KnuSkillz.Statement where

-- Imports
import qualified Data.Set as S

-- Represent a statement quite simply
newtype Statement = Statement (S.Set Int)
                        deriving Eq

-- Show instance
instance Show Statement where
    show (Statement ss) = show $ S.toList ss

-- Lift a function of two things
liftS2 :: (S.Set Int -> S.Set Int -> S.Set Int)
       -> Statement -> Statement -> Statement
liftS2 func (Statement xs) (Statement ys) = Statement $ func xs ys


-- Join of two statements
join :: Statement -> Statement -> Statement
join = liftS2 S.union


-- Meet of two statements
meet :: Statement -> Statement -> Statement
meet = liftS2 S.intersection


---- Does x imply y?
--implies :: Statement -> Statement -> Bool
--implies x y = 


---- A boolean lattice of size numAtoms.
booleanLattice :: Int -> [Statement]
booleanLattice numAtoms =
    let
        rawAtoms = [0..(numAtoms-1)]
        rawStatements = (S.toList . S.powerSet . S.fromList) rawAtoms
    in
        map Statement rawStatements

