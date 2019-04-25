module Main where

-- Imports
import KnuSkillz.Statement

-- Main action
main :: IO ()
main = do
    let statements = booleanLattice 3
    print statements

