{-|
Module      : Validation.SizeAwareValidation
Description : Comprehensive validation of size-aware energy conversion laws
Copyright   : (c) 2024
License     : MIT
Maintainer  : modularphysics@example.com
Stability   : experimental
Portability : portable

This module provides comprehensive validation tests for all functions
described in the ArXiv paper on size-aware energy conversion.
-}
module Validation.SizeAwareValidation where

import Core.Constants
import Core.Information
import Laws.SizeAware
import Laws.Thermal
import Control.Monad (forM, forM_, when, unless)
import Text.Printf (printf)
import System.Exit (exitFailure, exitSuccess)

-- Test configuration
data TestResult = Pass | Fail String deriving (Eq)

instance Show TestResult where
    show Pass = "PASS"
    show (Fail msg) = "FAIL: " ++ msg

-- Tolerance for floating point comparisons
epsilon :: Double
epsilon = 1e-10

-- Helper function for approximate equality
approxEqual :: Double -> Double -> Bool
approxEqual x y 
    | abs x < epsilon && abs y < epsilon = True
    | otherwise = abs((x - y) / max (abs x) (abs y)) < epsilon

-- Test 1: Verify size-aware energy law
testSizeAwareEnergyLaw :: IO TestResult
testSizeAwareEnergyLaw = do
    let testCases = [
            (1.0, 1e-9, "Single bit at nanometer scale"),
            (1e6, 1e-3, "Megabit at millimeter scale"),
            (1e9, 1.0, "Gigabit at meter scale")
            ]
    
    results <- forM testCases $ \(bits, radius, desc) -> do
        let energy = sizeAwareEnergy bits radius
            expected = (hbar * speedOfLight * ln2) / (2 * pi * boltzmann * radius) * bits
        if approxEqual energy expected
            then return Pass
            else return $ Fail $ printf "%s: Got %.3e, expected %.3e" desc energy expected
    
    return $ if all (== Pass) results then Pass else head [r | r@(Fail _) <- results]

-- Test 2: Verify mass-energy equivalence
testMassEnergyEquivalence :: IO TestResult
testMassEnergyEquivalence = do
    let bits = 1e6
        radius = 1e-6
        energy = sizeAwareEnergy bits radius
        mass = sizeAwareMass bits radius
        energyFromMass = mass * speedOfLight * speedOfLight
    
    if approxEqual energy energyFromMass
        then return Pass
        else return $ Fail $ printf "E=mc^2 violation: E=%.3e, mc^2=%.3e" energy energyFromMass

-- Test 3: Verify Bekenstein bound
testBekensteinBound :: IO TestResult
testBekensteinBound = do
    let energy = 1.0  -- 1 Joule
        radius = 0.01  -- 1 cm
        maxBits = bekensteinBound energy radius
        
        -- Test just below limit (should be valid)
        testBits1 = maxBits * 0.99
        valid1 = isValidConfiguration testBits1 energy radius
        
        -- Test above limit (should be invalid)
        testBits2 = maxBits * 1.01
        valid2 = isValidConfiguration testBits2 energy radius
    
    if valid1 && not valid2
        then return Pass
        else return $ Fail $ printf "Bekenstein bound check failed: below=%.3e (%s), above=%.3e (%s)" 
                            testBits1 (show valid1) testBits2 (show valid2)

-- Test 4: Verify Margolus-Levitin bound
testMargolusLevitin :: IO TestResult
testMargolusLevitin = do
    let energy = 1e-18  -- Small energy
        time = margolusLevitinTime energy
        expectedTime = (pi * hbar) / (2 * energy)
        opsPerSec = maxOperationsPerSecond energy
        expectedOps = 1.0 / expectedTime
    
    if approxEqual time expectedTime && approxEqual opsPerSec expectedOps
        then return Pass
        else return $ Fail $ printf "ML bound: time=%.3e (expected %.3e), ops=%.3e (expected %.3e)"
                            time expectedTime opsPerSec expectedOps

-- Test 5: Verify Landauer's principle
testLandauerPrinciple :: IO TestResult
testLandauerPrinciple = do
    let testTemperatures = [4.2, 77.0, 300.0]  -- Liquid He, liquid N2, room temp
    
    results <- forM testTemperatures $ \temp -> do
        let bits = 1.0
            energy = landauerEnergy temp bits
            expected = boltzmann * temp * ln2
        if approxEqual energy expected
            then return Pass
            else return $ Fail $ printf "At T=%.1fK: got %.3e, expected %.3e" temp energy expected
    
    return $ if all (== Pass) results then Pass else head [r | r@(Fail _) <- results]

-- Test 6: Verify scale invariance
testScaleInvariance :: IO TestResult
testScaleInvariance = do
    let bits = 1000.0
        scales = [(1e-9, 1e-6), (1e-6, 1e-3), (1e-3, 1.0)]
    
    results <- forM scales $ \(r1, r2) -> do
        let e1 = sizeAwareEnergy bits r1
            e2 = sizeAwareEnergy bits r2
            ratio = e1 / e2
            expectedRatio = r2 / r1
        if approxEqual ratio expectedRatio
            then return Pass
            else return $ Fail $ printf "Scale %.0e to %.0e: ratio %.3f, expected %.3f" 
                                r1 r2 ratio expectedRatio
    
    return $ if all (== Pass) results then Pass else head [r | r@(Fail _) <- results]

-- Test 7: Verify holographic bound
testHolographicBound :: IO TestResult
testHolographicBound = do
    let radius = 1e-3  -- 1 mm sphere
        surfaceArea = 4 * pi * radius * radius
        -- Note: holographicBound is not in the original modules, so we calculate it here
        planckLength = sqrt (hbar * gravitationalConstant / (speedOfLight ** 3))
        maxBits = surfaceArea / (4 * planckLength * planckLength * ln2)
        
        -- Energy that would create a black hole at this radius
        blackHoleEnergy = (speedOfLight ** 4 * radius) / (2 * gravitationalConstant)
        
        -- Information from Bekenstein bound should match holographic bound
        bekensteinBits = bekensteinBound blackHoleEnergy radius
        
    -- They should be approximately equal for a black hole
    if abs(bekensteinBits / maxBits - 1.0) < 0.1  -- Within 10%
        then return Pass
        else return $ Fail $ printf "Holographic: %.3e bits, Bekenstein: %.3e bits (ratio %.3f)"
                            maxBits bekensteinBits (bekensteinBits / maxBits)

-- Test 8: Verify thermal vs quantum regime transition
testRegimeTransition :: IO TestResult
testRegimeTransition = do
    let temp = 300.0  -- Room temperature
        -- Critical radius where thermal = quantum
        criticalRadius = (hbar * speedOfLight) / (2 * pi * boltzmann * temp)
        
        bits = 1.0
        
        -- Below critical radius: quantum dominates
        r1 = criticalRadius * 0.1
        quantumEnergy = sizeAwareEnergy bits r1
        thermalEnergy1 = landauerEnergy temp bits
        quantumDominates = quantumEnergy > thermalEnergy1
        
        -- Above critical radius: thermal dominates  
        r2 = criticalRadius * 10.0
        quantumEnergy2 = sizeAwareEnergy bits r2
        thermalEnergy2 = landauerEnergy temp bits
        thermalDominates = quantumEnergy2 < thermalEnergy2
        
    if quantumDominates && thermalDominates
        then return Pass
        else return $ Fail $ printf "Regime transition failed: quantum@%.3e=%s, thermal@%.3e=%s"
                            r1 (show quantumDominates) r2 (show thermalDominates)

-- Test 9: Energy density consistency
testEnergyDensity :: IO TestResult
testEnergyDensity = do
    let bits = 1e12  -- 1 terabit
        radius = 0.001  -- 1 mm
        density = sizeAwareEnergyDensity bits radius
        
        -- Calculate expected density
        volume = (4.0/3.0) * pi * radius ** 3
        totalEnergy = sizeAwareEnergy bits radius
        expectedDensity = totalEnergy / volume
        
    if approxEqual density expectedDensity
        then return Pass
        else return $ Fail $ printf "Energy density: got %.3e, expected %.3e" density expectedDensity

-- Test 10: Reversible vs irreversible computation
testComputationTypes :: IO TestResult
testComputationTypes = do
    let temp = 300.0
        bits = 100.0
        
        reversibleEnergy = computationEnergy Reversible temp bits
        irreversibleEnergy = computationEnergy Irreversible temp bits
        landauerMin = landauerEnergy temp bits
        
        reversibleCorrect = reversibleEnergy == 0.0
        irreversibleCorrect = approxEqual irreversibleEnergy landauerMin
        
    if reversibleCorrect && irreversibleCorrect
        then return Pass
        else return $ Fail $ printf "Reversible=%.3e (should be 0), Irreversible=%.3e (should be %.3e)"
                            reversibleEnergy irreversibleEnergy landauerMin

-- Main validation suite
runAllTests :: IO ()
runAllTests = do
    putStrLn "================================"
    putStrLn "Size-Aware Energy Law Validation"
    putStrLn "================================\n"
    
    let tests = [
            ("Size-Aware Energy Law", testSizeAwareEnergyLaw),
            ("Mass-Energy Equivalence", testMassEnergyEquivalence),
            ("Bekenstein Bound", testBekensteinBound),
            ("Margolus-Levitin Bound", testMargolusLevitin),
            ("Landauer's Principle", testLandauerPrinciple),
            ("Scale Invariance", testScaleInvariance),
            ("Holographic Bound", testHolographicBound),
            ("Regime Transition", testRegimeTransition),
            ("Energy Density", testEnergyDensity),
            ("Computation Types", testComputationTypes)
            ]
    
    results <- forM tests $ \(name, test) -> do
        result <- test
        printf "%-25s: %s\n" name (show result)
        return (name, result)
    
    let failures = [(n, r) | (n, r@(Fail _)) <- results]
        passCount = length results - length failures
        
    putStrLn $ "\n================================"
    printf "Results: %d/%d tests passed\n" passCount (length results)
    
    unless (null failures) $ do
        putStrLn "\nFailed tests:"
        forM_ failures $ \(name, Fail msg) -> 
            printf "  %s: %s\n" name msg
    
    putStrLn "================================"
    
    if null failures
        then do
            putStrLn "\nAll Haskell functions validated successfully!"
            putStrLn "The implementations accurately represent the physical laws."
            exitSuccess
        else do
            putStrLn "\nSome tests failed. Please review the implementations."
            exitFailure

-- Additional validation: Physical constants accuracy
validateConstants :: IO ()
validateConstants = do
    putStrLn "\nPhysical Constants Validation:"
    putStrLn "------------------------------"
    
    -- Check Planck scales
    let planckLength = sqrt (hbar * gravitationalConstant / (speedOfLight ** 3))
        planckTime = sqrt (hbar * gravitationalConstant / (speedOfLight ** 5))
        planckMass = sqrt (hbar * speedOfLight / gravitationalConstant)
        planckEnergy = planckMass * speedOfLight * speedOfLight
        
    printf "Planck length: %.3e m\n" planckLength
    printf "Planck time: %.3e s\n" planckTime
    printf "Planck mass: %.3e kg\n" planckMass
    printf "Planck energy: %.3e J\n" planckEnergy
    
    -- Verify relationships
    let c_calc = planckLength / planckTime
        c_error = abs(c_calc - speedOfLight) / speedOfLight
        
    printf "\nConsistency check: c = %.3e m/s (error: %.2e%%)\n" c_calc (c_error * 100)
    
    if c_error < 1e-10
        then putStrLn "Constants are self-consistent ✓"
        else putStrLn "WARNING: Constants show inconsistency!"

-- Run validation with detailed output
main :: IO ()
main = do
    validateConstants
    putStrLn ""
    runAllTests