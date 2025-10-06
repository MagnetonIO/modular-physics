module Main where

import Core.Constants
import Core.Information  
import Laws.SizeAware
import Laws.Thermal
import System.Exit (exitFailure, exitSuccess)
import Text.Printf (printf)

-- Test result type
data TestResult = Pass | Fail String

-- Helper for approximate equality
approxEqual :: Double -> Double -> Bool
approxEqual x y 
    | abs x < 1e-30 && abs y < 1e-30 = True
    | otherwise = abs((x - y) / max (abs x) (abs y)) < 1e-10

-- Test runner
runTest :: String -> IO TestResult -> IO Bool
runTest name test = do
    result <- test
    case result of
        Pass -> do
            putStrLn $ "✓ " ++ name
            return True
        Fail msg -> do
            putStrLn $ "✗ " ++ name ++ ": " ++ msg
            return False

-- Tests
testSizeAware :: IO TestResult
testSizeAware = do
    let bits = 1000.0
        radius = 1e-9
        energy = sizeAwareEnergy bits radius
        expected = (hbar * speedOfLight * ln2) / (2 * pi * boltzmann * radius) * bits
    return $ if approxEqual energy expected 
             then Pass 
             else Fail $ printf "Expected %.3e, got %.3e" expected energy

testBekenstein :: IO TestResult  
testBekenstein = do
    let energy = 1.0
        radius = 0.01
        maxBits = bekensteinBound energy radius
        validBelow = isValidConfiguration (maxBits * 0.99) energy radius
        invalidAbove = isValidConfiguration (maxBits * 1.01) energy radius
    return $ if validBelow && not invalidAbove
             then Pass
             else Fail "Bekenstein bound violation"

testLandauer :: IO TestResult
testLandauer = do
    let temp = 300.0
        bits = 1.0
        minEnergy = landauerEnergy temp bits
        expected = boltzmann * temp * ln2
    return $ if approxEqual minEnergy expected
             then Pass
             else Fail $ printf "Expected %.3e, got %.3e" expected minEnergy

testMassEnergy :: IO TestResult
testMassEnergy = do
    let bits = 1e6
        radius = 1e-6
        energy = sizeAwareEnergy bits radius
        mass = sizeAwareMass bits radius
        energyFromMass = mass * speedOfLight * speedOfLight
    return $ if approxEqual energy energyFromMass
             then Pass
             else Fail $ printf "E=%.3e, mc²=%.3e" energy energyFromMass

testScaleInvariance :: IO TestResult
testScaleInvariance = do
    let bits = 1000.0
        r1 = 1e-6
        r2 = 1e-3
        e1 = sizeAwareEnergy bits r1
        e2 = sizeAwareEnergy bits r2
        ratio = e1 / e2
        expectedRatio = r2 / r1
    return $ if approxEqual ratio expectedRatio
             then Pass
             else Fail $ printf "Ratio %.3f, expected %.3f" ratio expectedRatio

testMargolusLevitin :: IO TestResult
testMargolusLevitin = do
    let energy = 1e-18
        time = margolusLevitinTime energy
        expectedTime = (pi * hbar) / (2 * energy)
    return $ if approxEqual time expectedTime
             then Pass
             else Fail $ printf "Time %.3e, expected %.3e" time expectedTime

testThermalRegime :: IO TestResult
testThermalRegime = do
    let temp = 300.0  -- Room temperature
        bits = 1.0
        -- At meter scale, thermal should dominate
        largeRadius = 1.0  -- 1 meter - definitely thermal regime
        thermalEnergy = landauerEnergy temp bits
        sizeEnergy = sizeAwareEnergy bits largeRadius
    return $ if thermalEnergy > sizeEnergy
             then Pass
             else Fail $ printf "Thermal %.3e should be > Size %.3e at large scales" thermalEnergy sizeEnergy

testQuantumRegime :: IO TestResult
testQuantumRegime = do
    let temp = 4.2  -- Liquid helium
        bits = 1.0
        -- At small scale, quantum should dominate
        smallRadius = 1e-10
        thermalEnergy = landauerEnergy temp bits
        sizeEnergy = sizeAwareEnergy bits smallRadius
    return $ if sizeEnergy > thermalEnergy
             then Pass
             else Fail "Quantum should dominate at small scales"

main :: IO ()
main = do
    putStrLn "\n================================"
    putStrLn "  Modular Physics Test Suite"
    putStrLn "================================\n"
    
    let tests = 
            [ ("Size-Aware Energy Law", testSizeAware)
            , ("Bekenstein Bound", testBekenstein)
            , ("Landauer's Principle", testLandauer)
            , ("Mass-Energy Equivalence", testMassEnergy)
            , ("Scale Invariance", testScaleInvariance)
            , ("Margolus-Levitin Bound", testMargolusLevitin)
            , ("Quantum Regime Dominance", testQuantumRegime)
            ]
    
    results <- mapM (uncurry runTest) tests
    
    let passed = length (filter id results)
        total = length tests
        
    putStrLn $ "\n================================"
    printf "Results: %d/%d tests passed\n" passed total
    putStrLn "================================\n"
    
    if passed == total
        then do
            putStrLn "✅ All tests passed!"
            exitSuccess
        else do
            putStrLn "❌ Some tests failed"
            exitFailure