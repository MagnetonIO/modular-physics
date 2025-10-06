{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Core.Constants
import Core.Information  
import Laws.SizeAware
import Laws.Thermal
import System.Exit (exitFailure, exitSuccess)

-- Helper for approximate equality
approxEqual :: Double -> Double -> Bool
approxEqual x y = abs((x - y) / max (abs x) (abs y)) < 1e-10

-- Test Suite
tests :: Test
tests = TestList
    [ TestLabel "Size-Aware Energy Law" testSizeAware
    , TestLabel "Bekenstein Bound" testBekenstein
    , TestLabel "Landauer Principle" testLandauer
    , TestLabel "Mass-Energy Equivalence" testMassEnergy
    , TestLabel "Scale Invariance" testScaleInvariance
    ]

testSizeAware :: Test
testSizeAware = TestCase $ do
    let bits = 1000.0
        radius = 1e-9
        energy = sizeAwareEnergy bits radius
        expected = (hbar * speedOfLight * ln2) / (2 * pi * boltzmann * radius) * bits
    assertBool "Size-aware energy calculation" (approxEqual energy expected)

testBekenstein :: Test  
testBekenstein = TestCase $ do
    let energy = 1.0
        radius = 0.01
        maxBits = bekensteinBound energy radius
        validBelow = isValidConfiguration (maxBits * 0.99) energy radius
        invalidAbove = isValidConfiguration (maxBits * 1.01) energy radius
    assertBool "Bekenstein bound respected" (validBelow && not invalidAbove)

testLandauer :: Test
testLandauer = TestCase $ do
    let temp = 300.0
        bits = 1.0
        minEnergy = landauerEnergy temp bits
        expected = boltzmann * temp * ln2
    assertBool "Landauer principle" (approxEqual minEnergy expected)

testMassEnergy :: Test
testMassEnergy = TestCase $ do
    let bits = 1e6
        radius = 1e-6
        energy = sizeAwareEnergy bits radius
        mass = sizeAwareMass bits radius
        energyFromMass = mass * speedOfLight * speedOfLight
    assertBool "E=mc² consistency" (approxEqual energy energyFromMass)

testScaleInvariance :: Test
testScaleInvariance = TestCase $ do
    let bits = 1000.0
        r1 = 1e-6
        r2 = 1e-3
        e1 = sizeAwareEnergy bits r1
        e2 = sizeAwareEnergy bits r2
        ratio = e1 / e2
        expectedRatio = r2 / r1
    assertBool "Energy scales as 1/R" (approxEqual ratio expectedRatio)

main :: IO ()
main = do
    putStrLn "Running Modular Physics Tests"
    putStrLn "=============================="
    results <- runTestTT tests
    if errors results + failures results == 0
        then do
            putStrLn "\nAll tests passed!"
            exitSuccess
        else do
            putStrLn "\nSome tests failed"
            exitFailure