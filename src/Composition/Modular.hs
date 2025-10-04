{-|
Module      : Composition.Modular
Description : Framework for composing physical laws
Copyright   : (c) 2024
License     : MIT
Maintainer  : modularphysics@example.com
Stability   : experimental
Portability : portable

This module provides the compositional framework for combining
independent physical laws. Each law acts as a constraint, and
the physical behavior emerges from their composition.
-}
module Composition.Modular where

import Core.Constants
import Core.Information
import Laws.SizeAware
import Laws.Thermal

-- | A physical constraint represented as a function
type Constraint = Bits -> Energy

-- | Compose multiple constraints by taking the maximum
-- (most restrictive constraint dominates)
composeConstraints :: [Constraint] -> Bits -> Energy
composeConstraints constraints bits = 
    maximum $ map (\c -> c bits) constraints

-- | Physical regime classification
data Regime = Classical 
            | Quantum 
            | Relativistic 
            | Gravitational 
            | Mixed [Regime]
            deriving (Show, Eq)

-- | Detect dominant physical regime
detectRegime :: Energy -> Temperature -> Length -> Regime
detectRegime energy temp charLength
    | quantumScale > thermalScale && relativisticParam < 0.1 = Quantum
    | thermalScale > quantumScale && relativisticParam < 0.1 = Classical
    | relativisticParam > 0.5 && gravitationalParam < 0.1 = Relativistic
    | gravitationalParam > 0.5 = Gravitational
    | otherwise = Mixed $ filter (/= Mixed []) 
        [if quantumScale > 0.1 then Quantum else Classical,
         if relativisticParam > 0.1 then Relativistic else Classical,
         if gravitationalParam > 0.1 then Gravitational else Classical]
  where
    quantumScale = hbar * speedOfLight / (charLength * boltzmann * temp)
    thermalScale = boltzmann * temp / (hbar * speedOfLight / charLength)
    relativisticParam = energy / (planckMass * speedOfLight * speedOfLight)
    gravitationalParam = gravitationalConstant * (energy / (speedOfLight * speedOfLight)) / 
                        (charLength * speedOfLight * speedOfLight)

-- | Apply appropriate laws based on regime
applyLaws :: Regime -> Bits -> Temperature -> Length -> Energy
applyLaws Classical bits temp _ = landauerEnergy temp bits
applyLaws Quantum bits _ radius = sizeAwareEnergy bits radius
applyLaws Relativistic bits _ radius = sizeAwareEnergy bits radius * 
    sqrt (1 + (sizeAwareEnergy bits radius / (planckMass * speedOfLight * speedOfLight)))
applyLaws Gravitational bits _ radius = 
    let baseEnergy = sizeAwareEnergy bits radius
        infoMass = sizeAwareMass bits radius
        schwarzschildRadius = 2 * gravitationalConstant * infoMass / (speedOfLight * speedOfLight)
    in if radius < schwarzschildRadius 
       then baseEnergy * (radius / schwarzschildRadius)  -- Inside black hole
       else baseEnergy
applyLaws (Mixed regimes) bits temp radius =
    let constraints = map (\r -> applyLaws r bits temp radius) regimes
    in maximum constraints  -- Most restrictive applies

-- | Combined energy requirement considering all laws
totalEnergyRequirement :: Bits -> Temperature -> Length -> Energy
totalEnergyRequirement bits temp radius =
    let thermalReq = landauerEnergy temp bits
        sizeReq = sizeAwareEnergy bits radius
        regime = detectRegime (max thermalReq sizeReq) temp radius
    in applyLaws regime bits temp radius

-- | Check if configuration satisfies all constraints
isPhysicallyValid :: Bits -> Energy -> Temperature -> Length -> Bool
isPhysicallyValid bits energy temp radius =
    energy >= totalEnergyRequirement bits temp radius