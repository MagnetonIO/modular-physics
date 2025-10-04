{-|
Module      : Core.Information
Description : Information density and measures for modular physics
Copyright   : (c) 2024
License     : MIT
Maintainer  : modularphysics@example.com
Stability   : experimental
Portability : portable

This module defines information density and related measures,
forming the foundation of the information-theoretic approach
to physics where E = Ic².
-}
module Core.Information where

import Core.Constants

-- | Information content in bits
type Bits = Double

-- | Information density (bits per cubic meter)
type InformationDensity = Double

-- | Volume in cubic meters
type Volume = Double

-- | Energy in Joules
type Energy = Double

-- | Temperature in Kelvin
type Temperature = Double

-- | Length scale in meters
type Length = Double

-- | Calculate information density from total information and volume
informationDensity :: Bits -> Volume -> InformationDensity
informationDensity totalBits volume = totalBits / volume

-- | Calculate total information from density and volume
totalInformation :: InformationDensity -> Volume -> Bits
totalInformation density volume = density * volume

-- | Shannon entropy for a probability distribution
shannonEntropy :: [Double] -> Bits
shannonEntropy probs = -sum [p * logBase 2 p | p <- probs, p > 0]

-- | Von Neumann entropy for a quantum density matrix (simplified)
-- Note: Full implementation would require matrix operations
vonNeumannEntropy :: [Double] -> Bits
vonNeumannEntropy eigenvalues = shannonEntropy eigenvalues

-- | Maximum information content for a region (holographic bound)
holographicBound :: Double -> Bits
holographicBound surfaceArea = surfaceArea / (4 * planckLength * planckLength * ln2)

-- | Information content of a black hole (Bekenstein-Hawking)
blackHoleInformation :: Double -> Bits
blackHoleInformation mass = 
    let radius = 2 * gravitationalConstant * mass / (speedOfLight * speedOfLight)
        area = 4 * pi * radius * radius
    in holographicBound area

-- | Check if information density saturates holographic bound
isSaturated :: InformationDensity -> Length -> Bool
isSaturated density radius =
    let volume = (4/3) * pi * radius ** 3
        surface = 4 * pi * radius ** 2
        totalInfo = density * volume
        maxInfo = holographicBound surface
    in totalInfo >= 0.99 * maxInfo  -- 99% saturation threshold

-- | Information flow rate (bits per second)
type InformationFlowRate = Double

-- | Calculate information flow from energy flow and temperature
informationFlow :: Energy -> Temperature -> InformationFlowRate
informationFlow energyFlow temp = energyFlow / (boltzmann * temp * ln2)