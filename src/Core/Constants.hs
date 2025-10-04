{-|
Module      : Core.Constants
Description : Fundamental physical constants for modular physics
Copyright   : (c) 2024
License     : MIT
Maintainer  : modularphysics@example.com
Stability   : experimental
Portability : portable

This module provides fundamental physical constants used throughout
the modular physics framework. All constants are provided in SI units
for consistency and ease of translation to other languages.
-}
module Core.Constants where

-- | Speed of light in vacuum (m/s)
speedOfLight :: Double
speedOfLight = 299792458.0

-- | Reduced Planck constant (J⋅s)
hbar :: Double
hbar = 1.054571817e-34

-- | Boltzmann constant (J/K)
boltzmann :: Double
boltzmann = 1.380649e-23

-- | Gravitational constant (m³⋅kg⁻¹⋅s⁻²)
gravitationalConstant :: Double
gravitationalConstant = 6.67430e-11

-- | Natural logarithm of 2 (for bit conversions)
ln2 :: Double
ln2 = 0.6931471805599453

-- | Planck length (m)
planckLength :: Double
planckLength = sqrt (hbar * gravitationalConstant / (speedOfLight ** 3))

-- | Planck time (s)
planckTime :: Double
planckTime = sqrt (hbar * gravitationalConstant / (speedOfLight ** 5))

-- | Planck mass (kg)
planckMass :: Double
planckMass = sqrt (hbar * speedOfLight / gravitationalConstant)

-- | Planck energy (J)
planckEnergy :: Double
planckEnergy = planckMass * speedOfLight * speedOfLight

-- | Planck temperature (K)
planckTemperature :: Double
planckTemperature = planckEnergy / boltzmann

-- | Planck density (kg/m³)
planckDensity :: Double
planckDensity = planckMass / (planckLength ** 3)

-- | Information-mass coupling constant (dimensionless in natural units)
informationMassCoupling :: Double
informationMassCoupling = planckLength * planckLength / (hbar * speedOfLight)