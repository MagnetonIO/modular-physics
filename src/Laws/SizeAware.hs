{-|
Module      : Laws.SizeAware
Description : Size-aware energy conversion law
Copyright   : (c) 2024
License     : MIT
Maintainer  : modularphysics@example.com
Stability   : experimental
Portability : portable

Law I: Size-Aware Conversion
For any information content I (measured in bits) physically realized 
within a characteristic length scale R, the minimum energy requirement is:
E ≥ (ℏc ln 2)/(2πkR) · I
-}
module Laws.SizeAware where

import Core.Constants
import Core.Information

-- | Calculate minimum energy for information at given length scale
-- E ≥ (ℏc ln 2)/(2πkR) · I
sizeAwareEnergy :: Bits -> Length -> Energy
sizeAwareEnergy bits radius = 
    (hbar * speedOfLight * ln2) / (2 * pi * boltzmann * radius) * bits

-- | Calculate minimum mass for information at given length scale
-- M ≥ (ℏ ln 2)/(2πkRc) · I
sizeAwareMass :: Bits -> Length -> Double
sizeAwareMass bits radius =
    (hbar * ln2) / (2 * pi * boltzmann * radius * speedOfLight) * bits

-- | Margolus-Levitin bound: Maximum computation rate
-- τ ≥ πℏ/(2E)
margolusLevitinTime :: Energy -> Double
margolusLevitinTime energy = (pi * hbar) / (2 * energy)

-- | Maximum operations per second given energy
maxOperationsPerSecond :: Energy -> Double
maxOperationsPerSecond energy = 1.0 / margolusLevitinTime energy

-- | Bekenstein bound: Maximum information for given energy and size
-- S ≤ 2πkER/(ℏc)
bekensteinBound :: Energy -> Length -> Bits
bekensteinBound energy radius =
    (2 * pi * boltzmann * energy * radius) / (hbar * speedOfLight * ln2)

-- | Check if configuration violates size-aware bounds
isValidConfiguration :: Bits -> Energy -> Length -> Bool
isValidConfiguration bits energy radius =
    energy >= sizeAwareEnergy bits radius &&
    bits <= bekensteinBound energy radius

-- | Energy density from size-aware conversion
sizeAwareEnergyDensity :: InformationDensity -> Length -> Double
sizeAwareEnergyDensity infoDensity charLength =
    let volume = (4/3) * pi * charLength ** 3
        totalBits = infoDensity * volume
        energy = sizeAwareEnergy totalBits charLength
    in energy / volume