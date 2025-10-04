{-|
Module      : Laws.Thermal
Description : Thermal information processing law (Landauer's principle)
Copyright   : (c) 2024
License     : MIT
Maintainer  : modularphysics@example.com
Stability   : experimental
Portability : portable

Law II: Thermal Conversion
The minimum energy required to process (compute) or erase I bits 
of information at temperature T is:
E_proc/erase ≥ kT ln 2 · I
-}
module Laws.Thermal where

import Core.Constants
import Core.Information

-- | Landauer's principle: Minimum energy to erase information
-- E ≥ kT ln 2 · I
landauerEnergy :: Temperature -> Bits -> Energy
landauerEnergy temp bits = boltzmann * temp * ln2 * bits

-- | Energy per bit at given temperature
energyPerBit :: Temperature -> Energy
energyPerBit temp = boltzmann * temp * ln2

-- | Maximum erasable bits given energy budget
maxErasableBits :: Energy -> Temperature -> Bits
maxErasableBits energy temp = energy / (boltzmann * temp * ln2)

-- | Reversible vs irreversible computation energy
data ComputationType = Reversible | Irreversible

-- | Energy cost for computation
computationEnergy :: ComputationType -> Temperature -> Bits -> Energy
computationEnergy Reversible _ _ = 0  -- Ideal reversible computation
computationEnergy Irreversible temp bits = landauerEnergy temp bits

-- | Thermal efficiency of information processing
thermalEfficiency :: Energy -> Temperature -> Bits -> Double
thermalEfficiency actualEnergy temp bits =
    let minEnergy = landauerEnergy temp bits
    in minEnergy / actualEnergy

-- | Room temperature (standard conditions)
roomTemperature :: Temperature
roomTemperature = 298.15  -- Kelvin

-- | Energy cost at room temperature
roomTempEnergyPerBit :: Energy
roomTempEnergyPerBit = energyPerBit roomTemperature

-- | Check if process violates Landauer limit
isThermodynamicallyValid :: Energy -> Temperature -> Bits -> Bool
isThermodynamicallyValid energy temp bits = 
    energy >= landauerEnergy temp bits

-- | Entropy change from information erasure
entropyChange :: Bits -> Double
entropyChange bits = boltzmann * ln2 * bits

-- | Heat dissipation from information erasure
heatDissipation :: Temperature -> Bits -> Energy
heatDissipation = landauerEnergy  -- Same as Landauer energy