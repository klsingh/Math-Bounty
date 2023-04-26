# Math-Bounty
## A Plutus smart contract that locks deposited funds under a math bounty

This is a Plutus smart contract that locks deposited funds under a math bounty. The bounty requires finding a number whose square matches a given value.

Let's go through the code line by line to understand how it works:
```
-- Define the module name and language version
module MathBounty where

import           PlutusTx.Prelude
import           Ledger
```
This code defines the module name and language version, and imports the required modules from Plutus.
```
-- Define the data types for the smart contract
data MathBountyDatum = MathBountyDatum
    { targetSquare :: Integer
    , solved       :: Bool
    }
    deriving Show

data MathBountyRedeemer = SolveMathBounty Integer
```
This code defines the data types for the smart contract. MathBountyDatum is the datum type, which contains the target square and a boolean flag indicating whether the bounty has been solved or not. MathBountyRedeemer is the redeemer type, which contains the number whose square is being submitted to solve the bounty.
```
-- Define the validator script for the smart contract
validateMathBounty :: MathBountyDatum -> MathBountyRedeemer -> ScriptContext -> Bool
validateMathBounty md (SolveMathBounty n) _ = 
    not (solved md) && (n * n == targetSquare md)

validateMathBounty _ _ _ = False
```
This code defines the validator script for the smart contract. It takes in the MathBountyDatum, MathBountyRedeemer, and ScriptContext as inputs, and returns a boolean indicating whether the transaction is valid or not. The validateMathBounty function checks whether the bounty has not been solved yet and whether the submitted number's square matches the target square.
```
-- Define the monetary policy for the smart contract
mathBountyPolicy :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
mathBountyPolicy _ _ _ _ = True
```
This code defines the monetary policy for the smart contract. It takes in the CurrencySymbol, TokenName, () (an empty unit value), and ScriptContext as inputs, and always returns True. This means that any transaction that spends tokens of the specified CurrencySymbol and TokenName is allowed.
```
-- Define the smart contract endpoint for solving the math bounty
solveMathBounty :: AsContractError e => Integer -> Contract w s e ()
solveMathBounty n = do
    pkh <- pubKeyHash <$> ownPubKey
    utxos <- utxoAt (pubKeyHashAddress pkh)
    let orefs   = fst <$> Map.toList utxos
        tx      = foldr (\oref -> collectFromScript oref $ Redeemer $ toData $ SolveMathBounty n) emptyTransaction orefs
        script  = validatorScript $ unspentOutputs utxos
        tx'     = addScriptInputOutput tx script (unitValue policy)
    ledgerTx <- submitTxConstraintsSpending policy utxos tx'
    void $ awaitTxConfirmed $ txId ledgerTx
  where
    policy = scriptPolicy $ validatorScript $ unspentOutputs utxos
```
This code defines the smart contract endpoint for solving the math bounty. It takes in an Integer representing the number whose square is being submitted to solve the bounty. The solveMathBounty function first retrieves the public key hash of the current user, and then retrieves all unspent transaction outputs (UTXOs) associated with that.
