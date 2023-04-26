-- | A smart contract that locks deposited funds under a math bounty.
-- | The bounty requires finding a number whose square matches a given value.

-- Define the module name and language version
module MathBounty where

import           PlutusTx.Prelude
import           Ledger

-- Define the data types for the smart contract
data MathBountyDatum = MathBountyDatum
    { targetSquare :: Integer
    , solved       :: Bool
    }
    deriving Show

data MathBountyRedeemer = SolveMathBounty Integer

-- Define the validator script for the smart contract
validateMathBounty :: MathBountyDatum -> MathBountyRedeemer -> ScriptContext -> Bool
validateMathBounty md (SolveMathBounty n) _ = 
    not (solved md) && (n * n == targetSquare md)

validateMathBounty _ _ _ = False

-- Define the monetary policy for the smart contract
mathBountyPolicy :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
mathBountyPolicy _ _ _ _ = True

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

-- Define the Plutus contract with the MathBountyValidator as the validator
mathBountyContract :: AsContractError e => Contract () MathBountySchema e ()
mathBountyContract = do
    pkh <- pubKeyHash <$> ownPubKey
    let mathBounty = AssetClass (currencySymbol pkh) "MathBounty"
    utxos <- utxoAt $ pubKeyHashAddress pkh
    let orefs   = fst <$> Map.toList (Map.filter (== mathBounty) $ txOutType <$> utxos)
        targets = fmap (\oref -> let txout = txOutTxOut (fst oref) in targetSquare $ fromJust $ findDatum (datumHash txout) $ txOutTxOutDatum txout) orefs
    if null targets
        then logInfo @String "No MathBounties found"
        else do
            logInfo @String $ "Found " ++ show (length targets) ++ " MathBounties"
            mapM_ (solveMathBounty . round . sqrt . fromIntegral) targets

-- Define the Plutus schema for the smart contract
type MathBountySchema =
    BlockchainActions
        .\/ Endpoint "solve" Integer

-- Define the Plutus contract instance for the smart contract
mkMathBountyInstance :: AsContractError e => Contract () MathBountySchema e ()
mkMathBountyInstance = do
    selectList [solveMathBounty'] >>= runSelected
  where
    solveMathBounty' =
        endpoint @"solve" solveMathBounty
