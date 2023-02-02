{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module GroupPayoutContract
  ( groupPayoutContractScript
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley                             ( PlutusScript (..)
                                                                 , PlutusScriptV2 )
import qualified Data.ByteString.Lazy                            as LBS
import qualified Data.ByteString.Short                           as SBS
import qualified Plutus.V2.Ledger.Contexts                       as V2
import qualified Plutus.V2.Ledger.Api                            as V2
import           UsefulFuncs (adaValue, createAddress)
import           Plutonomy
{- |
  Author   : The Ancient Kraken
  Copyright: 2023
-}
-------------------------------------------------------------------------------
-- | Create the datum parameters data object.
-------------------------------------------------------------------------------
data CustomDatumType = CustomDatumType
  { pkhs :: [V2.PubKeyHash]
  -- ^ The public key hahses
  , scs  :: [V2.PubKeyHash]
  -- ^ The staking credentials
  , amts :: [Integer]
  -- ^ The lovelace amounts
  }
PlutusTx.unstableMakeIsData ''CustomDatumType
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = CustomRedeemerType {}
PlutusTx.unstableMakeIsData ''CustomRedeemerType
-------------------------------------------------------------------------------
-- | Reduced Script Context
-------------------------------------------------------------------------------
data PayoutOutputDatum =  NoOutputDatum | OutputDatum V2.Datum
PlutusTx.makeIsDataIndexed ''PayoutOutputDatum [('NoOutputDatum, 0), ('OutputDatum, 2)]

data PayoutTxOut = PayoutTxOut
  { txOutAddress         :: V2.Address
  , txOutValue           :: V2.Value
  , txOutDatum           :: PayoutOutputDatum
  , txOutReferenceScript :: BuiltinData
  }
PlutusTx.unstableMakeIsData ''PayoutTxOut

data PayoutTxInInfo = PayoutTxInInfo
    { txInInfoOutRef   :: V2.TxOutRef
    , txInInfoResolved :: PayoutTxOut
    } 
PlutusTx.unstableMakeIsData ''PayoutTxInInfo

data PayoutTxInfo = PayoutTxInfo
    { txInfoInputs          :: [PayoutTxInInfo] -- Transaction inputs
    , txInfoReferenceInputs :: BuiltinData
    , txInfoOutputs         :: [PayoutTxOut]    -- Transaction outputs
    , txInfoFee             :: BuiltinData
    , txInfoMint            :: BuiltinData
    , txInfoDCert           :: BuiltinData
    , txInfoWdrl            :: BuiltinData
    , txInfoValidRange      :: BuiltinData
    , txInfoSignatories     :: BuiltinData
    , txInfoRedeemers       :: BuiltinData
    , txInfoData            :: BuiltinData
    , txInfoId              :: BuiltinData
    }
PlutusTx.unstableMakeIsData ''PayoutTxInfo

data PayoutScriptPurpose = Spending V2.TxOutRef
PlutusTx.makeIsDataIndexed ''PayoutScriptPurpose [('Spending, 1)]

data PayoutScriptContext = PayoutScriptContext
  { scriptContextTxInfo :: PayoutTxInfo
  , scriptContextPurpose ::  PayoutScriptPurpose 
  }
PlutusTx.unstableMakeIsData ''PayoutScriptContext
-------------------------------------------------------------------------------
-- | Reduced Functions
-------------------------------------------------------------------------------
{-# INLINABLE getScriptInput #-}
getScriptInput :: [PayoutTxInInfo] -> V2.TxOutRef -> PayoutTxOut
getScriptInput [] _ = traceError "script input not found"
getScriptInput ((PayoutTxInInfo tref ot) : xs) o_ref
  | tref == o_ref = ot
  | otherwise = getScriptInput xs o_ref

{-# INLINABLE ownInput #-}
ownInput :: PayoutScriptContext -> PayoutTxOut
ownInput (PayoutScriptContext t_info (Spending o_ref)) = getScriptInput (txInfoInputs t_info) o_ref

{-# INLINABLE checkAllPayouts #-}
checkAllPayouts :: CustomDatumType -> [PayoutTxOut] -> Bool
checkAllPayouts dat outs = checkAllPayouts' (pkhs dat) (scs dat) (amts dat)
  where
    checkAllPayouts' :: [V2.PubKeyHash] -> [V2.PubKeyHash] -> [Integer] -> Bool
    checkAllPayouts' [] [] [] = True
    checkAllPayouts' [] _ _ = False
    checkAllPayouts' _ [] _ = False
    checkAllPayouts' _ _ [] = False
    checkAllPayouts' (pkh:pkhs) (sc:scs) (amt:amts) =
      if findPayout outs addr val == True
        then checkAllPayouts' pkhs scs amts
        else False
      where
        addr :: V2.Address
        addr = createAddress pkh sc

        val :: V2.Value
        val = adaValue amt

{-# INLINABLE findPayout #-}
findPayout :: [PayoutTxOut] -> V2.Address -> V2.Value -> Bool
findPayout list addr val = helper list
  where
    helper :: [PayoutTxOut] -> Bool
    helper [] = False
    helper (x:xs)
      | checkAddr && checkVal = True
      | otherwise             = helper xs
      where
        checkAddr :: Bool
        checkAddr = txOutAddress x == addr

        checkVal :: Bool
        checkVal = txOutValue x == val

{-# INLINABLE nInputs #-}
nInputs :: [PayoutTxInInfo] -> V2.Address -> Integer -> Bool
nInputs utxos addr number = loopInputs utxos 0 0
  where
    loopInputs :: [PayoutTxInInfo] -> Integer -> Integer -> Bool
    loopInputs []     !dC !sC = (dC == number) && (sC == number)
    loopInputs (x:xs) !dC !sC = 
      case txOutDatum txInOut  of
        NoOutputDatum       -> loopInputs xs dC sC
        (OutputDatum _)     -> 
          if txOutAddress txInOut == addr
            then loopInputs xs (dC + 1) (sC + 1) -- inline
            else loopInputs xs (dC + 1) sC
      where 
        txInOut :: PayoutTxOut
        txInOut = txInInfoResolved x
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PayoutScriptContext -> Bool
mkValidator datum _ context =
  let !info       = scriptContextTxInfo context
      !txIns      = txInfoInputs info
      !txOuts     = txInfoOutputs info
      !validInput = ownInput context
      !scriptAddr = txOutAddress validInput
      in
         (checkAllPayouts datum txOuts)  -- Make sure every address is paid
      && (nInputs txIns scriptAddr 1  )  -- singular script input
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator x y z = check (mkValidator (V2.unsafeFromBuiltinData x) (V2.unsafeFromBuiltinData y) (V2.unsafeFromBuiltinData z))

validator :: V2.Validator
validator = Plutonomy.optimizeUPLCWith Plutonomy.aggressiveOptimizerOptions $ Plutonomy.validatorToPlutus $ Plutonomy.mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

groupPayoutContractScriptShortBs :: SBS.ShortByteString
groupPayoutContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise validator

groupPayoutContractScript :: PlutusScript PlutusScriptV2
groupPayoutContractScript = PlutusScriptSerialised groupPayoutContractScriptShortBs