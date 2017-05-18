module Main ( main ) where

import qualified Data.List as L
import qualified Test.Tasty as T
import qualified Text.RE.TDFA as RE

import Dismantle.Testing

import qualified Dismantle.PPC as PPC

ppc :: ArchTestConfig
ppc = ATC { archName = "ppc"
          , disassemble = PPC.disassembleInstruction
          , assemble = PPC.assembleInstruction
          , prettyPrint = PPC.ppInstruction
          , expectFailure = Nothing
          , skipPrettyCheck = Just skipPrettyRE
          , ignoreAddresses = []
          }

main :: IO ()
main = do
  tg <- binaryTestSuite ppc "tests/bin"
  T.defaultMain tg

-- | A regular expression matching any instruction that we shouldn't test for
-- pretty printing accuracy.
skipPrettyRE :: RE.RE
skipPrettyRE = rx (L.intercalate "|" rxes)
  where
    rxes = [ -- IP-relative addressing references names in <>, and we can't
             -- match those correctly yet
             "<"
           -- The OR instruction is aliased to MR if the destination
           -- is the same as the second register.  We don't have a
           -- definition for MR, so we skip validating it
           , "^[[:space:]]*mr\\.?[[:space:]]"
           -- CLRLWI is a specialized form of RLWINM, but we don't have a def
           -- for it
           , "^[[:space:]]*clrlwi[[:space:]]"
           -- NOT is rendered as NOR with three operands, but we don't have a
           -- NOT def
           , "^[[:space:]]*not[[:space:]]"
           -- We render two extra zero operands with MTFSF compared to objdump
           , "^[[:space:]]*mtfsf[[:space:]]"
           -- CRCL is an alias for CRXOR
           , "^[[:space:]]*crcl[[:space:]]"
           -- Objdump renders CMPWI without its first operand if it is cr0
           , "^[[:space:]]*cmpwi[[:space:]]"
           --  ROTLWI is an alias for RLWINM
           , "^[[:space:]]*rotlwi[[:space:]]"
           -- ROTLW is an alias for RLWNM
           , "^[[:space:]]*rotlw[[:space:]]"
           -- CRCLR is an alias of CRXOR
           , "^[[:space:]]*crclr[[:space:]]"
           -- BLELR is an alias of BCL
           , "^[[:space:]]*blelr[[:space:]]"
           -- BEQLR is an alias of BCL
           , "^[[:space:]]*beqlr[[:space:]]"
           -- BNELR is an alias for BCL
           , "^[[:space:]]*bnelr[[:space:]]"
           -- BLTLR is an alias for BCL
           , "^[[:space:]]*bltlr[[:space:]]"
           -- CRSET is an alias for CREQV
           , "^[[:space:]]*crset[[:space:]]"
           -- CRNOT is an alias for CRNOR
           , "^[[:space:]]*crnot[[:space:]]"
           -- LWSYNC is an alias for SYNC 1
           , "^[[:space:]]*lwsync"
           -- MFTBU and MFTB have implicit operands that our pretty printer
           -- shows, but objdump does not.
           , "^[[:space:]]*mftbu?[[:space:]]"

           -- FIXME: The following two instructions have incorrect operand specs
           -- in the tablegen data.  Investigate upstream
           , "^[[:space:]]*mtfsb0[[:space:]]"
           , "^[[:space:]]*mtfsb1[[:space:]]"
           ]

rx :: String -> RE.RE
rx s =
  case RE.compileRegex s of
    Nothing -> error ("Invalid regex: " ++ s)
    Just r -> r

