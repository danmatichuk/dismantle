module Dismantle.X86.ISA () where

{- Notes [TableGen Format]

The x86 tablegen data is considerably different from the other architectures.
It is a bit more regular and is simple in its own way.

 * There are useful predicates: Not64BitMode
 * Uses and Defs list implicit operands
 * There are no instruction bit patterns laying out operands, as x86 operand structure is regular
 * There are opcode listings
 * The AsmString formatting is more complex
 * There is some OperandSize information that might help figure out the size override prefix (also OpSizeBits?)
 * There are things like hasREX_w_prefix
 * There is opcode overlap between the size variants, with prefixes used to disambiguate
 * Instruction variants are broken apart with suffixes
 * We'll want a relaxed decoder (i.e., one that accepts more prefixes than are strictly allowed)

The operands are pretty expressive:

 * There are immediates (e.g., i16imm)
 * There are immediates that get sign extended (e.g., i16i8imm is an 8 bit immediate sign extended to 16 bits)
 * There are register references (e.g., GR16 is a 16 bit slice of a GPR)
 * There are mixed mem/reg references that can be either a memory reference or a register
   (e.g., i16mem is a memory reference to a 16 bit memory value OR is a 16 bit register)
   The selection is based on the ModRM/SIB bytes.



-}
