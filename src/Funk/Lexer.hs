{-# LANGUAGE CPP,MagicHash #-}
{-# LINE 15 "src/Funk/Lexer.x" #-}

module Funk.Lexer
       (
         lex,
         Token(..),
         Posn(..)
       ) where

import Prelude hiding (lex)

import Debug.Trace (trace)

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
#if __GLASGOW_HASKELL__ >= 503
import System.IO
import System.IO.Unsafe
import Debug.Trace
#else
import IO
import IOExts
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "templates/wrappers.hs" #-}
{-# LINE 1 "<command-line>" #-}






# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 6 "<command-line>" 2
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

import Data.Word (Word8)
{-# LINE 22 "templates/wrappers.hs" #-}

import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))


{-# LINE 89 "templates/wrappers.hs" #-}

{-# LINE 103 "templates/wrappers.hs" #-}

{-# LINE 118 "templates/wrappers.hs" #-}

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad

{-# LINE 231 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)

{-# LINE 320 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper

{-# LINE 347 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version

{-# LINE 365 "templates/wrappers.hs" #-}

{-# LINE 378 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str = go (alexStartPos,'\n',[],str)
  where go inp@(pos,_,_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version

{-# LINE 410 "templates/wrappers.hs" #-}


-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.

alex_base :: AlexAddr
alex_base = AlexA# "\xf8\xff\xff\xff\x94\xff\xff\xff\xd6\xff\xff\xff\x07\x00\x00\x00\x06\x00\x00\x00\x43\x00\x00\x00\xd4\xff\xff\xff\xd8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA# "\x00\x00\x03\x00\x0a\x00\x03\x00\x03\x00\x03\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x03\x00\x07\x00\x03\x00\x03\x00\x03\x00\x07\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x00\x00\x00\x00\x02\x00\x06\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA# "\xff\xff\x09\x00\x6e\x00\x0b\x00\x0c\x00\x0d\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x09\x00\x3d\x00\x0b\x00\x0c\x00\x0d\x00\x3d\x00\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x20\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\x2e\x00\x3d\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x53\x00\x54\x00\x55\x00\x56\x00\x57\x00\x58\x00\x59\x00\x5a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x5f\x00\xff\xff\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\x66\x00\x67\x00\x68\x00\x69\x00\x6a\x00\x6b\x00\x6c\x00\x6d\x00\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA# "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0::Int,10) [AlexAccNone,AlexAccNone,AlexAccNone,AlexAccSkip,AlexAcc (alex_action_1),AlexAcc (alex_action_2),AlexAcc (alex_action_3),AlexAcc (alex_action_4),AlexAcc (alex_action_5),AlexAcc (alex_action_6),AlexAcc (alex_action_7)]
{-# LINE 49 "src/Funk/Lexer.x" #-}


data Posn = Posn Int Int Int
          deriving (Show)

data Token = FloatLiteral Double
           | DefOp
           | Name String
           | OpName String
           | OpenParen
           | CloseParen
           | Eol
           deriving (Show)

mkToken :: (String -> Token) -> AlexPosn -> String -> (Posn, Token)
mkToken f (AlexPn a l c) s = ((Posn a l c), f s)

mkToken' :: String -> (String -> Token) -> AlexPosn -> String -> (Posn, Token)
mkToken' n f (AlexPn a l c) s = ((Posn a l c), f (trace (n ++ "(" ++ s ++ ")") s))

mkOp :: String -> Token
mkOp s 
  | s == "="  = DefOp
  | otherwise = OpName s

lex :: String -> [(Posn, Token)]
lex = alexScanTokens


alex_action_1 =  mkToken (\s -> FloatLiteral (read s)) 
alex_action_2 =  mkToken Name 
alex_action_3 =  mkToken (\_ -> DefOp) 
alex_action_4 =  mkToken mkOp 
alex_action_5 =  mkToken (\_ -> OpenParen) 
alex_action_6 =  mkToken (\_ -> CloseParen) 
alex_action_7 =  mkToken (\_ -> Eol) 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 7 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 35 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}


data AlexAddr = AlexA# Addr#

#if __GLASGOW_HASKELL__ < 503
uncheckedShiftL# = shiftL#
#endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow16Int# i
  where
        i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
        high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
        low  = int2Word# (ord# (indexCharOffAddr# arr off'))
        off' = off *# 2#
#else
  indexInt16OffAddr# arr off
#endif





{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr (AlexA# arr) off = 
#ifdef WORDS_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
		     (b2 `uncheckedShiftL#` 16#) `or#`
		     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
#else
  indexInt32OffAddr# arr off
#endif





#if __GLASGOW_HASKELL__ < 503
quickIndex arr i = arr ! i
#else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#endif




-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (I# (sc))
  = alexScanUser undefined input (I# (sc))

alexScanUser user input (I# (sc))
  = case alex_scan_tkn user input 0# input sc AlexNone of
	(AlexNone, input') ->
		case alexGetByte input of
			Nothing -> 

				   trace ("End of input.") $

				   AlexEOF
			Just _ ->

				   trace ("Error.") $

				   AlexError input'

	(AlexLastSkip input'' len, _) ->

		trace ("Skipping.") $ 

		AlexSkip input'' len

	(AlexLastAcc k input''' len, _) ->

		trace ("Accept.") $ 

		AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = (check_accs (alex_accept `quickIndex` (I# (s))))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 

      trace ("State: " ++ show (I# (s)) ++ ", char: " ++ show c) $

      case fromIntegral c of { (I# (ord_c)) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base +# ord_c)
                check  = alexIndexInt16OffAddr alex_check offset
		
                new_s = if (offset >=# 0#) && (check ==# ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
        case new_s of
	    -1# -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len +# 1#) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
			new_input new_s new_acc
      }
  where
	check_accs (AlexAccNone) = last_acc
	check_accs (AlexAcc a  ) = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkip) = AlexLastSkip  input (I# (len))

	check_accs (AlexAccPred a predx rest)
	   | predx user orig_input (I# (len)) input
	   = AlexLastAcc a input (I# (len))
	   | otherwise
	   = check_accs rest
	check_accs (AlexAccSkipPred predx rest)
	   | predx user orig_input (I# (len)) input
	   = AlexLastSkip input (I# (len))
	   | otherwise
	   = check_accs rest


data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap f AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap f (AlexLastSkip x y) = AlexLastSkip x y

data AlexAcc a user
  = AlexAccNone
  | AlexAcc a
  | AlexAccSkip

  | AlexAccPred a   (AlexAccPred user) (AlexAcc a user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc a user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (I# (sc)) user _ _ input = 
     case alex_scan_tkn user input 0# input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.


-- used by wrappers
iUnbox (I# (i)) = i
