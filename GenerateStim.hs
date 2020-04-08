{-# LANGUAGE NoMonomorphismRestriction,
             TypeSynonymInstances,
             FlexibleInstances,
             InstanceSigs,
             KindSignatures,
             FlexibleContexts #-}

module GenerateStim where

import Prelude hiding (sequence, log, break)
import Data.List (intercalate)
import Numeric (showHex, showIntAtBase) -- https://stackoverflow.com/questions/1959715/how-to-print-integer-literals-in-binary-or-hex-in-haskell
import Data.Char (intToDigit)

-- https://stackoverflow.com/questions/44186214/how-do-you-split-a-string-into-a-list-of-strings-all-of-length-3/44186570
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = take n l : (chunksOf n $ drop n l) --tip: $ implies parens around the rest (implicit parens)

class StimSYM repr where
  -- A sequence of instructions.
  sequence :: [repr] -> repr
  
  -- The entire program.
  program :: [repr] -> repr

  -- A comment.
  comment :: String -> repr

  -- Takes the given elements and puts them on the same line, without being separated by a newline.
  sameline :: [repr] -> repr

  -- A blank line.
  blank :: repr
  
  -- A newline.
  newline :: repr

  -- A delay by some number of clock cycles.
  delay :: Int -> repr

  -- An assignment operation.
  set :: String -> String -> repr -- TODO: later add support for *TCCR0A parsing in the right-hand side, also support for ensuring Int is passed for the right-hand side otherwise.

  -- Directives --

  -- log -- Logs a register value given the name of a register when it changes during the duration between the startLog and stopLog directives below.
  log :: String -> repr

  -- Starts a log with the given filename.
  startLog :: String -> repr
  
  stopLog :: repr

  -- Breaks (halts/pauses) the execution of the program (for the debugger)
  break :: repr

instance StimSYM String where
  sequence statements = intercalate newline statements
  program statements = sequence statements ++ newline -- In Atmel Studio, Stimuli files must end in a newline or else they won't work!
  sameline statements = intercalate " " statements
  blank = ""
  newline = "\n"
  -- TODO: Limit comment line lengths to 100 characters. -- TODO: do for all lines?..
  -- Notes: chunksOf makes a list with elements of the given length out of the given string,
  -- and then intercalate ( https://stackoverflow.com/questions/9220986/is-there-any-haskell-function-to-concatenate-list-with-separator )
  -- makes a string out of the list separated by the given separator string.
  comment text = "// " ++ text
  delay cycles = "#" ++ (show cycles)
  set register value = register ++ " = " ++ value -- note: show() on a string puts quotes around it
  log register = "$log " ++ register
  startLog logFileName = "$startlog " ++ logFileName
  stopLog = "$stoplog"
  break = "$break"

intToHex x = "0x" ++ showIntAtBase 16 intToDigit x ""

--demo :: (StimSYM repr) => repr
demo = program [
  comment "This stimuli file sets PINB to go from 0 to 255 over time,",
  comment "which will in turn ramp up the duty cycle from 0% to 100%",
  comment "via pin change interrupts in the program.",
  blank,
  log "PIND",
  log "OCR0A",
  startLog "LogFile.stim",
  
  sameline [
      delay 23, comment "23 cycles to reach first \"rjmp spin\" instruction in the program."
      ],
  blank,
  (sequence (map (\currentCounter -> (sequence [
                                         set "PINB" (intToHex currentCounter),
                                         delay 256
                                         ])) [1..255])), --foldl (\currentString -> \currentCounter -> (currentString ++ sequence ([set ("PINB" intToHex currentCounter), delay 7])) "" [1..255])

  blank,
  stopLog,
  break
  ]
  
main = --putStrLn (concat (map (\currentCounter -> (sequence [set "PINB" (intToHex currentCounter), delay 7])) [1..100]))
  writeFile "Lab9B.stim" demo
