import ParseStim hiding (main)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams(toFile, FileFormat( SVG ), fo_size, fo_format)
import Text.ParserCombinators.Parsec.Error (ParseError)

signal :: [Double] -> [(Double,Double)]
signal xs = [ (x,(sin (x*3.14159/45) + 1) / 2 * (sin (x*3.14159/5))) | x <- xs ]

main = do
  x <- loadStim
  case x of
    Right res -> let (acc,  pindResList, ocr0aResList) = traverseRes res in do
      let result = plotStim (acc,  pindResList, ocr0aResList)
      putStrLn ("Number of clock cycles total: " ++ (show acc))
      result
    Left err -> print err
  --where stmts = (acc,  pindResList, ocr0aResList) = traverseRes stmts

outProps = fo_size .~ (3000,400)
        $ fo_format .~ SVG
        $ def

-- https://github.com/timbod7/haskell-chart/wiki/Monadic-API
-- https://github.com/timbod7/haskell-chart/wiki/Frequently-Asked-Questions
plotStim (acc,  pindResList, ocr0aResList) = toFile outProps "Lab9B_Plot.svg" $ do
    layout_title .= "PIND Waveform"
    setColors [opaque blue, opaque red]
    plot (line "PIND" [pindResList])
    --plot (line "OCR0A" [ocr0aResList])    

-- unused
--getDelays stmts = mapMaybe (\stmt -> case stmt of
--  IntAssignment lhs rhs -> Nothing
--  Delay clockCycles -> Just clockCycles) stmts

-- Takes the given list of statements and returns a tuple of: the accumulator of all the delays, and two lists of tuples where: the first element is the number of clock cycles at which the event occurred, and the second element is the value that was set in the event, and the event is PIND for the first tuple returned, or for OCR0A for the second tuple returned.
traverseRes :: [Statement] -> (Int, [(Int, Int)], [(Int, Int)])
traverseRes stmts = foldl (\(acc, pindResList, ocr0aResList) stmt -> case stmt of
  IntAssignment lhs rhs -> case lhs of
    "PIND" -> (acc, pindResList ++ [(acc, rhs)], ocr0aResList)
    "OCR0A" -> (acc, pindResList, ocr0aResList ++ [(acc, rhs)])
  Delay clockCycles -> (acc + clockCycles, pindResList, ocr0aResList)) (0, [], []) stmts

loadStim :: IO (Either ParseError [Statement])
loadStim = do
  contents <- readFile "../LogFile.stim"
  let result = runStimParser contents
  return result
