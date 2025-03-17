module Main where
import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
-- Función condicional personalizada que imita el if-then-else tradicional
ifThenElse :: Bool -> a -> a -> a
ifThenElse cond si no = if cond then si else no
-- Función principal que aplica las reglas de FizzBuzz
fizzBuzz :: Int -> String
fizzBuzz n
| n `mod` 15 == 0 = "FizzBuzz!" -- Si es múltiplo de 3 y 5

Coordinación de Ingeniería en
Sistemas Computacionales

| n `mod` 3 == 0 = "Fizz!" -- Si es múltiplo de 3
| n `mod` 5 == 0 = "Buzz!" -- Si es múltiplo de 5
| otherwise = numeroEnPalabras n -- Si no es múltiplo de ninguno, convierte a palabras
-- Diccionario con números del 1 al 19 en inglés
menoresDe20 :: Map.Map Int String
menoresDe20 = Map.fromList [
(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"),
(6, "six"), (7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"),
(11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen"),
(15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen"),
(19, "nineteen")
]
-- Diccionario con las decenas en inglés
decenas :: Map.Map Int String
decenas = Map.fromList [
(2, "twenty"), (3, "thirty"), (4, "forty"), (5, "fifty"),
(6, "sixty"), (7, "seventy"), (8, "eighty"), (9, "ninety")
]
-- Función que convierte un número a palabras en inglés
numeroEnPalabras :: Int -> String
numeroEnPalabras n
| n < 20 = fromMaybe (show n) (Map.lookup n menoresDe20) -- Si es menor a 20, busca en el diccionario
| n < 100 && n `mod` 10 == 0 = fromMaybe (show n) (Map.lookup (n `div` 10) decenas) -- Si es una decena exacta
| n < 100 = fromMaybe "" (Map.lookup (n `div` 10) decenas) ++ "-" ++ fromMaybe "" (Map.lookup (n `mod` 10) menoresDe20) --
Si es un número entre decenas
| n == 100 = "one hundred" -- Caso único para 100
| otherwise = show n -- Cualquier otro número fuera del rango
-- Pruebas para la función fizzBuzz
fizzBuzzTests :: TestTree
fizzBuzzTests = testGroup "FizzBuzz Tests"
[ testCase "1 is one" $ fizzBuzz 1 @?= "one"
, testCase "2 is two" $ fizzBuzz 2 @?= "two"
, testCase "3 is Fizz!" $ fizzBuzz 3 @?= "Fizz!"
, testCase "5 is Buzz!" $ fizzBuzz 5 @?= "Buzz!"
, testCase "15 is FizzBuzz!" $ fizzBuzz 15 @?= "FizzBuzz!"
, testCase "19 is nineteen" $ fizzBuzz 19 @?= "nineteen"
, testCase "20 is twenty" $ fizzBuzz 20 @?= "twenty"
, testCase "21 is Fizz!" $ fizzBuzz 21 @?= "Fizz!"
, testCase "25 is Buzz!" $ fizzBuzz 25 @?= "Buzz!"
, testCase "30 is FizzBuzz!" $ fizzBuzz 30 @?= "FizzBuzz!"
]
-- Función principal de pruebas
main :: IO ()
main = defaultMain fizzBuzzTests