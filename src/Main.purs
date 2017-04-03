module Main where

import App (runApp)
import Halogen.Aff (runHalogenAff)

main = runHalogenAff runApp
