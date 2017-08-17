module ActorMessages where

import qualified Data.Text as T

import XMapTypes
import Project
import WebClients

data CalculationMessage = CMMap XNamedMap |
                          CMLog T.Text |
                          CMStop

data ProjectMessage = PMMap XNamedMap |
                      PNUpdateProject Project |
                      PMLog T.Text |
                      PMStop

data ViewMessage = VMMap XNamedMap |
                   VMLog T.Text |
                   VMStop

data LogMessage = LMLog T.Text |
                  LMStop

data SystemMessage = SMLoadProject WAClient ProjectName |
                     SMNewProject WAClient Project |
                     SMStop

