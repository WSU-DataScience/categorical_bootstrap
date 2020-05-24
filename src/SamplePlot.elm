module SamplePlot exposing (..)

import DataSet exposing (Sample)
import VegaLite exposing (..)

samplePlotConfig =  { height = 100
                    , width = 150
                    , lblAngle = 0
                    }

samplePlot : Sample -> Spec
samplePlot sample =
    let
        n = sample.n
        pSuccess = (toFloat sample.numSuccess)/(toFloat n)
        pFailure = (toFloat sample.numFailures)/(toFloat n)
        xs = [sample.successLbl, sample.failureLbl]

        ys = [pSuccess, pFailure] 

        data =
            dataFromColumns []
                << dataColumn "Outcome" (strs xs)
                << dataColumn "Frequency" (nums ys)

        enc =
            encoding
                << position X
                    [ pName "Outcome"
                    , pMType Nominal
                    ]
                << position Y [ pName "Frequency"
                              , pMType Quantitative
                              , pScale [ scDomain (doNums [ 0, 1 ]) ]
                              ]
                << color [ mName "Outcome", mMType Nominal, mLegend []]
                -- << tooltips [ [ tName "Outcome", tMType Nominal]
                --               , [ tName "Frequency", tMType Quantitative]
                --               ]
        cfg =
               configure
                   << configuration
                       (coAxis
                           [ axcoGridOpacity 0.1
                           , axcoLabelAngle samplePlotConfig.lblAngle
                           ]
                       )
    in
      toVegaLite [ data []
                 , VegaLite.height samplePlotConfig.height
                 , VegaLite.width samplePlotConfig.width
                 , cfg []
                 , bar []
                 , enc []
                 ]

