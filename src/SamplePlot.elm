module SamplePlot exposing (..)

import Sample exposing (Sample)
import VegaLite exposing (..)
import Maybe exposing (withDefault)

samplePlotConfig =  { height = 100
                    , width = 150
                    , lblAngle = 0
                    }

samplePlot : Sample -> Spec
samplePlot sample =
    let
        n = sample.n
        pSuccess = if sample.n == 0 then 0 else sample.p |> withDefault 0
        pFailure = if sample.n == 0 then 0 else 1 - pSuccess

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

