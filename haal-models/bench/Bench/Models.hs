module Bench.Models (modelBenchmarks) where

import Test.Tasty.Bench (Benchmark, bench, bgroup, whnf)

import Haal.EquivalenceOracle.WMethod (WMethod, WMethodConfig (..), mkWMethod)
import Haal.EquivalenceOracle.WpMethod (WpMethod, WpMethodConfig (..), mkWpMethod)
import Haal.Experiment (experiment, runExperiment)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)

import Haal.Models.MQTT.EmqttSimple (emqttSimple)
import Haal.Models.TCP.TCPLinuxServer (tCPLinuxServer)
import Haal.Models.TLS.OpenSSLV1V0V2ServerRegular (openSSLV1V0V2ServerRegular)

wmethod :: Int -> WMethod
wmethod d = either error id (mkWMethod (WMethodConfig d))

wpmethod :: Int -> WpMethod
wpmethod d = either error id (mkWpMethod (WpMethodConfig d))

-- | End-to-end learning benchmarks on real protocol models.
-- Uses whnf since MealyAutomaton contains functions (no NFData).
modelBenchmarks :: Benchmark
modelBenchmarks = bgroup "Models"
    [ bgroup "MQTT/EmqttSimple (3 states)"
        [ bench "LMstar/WMethod" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 1))) emqttSimple
        , bench "LMstar/WpMethod" $
            whnf (runExperiment (experiment (mkLMstar Star) (wpmethod 1))) emqttSimple
        , bench "LMplus/WMethod" $
            whnf (runExperiment (experiment (mkLMstar Plus) (wmethod 1))) emqttSimple
        ]
    , bgroup "TLS/OpenSSL (7 states)"
        [ bench "LMstar/WMethod" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 1))) openSSLV1V0V2ServerRegular
        , bench "LMstar/WpMethod" $
            whnf (runExperiment (experiment (mkLMstar Star) (wpmethod 1))) openSSLV1V0V2ServerRegular
        , bench "LMplus/WMethod" $
            whnf (runExperiment (experiment (mkLMstar Plus) (wmethod 1))) openSSLV1V0V2ServerRegular
        ]
    , bgroup "TCP/LinuxServer (57 states)"
        [ bench "LMstar/WMethod" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 0))) tCPLinuxServer
        , bench "LMstar/WpMethod" $
            whnf (runExperiment (experiment (mkLMstar Star) (wpmethod 0))) tCPLinuxServer
        ]
    ]
