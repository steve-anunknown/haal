module Bench.Scaling (scalingBenchmarks) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf, whnf)

import Haal.BlackBox (accessSequences, globalCharacterizingSet, reachable)
import Haal.EquivalenceOracle.WMethod (WMethod, WMethodConfig (..), mkWMethod, wmethodSuiteSize)
import Haal.Experiment (experiment, runExperiment)
import Haal.Learning.LMstar (LMstarConfig (..), mkLMstar)

import Haal.Models.MQTT.ActiveMQNonClean (activeMQNonClean)
import Haal.Models.MQTT.EmqttSimple (emqttSimple)
import Haal.Models.TCP.TCPLinuxServer (tCPLinuxServer)
import Haal.Models.TLS.OpenSSLV1V0V2ServerRegular (openSSLV1V0V2ServerRegular)

wmethod :: Int -> WMethod
wmethod d = either error id (mkWMethod (WMethodConfig d))

-- | Scaling benchmarks: measure BlackBox operations and learning across model sizes.
scalingBenchmarks :: Benchmark
scalingBenchmarks = bgroup "Scaling"
    [ bgroup "reachable"
        [ bench "MQTT/EmqttSimple (3 states)" $
            nf (Set.size . reachable) emqttSimple
        , bench "TLS/OpenSSL (7 states)" $
            nf (Set.size . reachable) openSSLV1V0V2ServerRegular
        , bench "MQTT/ActiveMQNonClean (12 states)" $
            nf (Set.size . reachable) activeMQNonClean
        , bench "TCP/LinuxServer (57 states)" $
            nf (Set.size . reachable) tCPLinuxServer
        ]
    , bgroup "accessSequences"
        [ bench "MQTT/EmqttSimple (3 states)" $
            nf (Map.size . accessSequences) emqttSimple
        , bench "TLS/OpenSSL (7 states)" $
            nf (Map.size . accessSequences) openSSLV1V0V2ServerRegular
        , bench "MQTT/ActiveMQNonClean (12 states)" $
            nf (Map.size . accessSequences) activeMQNonClean
        , bench "TCP/LinuxServer (57 states)" $
            nf (Map.size . accessSequences) tCPLinuxServer
        ]
    , bgroup "globalCharacterizingSet"
        [ bench "MQTT/EmqttSimple (3 states)" $
            nf (Set.size . globalCharacterizingSet) emqttSimple
        , bench "TLS/OpenSSL (7 states)" $
            nf (Set.size . globalCharacterizingSet) openSSLV1V0V2ServerRegular
        , bench "MQTT/ActiveMQNonClean (12 states)" $
            nf (Set.size . globalCharacterizingSet) activeMQNonClean
        , bench "TCP/LinuxServer (57 states)" $
            nf (Set.size . globalCharacterizingSet) tCPLinuxServer
        ]
    , bgroup "wmethodSuiteSize"
        [ bgroup "MQTT/ActiveMQNonClean (12 states)"
            [ bench "depth=0" $ nf (\w -> wmethodSuiteSize w activeMQNonClean) (wmethod 0)
            , bench "depth=1" $ nf (\w -> wmethodSuiteSize w activeMQNonClean) (wmethod 1)
            , bench "depth=2" $ nf (\w -> wmethodSuiteSize w activeMQNonClean) (wmethod 2)
            , bench "depth=3" $ nf (\w -> wmethodSuiteSize w activeMQNonClean) (wmethod 3)
            ]
        ]
    , bgroup "learning"
        [ bench "MQTT/EmqttSimple (3 states)" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 1))) emqttSimple
        , bench "TLS/OpenSSL (7 states)" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 1))) openSSLV1V0V2ServerRegular
        , bench "TCP/LinuxServer (57 states)" $
            whnf (runExperiment (experiment (mkLMstar Star) (wmethod 0))) tCPLinuxServer
        ]
    ]
