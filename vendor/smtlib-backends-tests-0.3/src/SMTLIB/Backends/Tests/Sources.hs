{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module SMTLIB.Backends.Tests.Sources (Source (..), sources) where

import SMTLIB.Backends

-- | A source is a list of SMTLib2 commands, available in several different formats
-- for testing purposes.
data Source = Source
  { -- | The name of the source.
    name :: String,
    -- | A computation equivalent to sending the raw content of the source to the
    -- solver.
    run :: Solver -> IO ()
  }

-- | A list of examples SMT-LIB files. Most of them were taken from the official
-- SMT-LIB website:
-- https://smtlib.cs.uiowa.edu/examples.shtml
sources =
  [ assertions,
    assignments,
    boolean,
    info,
    integerArithmetic,
    modelingSequentialCodeSSA,
    modelingSequentialCodeBitvectors,
    scopes,
    sorts,
    unsatCores,
    valuesOrModels
  ]

assertions = Source "assertions" $ \solver -> do
  command_ solver "(set-option :produce-assertions true)"
  command_ solver "(set-logic QF_UF)"
  command_ solver "(declare-const p Bool)"
  command_ solver "(declare-const q Bool)"
  command_ solver "(push 1)"
  command_ solver "(assert (or p q))"
  command_ solver "(push 1)"
  command_ solver "(assert (not q))"
  _ <- command solver "(get-assertions)"
  command_ solver "(pop 1)"
  _ <- command solver "(get-assertions)"
  command_ solver "(pop 1)"
  _ <- command solver "(get-assertions)"
  return ()

assignments = Source "assignments" $ \solver -> do
  command_ solver "(set-option :produce-assignments true)"
  command_ solver "(set-logic QF_UF)"
  command_ solver "(declare-const p Bool)"
  command_ solver "(declare-const q Bool)"
  command_ solver "(declare-const r Bool)"
  command_ solver "(assert (not (=(! (and (! p :named P) q) :named PQ) (! r :named R))))"
  _ <- command solver "(check-sat)"
  _ <- command solver "(get-assignment)"
  return ()

boolean = Source "boolean" $ \solver -> do
  command_ solver "(set-logic QF_UF)"
  command_ solver "(declare-const p Bool)"
  command_ solver "(assert (and p (not p)))"
  _ <- command solver "(check-sat)"
  return ()

info = Source "info" $ \solver -> do
  _ <- command solver "(get-info :name)"
  _ <- command solver "(get-info :version )"
  _ <- command solver "(get-info :authors )"
  return ()

integerArithmetic = Source "integer arithmetic" $ \solver -> do
  command_ solver "(set-logic QF_LIA)"
  command_ solver "(declare-const x Int)"
  command_ solver "(declare-const y Int)"
  command_ solver "(assert (= (- x y) (+ x (- y) 1)))"
  _ <- command solver "(check-sat)"
  return ()

modelingSequentialCodeSSA = Source "modeling sequential code (SSA)" $ \solver -> do
  command_ solver "(set-logic QF_UFLIA)"
  command_ solver "(set-option :produce-models true)"
  command_ solver "(declare-fun x (Int) Int)"
  command_ solver "(declare-fun y (Int) Int)"
  command_ solver "(declare-fun t (Int) Int)"
  command_ solver "(assert (= (t 0) (x 0)))"
  command_ solver "(assert (= (y 1) (t 0)))"
  command_ solver "(assert (= (x 1) (y 1)))"

  command_ solver "(assert (not (and (= (x 1) (y 0)) (= (y 1) (x 0)))))"
  _ <- command solver "(check-sat)"
  _ <- command solver "(get-value ((x 0) (y 0) (x 1) (y 1)))"
  _ <- command solver "(get-model)"
  return ()

modelingSequentialCodeBitvectors = Source "modeling sequential code (bitvectors)" $
  \solver -> do
    command_ solver "(set-logic QF_BV)"
    command_ solver "(set-option :produce-models true)"
    command_ solver "(declare-const x_0 (_ BitVec 32))"
    command_ solver "(declare-const x_1 (_ BitVec 32))"
    command_ solver "(declare-const x_2 (_ BitVec 32))"
    command_ solver "(declare-const y_0 (_ BitVec 32))"
    command_ solver "(declare-const y_1 (_ BitVec 32))"
    command_ solver "(assert (= x_1 (bvadd x_0 y_0)))"
    command_ solver "(assert (= y_1 (bvsub x_1 y_0)))"
    command_ solver "(assert (= x_2 (bvsub x_1 y_1)))"
    command_ solver "(assert (not (and (= x_2 y_0) (= y_1 x_0))))"
    _ <- command solver "(check-sat)"
    return ()

scopes = Source "scopes" $ \solver -> do
  command_ solver "(set-logic QF_LIA)"
  command_ solver "(declare-const x Int)"
  command_ solver "(declare-const y Int)"
  command_ solver "(assert (= (+ x (* 2 y)) 20))"
  command_ solver "(push 1)"
  command_ solver "(assert (= (- x y) 2))"
  _ <- command solver "(check-sat)"
  command_ solver "(pop 1)"
  command_ solver "(push 1)"
  command_ solver "(assert (= (- x y) 3))"
  _ <- command solver "(check-sat)"
  command_ solver "(pop 1)"
  return ()

sorts = Source "sorts" $ \solver -> do
  command_ solver "(set-logic QF_UF)"
  command_ solver "(declare-sort A 0)"
  command_ solver "(declare-const a A)"
  command_ solver "(declare-const b A)"
  command_ solver "(declare-const c A)"
  command_ solver "(declare-const d A)"
  command_ solver "(declare-const e A)"
  command_ solver "(assert (or (= c a)(= c b)))"
  command_ solver "(assert (or (= d a)(= d b)))"
  command_ solver "(assert (or (= e a)(= e b)))"
  command_ solver "(push 1)"
  command_ solver "(assert (distinct c d))"
  _ <- command solver "(check-sat)"
  command_ solver "(pop 1)"
  command_ solver "(push 1)"
  command_ solver "(assert (distinct c d e))"
  _ <- command solver "(check-sat)"
  command_ solver "(pop 1)"
  return ()

unsatCores = Source "unsat cores" $ \solver -> do
  command_ solver "(set-option :produce-unsat-cores true)"
  command_ solver "(set-logic QF_UF)"
  command_ solver "(declare-const p Bool)"
  command_ solver "(declare-const q Bool)"
  command_ solver "(declare-const r Bool)"
  command_ solver "(declare-const s Bool)"
  command_ solver "(declare-const t Bool)"
  command_ solver "(assert (! (=> p q) :named PQ))"
  command_ solver "(assert (! (=> q r) :named QR))"
  command_ solver "(assert (! (=> r s) :named RS))"
  command_ solver "(assert (! (=> s t) :named ST))"
  command_ solver "(assert (! (not (=> q s)) :named NQS))"
  _ <- command solver "(check-sat)"
  _ <- command solver "(get-unsat-core)"
  return ()

valuesOrModels = Source "values or models" $ \solver -> do
  command_ solver "(set-option :produce-models true)"
  command_ solver "(set-logic QF_LIA)"
  command_ solver "(declare-const x Int)"
  command_ solver "(declare-const y Int)"
  command_ solver "(assert (= (+ x (* 2 y)) 20))"
  command_ solver "(assert (= (- x y) 2))"
  _ <- command solver "(check-sat)"
  _ <- command solver "(get-value (x y))"
  _ <- command solver "(get-model)"
  return ()
