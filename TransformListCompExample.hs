{-# LANGUAGE TransformListComp #-}
-- {-# OPTIONS_GHC -ddump-deriv  #-}
-- {-# OPTIONS_GHC -ddump-ds -dsuppress-all  #-}
-- {-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use fst" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use map once" #-}
module Tmp where
import           GHC.Exts (groupWith, sortWith, the)
import Data.Function ( (&) )


employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60) ]

output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept using groupWith
         , then sortWith by sum salary
         , then take 5 ]


-- Below is what GHC expands the above to, created via a few refactorings based on the ugly outogenerated code


mapFix :: (a -> b) -> ([a] -> [b]) -> [a] -> [b]
mapFix f self = \xs -> case xs of
  []   -> []
  x:xs -> f x : self xs

matchList f = \xs -> case xs of
  []   -> []
  x:xs -> f x xs

-- unzip :: [(a,b)] -> ([a], [b])
-- unzip = foldr (\ (a, b) (as, bs) -> (a : as, b : bs)) ([], [])

-- {-# RULES "extractCase" forall a b. case ds_d5OSE of { (dept, salary) -> (the dept, sum salary) : ds_d5OSC ds_d5OSF } = 9 #-}

-- {-# RULES "extractCase" forall a f b. let a = mapFix f a in a b = map f b #-}

mmap :: (a -> b) -> [a] -> [b]
mmap f x = let a = mapFix f a in a x

output1 =
    employees
  & map       (\(_, dept, salary) -> (dept, salary))
  & groupWith (\(dept, _) -> dept)
  & map       unzip
  -- & map       (\(dept, salary) -> (dept, salary))
  & sortWith  (\(_, salary) -> sum salary)
  -- & map       (\(dept, salary) -> (dept, salary))
  & take 5
  & map       (\(dept, salary) -> (the dept, sum salary))



{-


output1
  = let {
      ds_d5OSC
        = \ ds_d5OSD ->
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                (the dept, sum salary) : ds_d5OSC ds_d5OSF
                }
            }; } in
    ds_d5OSC
      (take 5
         (let {
            ds_d5OSu
              = \ ds_d5OSv ->
                  case ds_d5OSv of {
                    [] -> [];
                    ds_d5OSw : ds_d5OSx ->
                      case ds_d5OSw of { (dept, salary) ->
                      (dept, salary) : ds_d5OSu ds_d5OSx
                      }
                  }; } in
          ds_d5OSu
            (sortWith
               (\(_, salary) -> sum salary)
               (let {
                  ds_d5OSl
                    = \ ds_d5OSm ->
                        case ds_d5OSm of {
                          [] -> [];
                          ds_d5OSn : ds_d5OSo ->
                            case ds_d5OSn of { (dept, salary) ->
                            (dept, salary) : ds_d5OSl ds_d5OSo
                            }
                        }; } in
                ds_d5OSl
                  (map
                     (foldr
                          (\ (ds_d5OSg, ds_d5OSh) (ds_d5OSi, ds_d5OSj) -> (ds_d5OSg : ds_d5OSi, ds_d5OSh : ds_d5OSj))
                          ([], []))
                     (groupWith
                        (\(dept, _) -> dept)
                        (let {
                           ds_d5OS4
                             = \ ds_d5OS5 ->
                                 case ds_d5OS5 of {
                                   [] -> [];
                                   ds_d5OS6 : ds_d5OS7 ->
                                     case ds_d5OS6 of { (_, dept, salary) ->
                                     (dept, salary) : ds_d5OS4 ds_d5OS7
                                     }
                                 }; } in
                         ds_d5OS4 employees)))))))



mapPairs :: (t1 -> t2 -> a) -> ([(t1, t2)] -> [a]) -> [(t1, t2)] -> [a]
mapPairs f ds_d5OSC ds_d5OSD =
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                f dept salary : ds_d5OSC ds_d5OSF
                }
            };

fancyId :: ([(a, b)] -> [(a, b)]) -> [(a, b)] -> [(a, b)]
fancyId ds_d5OSC ds_d5OSD =
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                (dept, salary) : ds_d5OSC ds_d5OSF
                }
            };

sillyId = fancyId sillyId

-- {-# RULES "lst" forall a b. (:) a b = a : b #-}


-- RHS size: {terms: 36, types: 61, coercions: 0, joins: 0/0}
-- employees1
--   = : (unpackCString# "Simon"#, unpackCString# "MS"#, 80)
--       (: (unpackCString# "Erik"#, unpackCString# "MS"#, 100)
--          (: (unpackCString# "Phil"#, unpackCString# "Ed"#, 40)
--             (: (unpackCString# "Gordon"#, unpackCString# "Ed"#, 45)
--                (: (unpackCString# "Paul"#, unpackCString# "Yale"#, 60) []))))

-- RHS size: {terms: 120, types: 388, coercions: 0, joins: 0/5}
output1
  = let {
      ds_d5OSC
        = mapPairs  (\dept salary -> (the dept, sum salary))   ds_d5OSC; } in
    ds_d5OSC
      (take 5
         (sillyId
            (sortWith
               (\(_, salary) -> sum salary)
               (sillyId
                  (map
                     (foldr
                          (\ (ds_d5OSg, ds_d5OSh) (ds_d5OSi, ds_d5OSj) -> (ds_d5OSg : ds_d5OSi, ds_d5OSh : ds_d5OSj))
                          ([], []))
                     (groupWith
                        (\(dept, _) -> dept)
                        (let {
                           ds_d5OS4
                             = \ ds_d5OS5 ->
                                 case ds_d5OS5 of {
                                   [] -> [];
                                   ds_d5OS6 : ds_d5OS7 ->
                                     case ds_d5OS6 of { (_, dept, salary) ->
                                     (dept, salary) : ds_d5OS4 ds_d5OS7
                                     }
                                 }; } in
                         ds_d5OS4 employees)))))))


-}


{-

mapPairs :: (t1 -> t2 -> a) -> ([(t1, t2)] -> [a]) -> [(t1, t2)] -> [a]
mapPairs f ds_d5OSC ds_d5OSD =
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                f dept salary : ds_d5OSC ds_d5OSF
                }
            };

fancyId :: ([(a, b)] -> [(a, b)]) -> [(a, b)] -> [(a, b)]
fancyId ds_d5OSC ds_d5OSD =
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                (dept, salary) : ds_d5OSC ds_d5OSF
                }
            };

sillyId = fancyId sillyId

-- {-# RULES "lst" forall a b. (:) a b = a : b #-}


-- RHS size: {terms: 36, types: 61, coercions: 0, joins: 0/0}
-- employees1
--   = : (unpackCString# "Simon"#, unpackCString# "MS"#, 80)
--       (: (unpackCString# "Erik"#, unpackCString# "MS"#, 100)
--          (: (unpackCString# "Phil"#, unpackCString# "Ed"#, 40)
--             (: (unpackCString# "Gordon"#, unpackCString# "Ed"#, 45)
--                (: (unpackCString# "Paul"#, unpackCString# "Yale"#, 60) []))))

-- RHS size: {terms: 120, types: 388, coercions: 0, joins: 0/5}
output1
  = let {
      ds_d5OSC
        = mapPairs  (\dept salary -> (the dept, sum salary))   ds_d5OSC; } in
    ds_d5OSC
      (take 5
         (sillyId
            (sortWith
               (\(_, salary) -> sum salary)
               (sillyId
                  (map
                     (foldr
                          (\ (ds_d5OSg, ds_d5OSh) (ds_d5OSi, ds_d5OSj) -> (ds_d5OSg : ds_d5OSi, ds_d5OSh : ds_d5OSj))
                          ([], []))
                     (groupWith
                        (\(dept, _) -> dept)
                        (let {
                           ds_d5OS4
                             = \ ds_d5OS5 ->
                                 case ds_d5OS5 of {
                                   [] -> [];
                                   ds_d5OS6 : ds_d5OS7 ->
                                     case ds_d5OS6 of { (_, dept, salary) ->
                                     (dept, salary) : ds_d5OS4 ds_d5OS7
                                     }
                                 }; } in
                         ds_d5OS4 employees)))))))


-- -}

{-

mapPairs :: (t1 -> t2 -> a) -> ([(t1, t2)] -> [a]) -> [(t1, t2)] -> [a]
mapPairs f ds_d5OSC ds_d5OSD =
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                f dept salary : ds_d5OSC ds_d5OSF
                }
            };

fancyId :: ([(a, b)] -> [(a, b)]) -> [(a, b)] -> [(a, b)]
fancyId ds_d5OSC ds_d5OSD =
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                (dept, salary) : ds_d5OSC ds_d5OSF
                }
            };

sillyId = fancyId sillyId

-- {-# RULES "lst" forall a b. (:) a b = a : b #-}


-- RHS size: {terms: 36, types: 61, coercions: 0, joins: 0/0}
-- employees1
--   = : (unpackCString# "Simon"#, unpackCString# "MS"#, 80)
--       (: (unpackCString# "Erik"#, unpackCString# "MS"#, 100)
--          (: (unpackCString# "Phil"#, unpackCString# "Ed"#, 40)
--             (: (unpackCString# "Gordon"#, unpackCString# "Ed"#, 45)
--                (: (unpackCString# "Paul"#, unpackCString# "Yale"#, 60) []))))

-- RHS size: {terms: 120, types: 388, coercions: 0, joins: 0/5}
output1
  = let {
      ds_d5OSC
        = mapPairs  (\dept salary -> (the dept, sum salary))   ds_d5OSC; } in
    ds_d5OSC
      (take 5
         (sillyId
            (sortWith
               (\(_, salary) -> sum salary)
               (sillyId
                  (map
                     (foldr
                          (\ (ds_d5OSg, ds_d5OSh) (ds_d5OSi, ds_d5OSj) -> (ds_d5OSg : ds_d5OSi, ds_d5OSh : ds_d5OSj))
                          ([], []))
                     (groupWith
                        (\(dept, _) -> dept)
                        (let {
                           ds_d5OS4
                             = \ ds_d5OS5 ->
                                 case ds_d5OS5 of {
                                   [] -> [];
                                   ds_d5OS6 : ds_d5OS7 ->
                                     case ds_d5OS6 of { (_, dept, salary) ->
                                     (dept, salary) : ds_d5OS4 ds_d5OS7
                                     }
                                 }; } in
                         ds_d5OS4 employees)))))))


-- -}

{-

{-# LANGUAGE TransformListComp #-}
-- {-# OPTIONS_GHC -ddump-deriv  #-}
{-# OPTIONS_GHC -ddump-ds -dsuppress-all  #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use fst" #-}
module Tmp where
import GHC.Exts (the, groupWith, sortWith)


employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60) ]

output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept using groupWith
         , then sortWith by sum salary
         , then take 5 ]

-- {-


-- {-# RULES "lst" forall a b. (:) a b = a : b #-}


-- RHS size: {terms: 36, types: 61, coercions: 0, joins: 0/0}
-- employees1
--   = : (unpackCString# "Simon"#, unpackCString# "MS"#, 80)
--       (: (unpackCString# "Erik"#, unpackCString# "MS"#, 100)
--          (: (unpackCString# "Phil"#, unpackCString# "Ed"#, 40)
--             (: (unpackCString# "Gordon"#, unpackCString# "Ed"#, 45)
--                (: (unpackCString# "Paul"#, unpackCString# "Yale"#, 60) []))))

-- RHS size: {terms: 120, types: 388, coercions: 0, joins: 0/5}
output1
  = let {
      ds_d5OSC
        = \ ds_d5OSD ->
            case ds_d5OSD of {
              [] -> [];
              ds_d5OSE : ds_d5OSF ->
                case ds_d5OSE of { (dept, salary) ->
                (the dept, sum salary) : ds_d5OSC ds_d5OSF
                }
            }; } in
    ds_d5OSC
      (take 5
         (let {
            ds_d5OSu
              = \ ds_d5OSv ->
                  case ds_d5OSv of {
                    [] -> [];
                    ds_d5OSw : ds_d5OSx ->
                      case ds_d5OSw of { (dept, salary) ->
                      (dept, salary) : ds_d5OSu ds_d5OSx
                      }
                  }; } in
          ds_d5OSu
            (sortWith
               (\(_, salary) -> sum salary)
               (let {
                  ds_d5OSl
                    = \ ds_d5OSm ->
                        case ds_d5OSm of {
                          [] -> [];
                          ds_d5OSn : ds_d5OSo ->
                            case ds_d5OSn of { (dept, salary) ->
                            (dept, salary) : ds_d5OSl ds_d5OSo
                            }
                        }; } in
                ds_d5OSl
                  (map
                     (foldr
                          (\ (ds_d5OSg, ds_d5OSh) (ds_d5OSi, ds_d5OSj) -> (ds_d5OSg : ds_d5OSi, ds_d5OSh : ds_d5OSj))
                          ([], []))
                     (groupWith
                        (\(dept, _) -> dept)
                        (let {
                           ds_d5OS4
                             = \ ds_d5OS5 ->
                                 case ds_d5OS5 of {
                                   [] -> [];
                                   ds_d5OS6 : ds_d5OS7 ->
                                     case ds_d5OS6 of { (_, dept, salary) ->
                                     (dept, salary) : ds_d5OS4 ds_d5OS7
                                     }
                                 }; } in
                         ds_d5OS4 employees)))))))


-- -}

-}


{-

{-# LANGUAGE TransformListComp #-}
-- {-# OPTIONS_GHC -ddump-deriv  #-}
{-# OPTIONS_GHC -ddump-ds -dsuppress-all  #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use fst" #-}
module Tmp where
import GHC.Exts (the, groupWith, sortWith)


employees = [ ("Simon", "MS", 80)
            , ("Erik", "MS", 100)
            , ("Phil", "Ed", 40)
            , ("Gordon", "Ed", 45)
            , ("Paul", "Yale", 60) ]

output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept using groupWith
         , then sortWith by sum salary
         , then take 5 ]

-- {-


sillyIdentity :: ([(a, b)] -> [(a, b)]) -> [(a, b)] -> [(a, b)]
sillyIdentity ds_d5OSl = \ ds_d5OSm ->
                        case ds_d5OSm of {
                          [] -> [];
                          ds_d5OSn : ds_d5OSo ->
                            case ds_d5OSn of { (dept, salary) ->
                            (dept, salary) : ds_d5OSl ds_d5OSo
                            }
                        };

sillyIdentity2 :: ([(a, b)] -> [(a, b)]) -> [(a, b)] -> [(a, b)]
sillyIdentity2 fancyId = \x -> case x of
                        [] -> []
                        (dept, salary) : rest -> (dept, salary) : fancyId rest

-- {-# RULES "lst" forall a b. (:) a b = a : b #-}


-- RHS size: {terms: 36, types: 61, coercions: 0, joins: 0/0}
-- employees1
--   = : (unpackCString# "Simon"#, unpackCString# "MS"#, 80)
--       (: (unpackCString# "Erik"#, unpackCString# "MS"#, 100)
--          (: (unpackCString# "Phil"#, unpackCString# "Ed"#, 40)
--             (: (unpackCString# "Gordon"#, unpackCString# "Ed"#, 45)
--                (: (unpackCString# "Paul"#, unpackCString# "Yale"#, 60) []))))

-- RHS size: {terms: 120, types: 388, coercions: 0, joins: 0/5}
output1
  = let
      finalTransform = \ x -> case x of
              [] -> []
              (dept, salary) : someTail -> (the dept, sum salary) : finalTransform someTail
    in
    finalTransform
      (take 5
         (let {
            fancyId = sillyIdentity2 fancyId
            } in
          fancyId
            (sortWith
               (\(_, salary) -> sum salary)
               (let {
                  ds_d5OSl = sillyIdentity ds_d5OSl; } in
                ds_d5OSl
                  (map
                     (foldr
                          (\ (ds_d5OSg, ds_d5OSh) (ds_d5OSi, ds_d5OSj) -> (ds_d5OSg : ds_d5OSi, ds_d5OSh : ds_d5OSj))
                          ([], []))
                     (groupWith
                        (\(dept, _) -> dept)
                        (let {
                           ds_d5OS4
                             = \ ds_d5OS5 ->
                                 case ds_d5OS5 of {
                                   [] -> [];
                                   ds_d5OS6 : ds_d5OS7 ->
                                     case ds_d5OS6 of { (_, dept, salary) ->
                                     (dept, salary) : ds_d5OS4 ds_d5OS7
                                     }
                                 }; } in
                         ds_d5OS4 employees)))))))


-- -}


 -}
