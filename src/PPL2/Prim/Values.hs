{-# LANGUAGE ConstraintKinds #-}

module PPL2.Prim.Values
where

import PPL2.Prim.Prelude


-- in the VM we always need these kinds of values
--
-- a machine word may be interpeted as unsigned or signed value or as boolean
--
-- for references to program variable we need data refs,
-- for code pointers code refs
--
-- the default prism may be used for initialization and/or
-- test against uninitialized variables (_Default may construct an illegal value)
--
-- How these values are encoded in a machine value v is hidden
-- by the prisms, these act as selectors and as smart constructors

class WordValue v where
  _Word    :: Prism' v Word
  _Int     :: Prism' v Int
  _Bool    :: Prism' v Bool

  _Int = _Word . isoWordInt
    where
      isoWordInt = iso fromIntegral fromIntegral

  _Bool = _Word . isoWordBool
    where
      isoWordBool = iso (/= 0) (toEnum . fromEnum)

class DataRefValue v where
  _DataRef :: Prism' v (SegId, Offset)

class CodeRefValue v where
  _CodeRef :: Prism' v Offset

class DefaultValue v where
  _Default :: Prism' v ()

type CoreValue v = (WordValue v, DataRefValue v, CodeRefValue v, DefaultValue v)

-- ----------------------------------------
--
-- application specific values
--
-- doubles and strings as example

class DoubleValue v where
  _Double :: Prism' v String

class StringValue v where
  _String :: Prism' v String

-- ----------------------------------------
