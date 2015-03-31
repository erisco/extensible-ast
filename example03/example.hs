module Main where

data X

data X'

x'_subtypes_x :: X' -> X
x'_subtypes_x = undefined

f :: (() -> X) -> X
f = undefined

g :: (() -> X') -> X'
g = undefined

-- Problem: impossible to turn f into g even though X' subtypes X!
--
-- Note: we can turn g into f
--


class Y a

class (Y a) => (Y' a)

j :: (Y a) => (() -> a) -> a
j = undefined

k :: (Y' a) => (() -> a) -> a
k = undefined

-- This version with classes works as desired.
-- typeof j ~ typeof k = typeof k
--
-- Note: we cannot turn k into j.


data Z subtype = Z { z_subtype :: subtype }

data Z'data subtype = Z'data { z'_subtype :: subtype }

type Z' subtype = Z (Z'data subtype)


z'_subtypes_z :: Z' subtype -> Z subtype
z'_subtypes_z = Z . z'_subtype . z_subtype

m :: (() -> Z subtype) -> Z subtype
m = undefined

n :: (() -> Z' subtype) -> Z' subtype
n = undefined

-- This version also works as desired.
-- typeof m ~ typeof n = typeof n
--
-- Note: we can turn n into m ...


data Q'data subtype

type Q subtype = Z (Q'data subtype)

q_subtypes_z :: Q subtype -> Z subtype
q_subtypes_z = undefined

-- Problem: now how do we subtype both Z' and Q?


main :: IO ()
main = undefined
