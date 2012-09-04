module Core.Examples where

import Core.Parser

Right example1 = parse "\
\ id x = x             ;\
\ main = id {1,0}       \
\ "

Right example2 = parse "\
\ s x y z = x z (y z)  ;\
\ k x y   = x          ;\
\                       \
\ main = s k k {2,0}    \
\ "

Right example3 = parse "      \
\ id x      = x              ;\
\ twice f x = f (f x)        ;\
\                             \
\ main = twice twice id {3,0} \
\ "

Right example4 = parse "  \
\ id x      = x          ;\
\ twice f x = f (f x)    ;\
\                         \
\ main = let f = twice id \
\        in twice f {4,0} \
\ "

Right example5 = parse "    \
\ k x y = x                ;\
\ y f   = let x = f x in x ;\
\ main  = y k {5,0}         \
\ "

Right example6 = parse "\
\ main = case {1,0} of  \
\   <0> -> {5,0}        \
\   <1> -> {6,0}        \
\ "

Right example7 = parse "   \
\ pair x y = {0,2} x y    ;\
\ main = pair {1,0} {7,0}  \
\ "

Right example8 = parse "\
\ false = {0,0}        ;\
\ true  = {1,0}        ;\
\                       \
\ and x y = case x of   \
\   <0> -> false        \
\   <1> -> y           ;\
\                       \
\ main = and true true  \
\ "

Right example9 = parse "          \
\ zero   = {0,0}                 ;\
\ succ n = {1,1} n               ;\
\                                 \
\ nil       = {0,0}              ;\
\ cons x xs = {1,2} x xs         ;\
\ length l  = case l of           \
\   <0>      -> zero              \
\   <1> x xs -> succ (length xs) ;\
\                                 \
\ main = length nil               \
\ "
