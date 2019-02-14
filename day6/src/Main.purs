module Main where

import Prelude

import Day6 as Day6
import Effect (Effect)
import Effect.Console (log)

input :: String
input = """66, 204
55, 226
231, 196
69, 211
69, 335
133, 146
321, 136
220, 229
148, 138
42, 319
304, 181
101, 329
72, 244
242, 117
83, 237
169, 225
311, 212
348, 330
233, 268
99, 301
142, 293
239, 288
200, 216
44, 215
353, 289
54, 73
73, 317
55, 216
305, 134
343, 233
227, 75
139, 285
264, 179
349, 263
48, 116
223, 60
247, 148
320, 232
60, 230
292, 78
247, 342
59, 326
333, 210
186, 291
218, 146
205, 246
124, 204
76, 121
333, 137
117, 68"""

main :: Effect Unit
main = do
  log $ show $ input # Day6.parseInput <#> (Day6.findClosestCoords >>> Day6.largestFiniteArea)
  log $ show $ input # Day6.parseInput <#> (Day6.sumDistancesToEachCoord >>> Day6.areaWithDistanceLessThan 10000)
