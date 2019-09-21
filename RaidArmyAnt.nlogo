breed [ants ant]
;breed [foods food]

globals [foodCollected totalPheromones totalFood foodDiscovered]

ants-own[
  isHome
  carryingFood
  move]

patches-own[
  pheromones
  foodQuant
  numAnts
  foodDis
]

to countPheromones
  ask patches
  [
    if pheromones > 0
    [
     set totalPheromones totalPheromones + pheromones
    ]
  ]
end
;;observer methods
to setup
 clear-all
 reset-ticks
 set foodCollected 0
 set totalPheromones 0
 set foodDiscovered 0
 setupAnts
 setupFood
end

;;Setup the ants
to setupAnts
 create-ants numberAnts
 [
    setxy 0 5
   set shape "ant"
   set color red - 3
   set heading 0
   set isHome true
   set carryingFood false
   set move false
  ]

end

;;Setup food
to setupFood
  let num count patches
  let num2 (num * foodDensity) / 100
  ask n-of num2 patches[
    set foodQuant foodConc
    if foodVisibility
    [set pcolor yellow]
    set foodDis false
  ]
   set totalFood num2 * foodConc

end


;;sets the ants in the nest and  moves them
to go
  setupAnts
  moveAnts
  evapPheromones
  countPheromones
  tick
end

;;Evaporates pheromones held by the patches based on evaporation rate
to evapPheromones
  ask patches[
    ifelse (pheromones > 0)
    [
      ;set pheromones (pheromones - ((pheromones * pheromoneEvapRate) / 100))
      set pheromones (pheromones * (1 - (pheromoneEvapRate / 30)))
      ifelse pheromoeVisibility
      [ set pcolor scale-color green pheromones 0 1000]
      [ set pcolor black]
    ]
    [set pheromones 0]
  ]

end

;;this function moves the ants based on probabilities
to moveAnts
  ask ants [
    ifelse isHome
    [
      set color white
      set isHome false
      if carryingFood
      [
        set carryingFood false
        set foodCollected (foodCollected + 1)
      ]
      set heading 0
    ]
    [
      ifelse (abs(xcor) > max-pxcor - 1) or (ycor > max-pycor - 1)  or ( ycor < min-pycor + 1)
      [ die ]
      [
        ifelse carryingFood
        [set color blue
          ifelse (distancexy-nowrap 0 5 < 1) or (ycor < 5)
          [
            set xcor 0
            set ycor 5
            set isHome true
          ]
          [goHome]
        ]
        [lookForFood]
      ]
    ]
  ]

end



;;this function looks for food
to lookForFood
  if (foodQuant > 0) and not (foodDis)
  [
    set foodDis true
    if foodDis
    [set foodDiscovered (foodDiscovered + foodQuant)]
  ]
  ifelse((foodQuant > 0) and not (carryingFood))
  [
    set foodQuant (foodQuant - 1)
    if foodQuant <= 0
    [
      set pcolor black
    ]
    set carryingFood true
    set color blue
    set heading 180
    goHome
  ]
  [
    moveToFood
  ]
end

;This function moves the ant towards food
to moveToFood
  let probM 0
  let probMoveLeft 0
  let probMoveRight 0
  set color red
  set heading 0
  let pherR [pheromones] of patch-right-and-ahead 45 1
  let pherL [pheromones] of patch-left-and-ahead 45 1
  set probM calcMoveProb pherR pherL
  ;print ("The probability of movement is")
  ;print (probM)
  ;let numR ((random 100) / 100)
  let numR random-float 1
  ;print ("The probability of movement is Random")
  ;print (numR)
  if(numR < probM)
  ;if (ProbM > one-of [0 0.1 0.2 0.3])
  [
    set move true
    if(pheromones < 1000)
          [
            set pheromones pheromones + pheromoneOut
          ]
  ]

  ;set move true

  if move
  [
    set probMoveLeft calcProbLeft pherR pherL
    set probMoveRight (1 - probMoveLeft)
    ;print ("The probability of movement left is")
    ;print (probMoveLeft)

    ;let numR2 ((random 100) / 100)
    let numR2 random-float 1
    ;print ("The probability of movement is Random")
    ;print (numR2)
    ifelse (numR2 < probMoveLeft)
    [
      set heading 315
      ifelse (count ants-on patch-ahead 1 > antsPerSite)
      [
        set heading 45
        ifelse (count ants-on patch-ahead 1 > antsPerSite)
        [
          forward 0
        ]
        [
          forward 1.41421356
          set heading 0
        ]
      ]
      [
        forward 1.41421356
        set heading 0
      ]
    ]
    [
      set heading 45
      ifelse(count ants-on patch-ahead 1 > antsPerSite)
      [
        set heading 315
        ifelse(count ants-on patch-ahead 1 > antsPerSite)
        [forward 0]
        [
          forward 1.41421356
          set heading 0
       ]
      ]
      [
        forward 1.41421356
        set heading 0
      ]



    ]
  ]

set move false
end
;calculates the probability of Movement
to-report calcMoveProb [phR phL]
  let X (((phL + phR) / 100 ) - 1)
  ;if(x > 500)
  ;[set X 500]
  let tanh (((exp X) - (1 / exp X)) / ((exp X) + (1 / exp X)))
  let Pm ((1 + tanh) / 2 )
  report Pm
end

to-report calcProbLeft [phR phL]
  Let Y (1 / ( (K + phL) ^ N + (phR + k) ^ N ) )
  Let Pml ( Y * ((K + phL) ^ N ))
  report Pml
end


;;going home
to goHome

  let pheroRight 0
  let pheroLeft 0
  let moveProb 0
  let probMoveLeft 0
  let probMoveRight 0
  let sumProb 0

      set heading 180
      rt 45
      set pheroRight  [pheromones] of patch-ahead 1
      lt 90
      set pheroLeft  [pheromones] of patch-ahead 1
      set heading 180
      if move = false
      [
        set moveProb calcMoveProb pheroRight pheroLeft
        let localRandom ((random 100) / 100)
        if(localRandom < moveProb)
        [
          set move true
          if(pheromones < 300)
          [
            set pheromones pheromones + pheromoneIn
          ]
        ]
      ]
      if move = true
      [
        set probMoveLeft calcProbLeft pheroRight pheroLeft
        set probMoveRight 1 - ProbMoveLeft
        ;normalizing the probabilities
        set sumProb (probMoveLeft + probMoveRight)
        set probMoveLeft probMoveLeft / sumProb
        set probMoveRight probMoveRight / sumProb

        if (pheroLeft < (10 * 0.5))
          [ set probMoveLeft 0 ]
        if (pheroRight < (10 * 0.5))
            [ set probMoveRight 0 ]
        set sumProb (probMoveLeft + probMoveRight)

        ifelse (sumProb = 0) and (xCor < 0)
            [
              set heading 135
              forward 1
              set heading 180
              set move false
            ]
            [
              ifelse (sumProb = 0) and (xCor > 0)
              [
                set heading 225
                forward 1
                set heading 180
                set move false
              ]
              [
                ifelse (sumProb = 0)
                [
                  set heading 180
                  forward 1
                  set move false
                ]
                [
                 set probMoveLeft probMoveLeft / sumProb
                 set probMoveRight probMoveRight / sumProb

                ]
              ]
            ]

            let rNum ((random 100) / 100)
            if move
            [
              ifelse (probMoveLeft > rNum)
              [
                set heading 135
                ifelse (count ants-on patch-ahead 1 > antsPerSite)
                [
                  set heading 225
                  ifelse (count ants-on patch-ahead 1 > antsPerSite)
                  [ forward 0 ]
                  [
                   forward 1
                   set heading 180
                  ]
                ]
                [
                  forward 1
                  set heading 180
                ]
              ]
              [
                set heading 225
                ifelse (count ants-on patch-ahead 1 > antsPerSite)
                [
                 set heading 135
                 ifelse (count ants-on patch-ahead 1 > antsPerSite)
                 [
                  forward 0
                 ]
                 [
                  forward 1
                  set heading 180
                 ]
                ]
               [
                 forward 1
                 set heading 180
               ]
              ]
            ]
      ]
      set move false


end
@#$#@#$#@
GRAPHICS-WINDOW
398
32
728
643
-1
-1
2.0
1
10
1
1
1
0
1
1
1
-80
80
0
300
0
0
1
ticks
30.0

BUTTON
27
27
91
60
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
21
92
193
125
numberAnts
numberAnts
5
100
10.0
5
1
NIL
HORIZONTAL

SLIDER
20
324
192
357
foodConc
foodConc
1
500
1.0
1
1
NIL
HORIZONTAL

SLIDER
20
288
192
321
foodDensity
foodDensity
0
100
50.0
1
1
NIL
HORIZONTAL

SWITCH
220
95
373
128
foodVisibility
foodVisibility
0
1
-1000

SLIDER
21
233
193
266
pheromoneEvapRate
pheromoneEvapRate
0
30
1.0
1
1
NIL
HORIZONTAL

SWITCH
221
180
375
213
pheromoeVisibility
pheromoeVisibility
0
1
-1000

BUTTON
95
28
158
61
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
209
255
381
288
K
K
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
209
292
381
325
N
N
1
100
2.0
1
1
NIL
HORIZONTAL

BUTTON
162
28
225
61
Clear
clear-all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
817
68
874
113
NIL
Ticks
17
1
11

PLOT
761
137
961
287
Food Collected
Time
Food Collected
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plotxy ticks foodCollected"

SLIDER
20
163
192
196
pheromoneOut
pheromoneOut
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
20
198
192
231
pheromoneIn
pheromoneIn
0
100
10.0
1
1
NIL
HORIZONTAL

SLIDER
20
128
192
161
antsPerSite
antsPerSite
0
100
20.0
1
1
NIL
HORIZONTAL

MONITOR
766
381
836
426
Total Ants
count ants
17
1
11

MONITOR
764
434
859
479
Food Collected
foodCollected
17
1
11

MONITOR
763
488
868
533
Food Discovered
foodDiscovered
17
1
11

@#$#@#$#@
## WHAT IS IT?

A simulation of the forging behaviour of army ants based on a model proposed in the book "Swarm Intelligence" by Bonabeau et al.'s

## HOW IT WORKS
The assumptions are as follows:

1. The environment is considered as a bidirectional grid.

2. At every time-step a certain number of ants come out of the nest.

3. Ants deposit pheromones at every site they visit

4. All ants make sure that there are not more than 20 ants per site.

5. On the way out of the nest, an ant lays 1 unit of pheromones and does not exceed the limit of 1000 pheromones per site.

6. The probability Pm that an ant will move Pm is given by:

			Pm=(1/2)[1+tanh((pl+pr)/100-1) ]

	Where, pl and pm are the pheromone level on the left and right side of the ant respectively.

	If this pm is larger than a randomly generated number then the ant decides to move.

7. If an ant decides to move, then the ant will move left with the following probability

			Prob_l=(5+Pl)^2/((5+Pl)^2+ (5+Pr)^2 )
	If this Prob_l is larger than a randomly generated probability then it goes left or else it goes right.

8. Where, n is the degree of non-linearity and k is the degree of attraction.

9.  At every time step the pheromones of every site evaporates at a certain rate which is a fixed fraction of 1/30.

10. The environment is set-up with a certain food density and food concentration. 


## HOW TO USE IT
The GUI has various parameters that can be changed:

1. numberAnts: Number of ants coming out of the nest per unit time.

2. antsPerSite: Number of ants allowed per patch.

3. pheromoneOut: Units of pheromones laid by the ants on the way out of the nest.

4. pheromoneIn: Units of pheromones laid by the ants on the way back to the nest.

5. pheromoneEvapRate: Rate of evaporation of pheromones.

6. foodDensity: Percentage of area tha contains food.

7. foodConc: Units of food per patch that contains food.

8. K: Value of degree of attraction.

9. N: Value of degree of non-linearity. 

10. foodVisibility: toggle to display food.

11. pheromoeVisibility: toggle to display pheromones. 

12. Set  the variables and click Setup to setup the environment. 

13. Click Go to run the simulation.

14. Click Clear to clear the screen.

## THINGS TO NOTICE

The simulation has various monitors:

1. Ticks: Displays the unit of time passed.

2. Food Collected: Displays the amount of food collected by the colony with passing time.

3. Food Discovered: Displays the total food dicovered by the colony.

4. Total Ants: Displays the total number of ants generated throughout the system.
## THINGS TO TRY

Change the various variables and run the simulation.

## CREDITS AND REFERENCES

Bonabeau E., Dorigo M., Theraulaz G.- “Swarm Intelligence – From Natural to Artificial Systems”

Deneubourg, J.-L., S. Aron, S. Goss, and J.-M. Pasteels. “The Self-Organizing Exploratory Pattern of the Argentine Ant.” J. Insect Behavior 3 (1990): 159168
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

ant
true
0
Polygon -7500403 true true 136 61 129 46 144 30 119 45 124 60 114 82 97 37 132 10 93 36 111 84 127 105 172 105 189 84 208 35 171 11 202 35 204 37 186 82 177 60 180 44 159 32 170 44 165 60
Polygon -7500403 true true 150 95 135 103 139 117 125 149 137 180 135 196 150 204 166 195 161 180 174 150 158 116 164 102
Polygon -7500403 true true 149 186 128 197 114 232 134 270 149 282 166 270 185 232 171 195 149 186
Polygon -7500403 true true 225 66 230 107 159 122 161 127 234 111 236 106
Polygon -7500403 true true 78 58 99 116 139 123 137 128 95 119
Polygon -7500403 true true 48 103 90 147 129 147 130 151 86 151
Polygon -7500403 true true 65 224 92 171 134 160 135 164 95 175
Polygon -7500403 true true 235 222 210 170 163 162 161 166 208 174
Polygon -7500403 true true 249 107 211 147 168 147 168 150 213 150

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment38" repetitions="10" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>ticks</metric>
    <metric>count ants</metric>
    <metric>foodCollected</metric>
    <enumeratedValueSet variable="foodDensity">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foodVisibility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="antsPerSite">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pheromoneOut">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="K">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pheromoeVisibility">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pheromoneEvapRate">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="foodConc">
      <value value="400"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pheromoneIn">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberAnts">
      <value value="10"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
