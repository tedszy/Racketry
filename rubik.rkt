;;;
;;; rubik.rkt
;;; 
;;; A study of Rubik's cube.
;;;

#lang racket 

(provide (all-defined-out))

;; Cube organization:
;;
;; layers:
;;    front - middle - back
;;    left - middle - right
;;    top - middle - bottom

;; This is what my cube looks like.
;;
;;    w = white,  r = red,  g = green, o = orange, b = blue, y = yellow.
;;
;; Putting the cube with white upward, green in front, and red on the left, 
;; we have:
;;
;;                 +--------------+
;;                 |  w    w    w |
;;                 |              |
;;                 |  w   up    w |
;;                 |              |
;;                 |  w    w    w |
;;  +--------------+--------------+--------------+--------------+
;;  |  r    r    r |  g    g    g |  o    o    o |  b    b    b |
;;  |              |              |              |              |
;;  |  r  left   r |  g  front  g |  o  right  o |  b  back   b |
;;  |              |              |              |              |
;;  |  r    r    r |  g    g    g |  o    o    o |  b    b    b |
;;  +--------------+--------------+--------------+--------------+
;;                 |  y    y    y |
;;                 |              |
;;                 |  y  down   y |
;;                 |              |
;;                 |  y    y    y |
;;                 +--------------+

;; And now we can attach little lables to the colored facets. This gives
;; us a standard labeling scheme where each facet gets a unique ID number.

;; Here the initial position (start position) of Rubik's cube. The cube is 
;; unfolded and laid out as a 2D map. When you scramble your cube, the 
;; ID numbers end up all over the place. Try it yourself by sticking 
;; little labels on your cube.

;;                 +--------------+
;;                 | 1     2    3 |
;;                 |              |
;;                 | 4    up    5 |
;;                 |              |
;;                 | 6     7    8 |
;;  +--------------+--------------+--------------+--------------+
;;  |  9   10   11 | 17   18   19 | 25   26   27 | 33   34   35 |
;;  |              |              |              |              |
;;  | 12  left  13 | 20 front  21 | 28 right  29 | 36  back  37 |
;;  |              |              |              |              |
;;  | 14   15   16 | 22   23   24 | 30   31   32 | 38   39   40 |
;;  +--------------+--------------+--------------+--------------+
;;                 | 41   42   43 |
;;                 |              |
;;                 | 44  down  45 |
;;                 |              |
;;                 | 46   47   48 |
;;                 +--------------+

;; Each facet has an ID number, EXCEPT for the center cube facets. 
;; We don't need to give them labels or ID numbers.
;;
;; Rubik's cube has 54 facets. Of these, 6 are attached to center cubes. 
;; The center cubes NEVER MOVE. So there are only 54 - 6 = 48 moveable 
;; facets. These are the objects that can be permuted, so these are the 
;; only ones that need ID labels. 
;;
;; We *can* use a 54 element permutation vector to represent a cube, 
;; but there's no point carrying those 6 facets that are always fixed.

;; Up, down, left, right, back faces can ALWAYS be identified by looking 
;; at the center face! So even if you rotated your cube as you have been 
;; playing with it, you can ALWAYS return it to the standard orientation: 
;; up pointing up, front pointing toward you, back pointing away from you. 
;; This is a very important point! You've got to imagine this carefully!

;; ---------------------------------------------------------------------

;; The next thing you notice when you examine your Rubik's cube is that 
;; it is made of many little cubes. We will call these little cubes
;; *minicubes*. Count them: there are 26 minicubes in all:
;;
;;   * 9 on the front layer.
;;   * 8 on the middle layer.
;;   * 9 on the back layer.
;;
;; These minicubes are of three types:
;;
;;   * 6 center cubes, 1 facet each.
;;   * 12 edge cubes, 2 facets each.
;;   * 8 corner cubes, 3 facets each.
;;
;; Of these, the 6 center cubes are immovable, the other 20 are moveable.
;; We don't consider the center minicubes because, again, they don't move.
;;
;; Corner cubes:
;;
;;   Up face:
;;   1,9,35  <=> white, red, blue
;;   3,27,33 <=> white, orange, blue
;;   8,19,25 <=> white, orange green
;;   6,11,17 <=> white, red, green
;;
;;   Down face:
;;   14,40,46 <=> red, blue, yellow
;;   16,22,41 <=> red, green, yellow
;;   24,30,43 <=> green, orange, yellow
;;   32,38,48 <=> orange, blue, yellow
;;

;; Edge cubes:
;;
;; Top layer:
;;   2,34 <=> white, blue
;;   4,10 <=> white, red
;;   5,26 <=> white, orange
;;   7,18 <=> white, green
;;
;; Middle layer:
;;   12,37 <=> red, blue
;;   13,29 <=> red, green
;;   21,28 <=> green, orange
;;   29,36 <=> orange, blue
;;
;; Bottom layer:
;;   15,44 <=> red, yellow
;;   23,42 <=> green, yellow
;;   31,45 <=> orange, yellow
;;   39,47 <=> blue, yellow




;; ----------------------------------------------------------------------

;; Turning a face cycles the facets of that face and also facets of the
;; 4 adjacent faces. Therefore there are 5 cycles involved in the
;; definition of a generator.
