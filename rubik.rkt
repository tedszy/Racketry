;;;
;;; rubik.rkt
;;; 
;;; A study of Rubik's cube.
;;;

#lang racket 

(provide (all-defined-out))


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
;; up pointing up, front pointing toward you, back pointing away from you, etc. 
;; This is a very important point! You've got to imagine this carefully!

;; ---------------------------------------------------------------------

;; The next thing you notice when you examine your Rubik's cube is that 
;; it is made of many little cubes. We will call these little cubes *minicubes*.
;; Count them: there are 26 minicubes in all:
;;
;;   * 9 on the front layer.
;;   * 8 on the middle layer.
;;   * 9 on the back layer.
;;
;; Of these, the 6 center cubes are immovable, the other 20 are moveable.

;; These minicubes are of three types:
;;
;;   * center cubes
;;   * edge cubes
;;   * corner cubes.










;; ----------------------------------------------------------------------

;; Turning a face cycles the facets of that face and also of the 4 adjacent 
;; faces. That's why there are 5 cycles involved in the definition of a generator.
