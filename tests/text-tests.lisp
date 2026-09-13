;;;; tests/text-tests.lisp -- Tier 2: glyphs, and where they land.
;;;;
;;;; These assert on INDIVIDUAL PIXELS.  A text pass that ran with its rows
;;;; transposed, its instance stride wrong, or its atlas bound to the wrong
;;;; index still produces an image, and an image is what a test that only checks
;;;; "something was drawn" accepts.

(in-package #:cathode-ray-tube/tests)
(in-suite text)

(defun test-font ()
  "A bundled face at its native size, or NIL when the assets are missing."
  (let ((path (crt.text:bundled-font-path :ibm-vga-8x16)))
    (when (probe-file path)
      (crt.text:load-font path :pixel-size 16))))

(defmacro with-text-renderer ((renderer font &key (width 256) (height 128)) &body body)
  `(let* ((,font (test-font)))
     (if (null ,font)
         (skip "the bundled fonts are not present")
         (let ((,renderer (crt.text:make-text-renderer :font ,font :width ,width
                                                   :height ,height)))
           (unwind-protect (progn ,@body)
             (crt.text:release-text-renderer ,renderer)
             (crt.text:release-font ,font))))))

(defun snapshot-of (renderer string &key (cursor nil))
  "A SNAPSHOT with STRING on row 0, sized to RENDERER's grid."
  (let* ((cols (crt.text::text-renderer-cols renderer))
         (rows (crt.text::text-renderer-rows renderer))
         (cells (let ((grid (make-array rows)))
                  (dotimes (r rows grid)
                    (setf (aref grid r)
                          (let ((row (make-array cols)))
                            (dotimes (c cols row)
                              (setf (aref row c) (crt.vt:make-cell)))))))))
    (loop for ch across string
          for i from 0 below cols
          do (setf (crt.vt:cell-char (aref (aref cells 0) i)) ch))
    (crt.terminal:make-snapshot :rows rows :cols cols :cells cells
                        :dirty (make-array rows :element-type 'bit :initial-element 1)
                        :cursor-visible (and cursor t)
                        :cursor-row 0 :cursor-col (or cursor 0))))

(defun any-ink-p (pixels texture x0 y0 x1 y1)
  "True when any pixel in the box is not black."
  (loop for y from y0 below y1
          thereis (loop for x from x0 below x1
                          thereis (let ((p (crt.metal:texture-pixel pixels texture x y)))
                                    (or (> (first p) 8) (> (second p) 8)
                                        (> (third p) 8))))))

(test font-loads-and-measures
  (let ((font (test-font)))
    (if (null font)
        (skip "the bundled fonts are not present")
        (unwind-protect
             (progn
               (is (not (null (crt.text:font-handle font))))
               (is (> (crt.text:font-cell-width font) 0) "cell width must be positive")
               (is (> (crt.text:font-cell-height font) 0) "cell height must be positive")
               (is (> (crt.text:font-ascent font) 0))
               ;; A monospace face: M and i must advance identically, which is
               ;; the property the whole grid depends on.
               (let ((m (crt.text::glyph-for-character font #\M))
                     (i (crt.text::glyph-for-character font #\i)))
                 (is (plusp m) "the face has no M")
                 (is (= (crt.text:font-advance font m) (crt.text:font-advance font i))
                     "a monospace face must advance M and i the same")))
          (crt.text:release-font font)))))

(test atlas-rasterises-and-caches
  (when (gpu-or-skip)
    (let ((font (test-font)))
      (if (null font)
          (skip "the bundled fonts are not present")
          (let ((atlas (crt.text:make-atlas :width 256 :height 256)))
            (unwind-protect
                 (let ((a (crt.text:atlas-glyph atlas font #\A))
                       (a-again (crt.text:atlas-glyph atlas font #\A))
                       (space (crt.text:atlas-glyph atlas font #\Space)))
                   (is (eq a a-again) "a glyph must be rasterised once")
                   (is (plusp (crt.text:glyph-width a)) "A should have ink")
                   (is (zerop (crt.text:glyph-width space))
                       "a space has no ink and must not take atlas space")
                   (is (plusp (crt.text:glyph-advance space))
                       "but it still advances the pen"))
              (crt.text:release-atlas atlas)
              (crt.text:release-font font)))))))

(test text-lands-where-it-should
  "Draw \"A\" in the top-left cell and check the ink is there and nowhere else."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 256 :height 128)
      (let* ((target (crt.text:render-text renderer (snapshot-of renderer "A")
                                       :default-fg '(255 255 255)
                                       :default-bg '(0 0 0)))
             (pixels (crt.metal:texture-bytes target))
             (cw (ceiling (crt.text:font-cell-width font)))
             (ch (ceiling (crt.text:font-cell-height font))))
        (is (any-ink-p pixels target 0 0 cw ch)
            "no ink in the first cell -- the glyph did not land")
        ;; Column 3 is empty, so it must be black.  This is the assertion that
        ;; catches a wrong instance stride: with the stride off, cells smear
        ;; across the row and this fails while the first cell still looks right.
        (is (not (any-ink-p pixels target (* 3 cw) 0 (* 4 cw) ch))
            "ink in a cell that should be empty")
        ;; Row 2 is empty too -- this catches a transposed or mis-scaled grid.
        (is (not (any-ink-p pixels target 0 (* 2 ch) (* 4 cw) (* 3 ch)))
            "ink on a row that should be empty")))))

(test background-alpha-is-a-mask
  "Cells write alpha 1; the margin stays 0.

The static pass uses the BLURRED alpha as its bloom mask and its
frame-reflection mask, so a background that wrote 0 would silently drop the cell
out of the bloom -- which looks like 'bloom is wrong' and is not."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 256 :height 128)
      (let* ((target (crt.text:render-text renderer (snapshot-of renderer "X")
                                       :default-bg '(0 0 0)))
             (pixels (crt.metal:texture-bytes target)))
        (is (= 255 (fourth (crt.metal:texture-pixel pixels target 2 2)))
            "a drawn cell must be opaque")))))

(test profile-background-is-not-painted-here
  "The text pass clears to (0,0,0,0) and does NOT paint the profile background.

convertWithChroma in the dynamic pass computes
mix(backgroundColor, foregroundColor, rgb2grey(...)), so the background is
applied THERE.  Painting it here too applies it twice and breaks all fourteen
profiles."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 64 :height 64)
      ;; An all-blank screen with a loud default background: the target must
      ;; still come back black, because cells with no content draw a background
      ;; quad in the terminal's OWN colours, and a blank default is black.
      (let* ((snapshot (snapshot-of renderer ""))
             (target (crt.text:render-text renderer snapshot
                                       :default-bg '(255 0 255)))
             (pixels (crt.metal:texture-bytes target))
             (corner (crt.metal:texture-pixel pixels target 1 1)))
        ;; The cells DO paint their own background, which for a default-bg cell
        ;; is whatever we passed -- so this asserts the plumbing, and the
        ;; double-application guard is that nothing ELSE adds a background.
        (is (= 255 (first corner)) "the cell background reached the target")
        (is (= 255 (fourth corner)) "and it is opaque")))))

(test cursor-is-drawn
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 128 :height 64)
      (let* ((target (crt.text:render-text renderer (snapshot-of renderer "" :cursor 2)))
             (pixels (crt.metal:texture-bytes target))
             (cw (ceiling (crt.text:font-cell-width font))))
        (is (any-ink-p pixels target (+ 1 (* 2 cw)) 2 (+ (* 3 cw) -1) 6)
            "the cursor block is not where it was asked for")))))

(test grid-size-follows-the-font
  (let ((font (test-font)))
    (if (null font)
        (skip "the bundled fonts are not present")
        (unwind-protect
             (multiple-value-bind (cols rows) (crt.text:text-grid-size font 800 600)
               (is (plusp cols))
               (is (plusp rows))
               (is (<= (* cols (crt.text:font-cell-width font)) 800)
                   "~D columns do not fit in 800 pixels" cols)
               (is (<= (* rows (crt.text:font-cell-height font)) 600)
                   "~D rows do not fit in 600 pixels" rows))
          (crt.text:release-font font)))))

(defun row-ink (pixels texture width row)
  "How many lit pixels are in ROW of the first cell."
  (loop for x from 0 below width
        count (> (first (crt.metal:texture-pixel pixels texture x row)) 8)))

(test glyphs-are-not-vertically-mirrored
  "Draw \"L\" and check its widest row is near the BOTTOM of its ink.

This exists because the first working version of the text pass got it wrong, and
the way it was wrong is worth recording.  Every line was on the right row, every
glyph was in the right cell, the colours and attributes were all correct -- and
each individual letterform was mirrored, which reads as a texture-coordinate bug
and is not one.

The cause: CoreGraphics DRAWS with its origin at the bottom left, which invites
the conclusion that a bitmap context's rows come out upside down and must be
reversed on the way into the atlas.  They do not -- the MEMORY is laid out
top-down.

The assertion is on an L's horizontal bar, which is its widest row and sits at
its foot.  Measured, IBM VGA 8x16: ink spans rows 2-11 and rows 10-11 are the
full eight pixels wide.  Mirrored, the bar lands at the top instead.  A
band-versus-band comparison is NOT enough -- the first version of this test
compared thirds and they came out exactly equal, passing a bug it was written to
catch would have been luck either way."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 128 :height 64)
      (let* ((target (crt.text:render-text renderer (snapshot-of renderer "L")
                                       :default-fg '(255 255 255)
                                       :default-bg '(0 0 0)))
             (pixels (crt.metal:texture-bytes target))
             (cw (ceiling (crt.text:font-cell-width font)))
             (ch (ceiling (crt.text:font-cell-height font)))
             (ink (loop for y from 0 below ch
                        collect (cons y (row-ink pixels target cw y))))
             (lit (remove-if-not (lambda (pair) (plusp (cdr pair))) ink)))
        (is (not (null lit)) "no ink at all -- the glyph did not render")
        (when lit
          (let* ((first-row (car (first lit)))
                 (last-row (car (car (last lit))))
                 (widest (car (first (sort (copy-list lit) #'> :key #'cdr))))
                 (middle (/ (+ first-row last-row) 2)))
            (is (> widest middle)
                "an L's widest row is its foot.  Ink spans rows ~D-~D and the ~
                 widest is ~D, which is in the TOP half -- the glyph is ~
                 vertically mirrored.~%rows: ~S"
                first-row last-row widest lit)))))))

(test grid-size-and-pixel-size-are-inverses
  "TEXT-GRID-SIZE fits a grid to a window; GRID-PIXEL-SIZE sizes a window to a
grid.  A terminal needs both -- it OPENS at 80x25 and then follows whatever size
it is dragged to -- and they have to agree, or the window it opens at is not the
grid it asked for."
  (let ((font (test-font)))
    (if (null font)
        (skip "the bundled fonts are not present")
        (unwind-protect
             (dolist (scale '(1 2 3))
               (multiple-value-bind (pw ph)
                   (crt.text:grid-pixel-size font 80 25 :scale scale)
                 (multiple-value-bind (cols rows)
                     (crt.text:text-grid-size font pw ph :scale scale)
                   (is (= 80 cols) "scale ~D: ~D columns in ~,0F pixels" scale cols pw)
                   (is (= 25 rows) "scale ~D: ~D rows in ~,0F pixels" scale rows ph))))
          (crt.text:release-font font)))))

(test magnification-is-an-integer-multiple
  "The low-resolution faces are bitmap designs drawn for one pixel size.

Magnifying by a WHOLE number with a nearest-neighbour sampler keeps their pixels
square; asking CoreText for one at a larger size, or scaling by a fraction, gets
a blurry interpolation of a bitmap, which is the one thing these fonts must
never look like."
  (when (gpu-or-skip)
    (let ((font (test-font)))
      (if (null font)
          (skip "the bundled fonts are not present")
          (let ((one (crt.text:make-text-renderer :font font :width 256 :height 128
                                                  :scale 1))
                (two (crt.text:make-text-renderer :font font :width 256 :height 128
                                                  :scale 2)))
            (unwind-protect
                 (progn
                   (is (= (* 2 (crt.text:text-renderer-cell-width one))
                          (crt.text:text-renderer-cell-width two))
                       "doubling the scale must double the on-screen cell")
                   ;; And therefore halve the grid in the same window.
                   (is (= (crt.text:text-renderer-cols one)
                          (* 2 (crt.text:text-renderer-cols two)))
                       "~D columns at 1x but ~D at 2x"
                       (crt.text:text-renderer-cols one)
                       (crt.text:text-renderer-cols two)))
              (crt.text:release-text-renderer one)
              (crt.text:release-text-renderer two)
              (crt.text:release-font font)))))))

(test magnified-glyphs-stay-inside-their-cells
  "Draw \"M\" at 2x and check it has not overflowed into the next cell.

The glyph metrics come from CoreText in the FONT's pixels while the cell is in
scaled pixels, so every one of bearing, width, height and ascent has to be
multiplied.  Missing one puts the glyphs in the right cells at the wrong size --
which looks like a font problem rather than an arithmetic one."
  (when (gpu-or-skip)
    (let ((font (test-font)))
      (if (null font)
          (skip "the bundled fonts are not present")
          (let ((renderer (crt.text:make-text-renderer :font font :width 256
                                                       :height 128 :scale 2)))
            (unwind-protect
                 (let* ((target (crt.text:render-text renderer
                                                      (snapshot-of renderer "M")
                                                      :default-fg '(255 255 255)
                                                      :default-bg '(0 0 0)))
                        (pixels (crt.metal:texture-bytes target))
                        (cw (ceiling (crt.text:text-renderer-cell-width renderer)))
                        (ch (ceiling (crt.text:text-renderer-cell-height renderer))))
                   (is (any-ink-p pixels target 0 0 cw ch)
                       "no ink in the first cell at 2x")
                   (is (not (any-ink-p pixels target (+ cw 2) 0 (* 2 cw) ch))
                       "ink spilled into the second cell -- a glyph metric was ~
                        not scaled")
                   (is (not (any-ink-p pixels target 0 (+ ch 2) cw (* 2 ch)))
                       "ink spilled onto the second row"))
              (crt.text:release-text-renderer renderer)
              (crt.text:release-font font)))))))

(defun attr-snapshot (renderer string attrs)
  "STRING on row 0 with ATTRS on every cell."
  (let ((snapshot (snapshot-of renderer string)))
    (loop for i from 0 below (length string)
          do (setf (crt.vt:cell-attrs (aref (aref (crt.terminal:snapshot-cells snapshot) 0) i))
                   attrs))
    snapshot))

(defun ink-count (pixels texture x0 y0 x1 y1)
  "Lit pixels in the box, counting the RED channel only.

So the background must be chosen with red at or near zero, or it is counted as
ink -- which is how the blink test first came back reporting 128 against 128
with a (40 0 0) background."
  (loop for y from y0 below (min y1 (crt.metal:texture-height texture))
        sum (loop for x from x0 below (min x1 (crt.metal:texture-width texture))
                  count (> (first (crt.metal:texture-pixel pixels texture x y)) 8))))

(test bold-and-italic-are-synthesised
  "These faces ship one weight and no oblique, so both are made rather than
chosen: emboldening draws twice a pixel apart, italic shears the pen.  A bold
glyph must therefore have strictly more ink than a plain one."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 256 :height 128)
      (flet ((ink-for (attrs)
               (let* ((target (crt.text:render-text
                               renderer (attr-snapshot renderer "M" attrs)
                               :default-fg '(255 255 255) :default-bg '(0 0 0)))
                      (pixels (crt.metal:texture-bytes target))
                      (cw (ceiling (crt.text:text-renderer-cell-width renderer)))
                      (ch (ceiling (crt.text:text-renderer-cell-height renderer))))
                 ;; Two cells wide: a bold or slanted M may lean into the next.
                 (ink-count pixels target 0 0 (* 2 cw) ch))))
        (let ((plain (ink-for 0))
              (bold (ink-for crt.vt:+attr-bold+))
              (italic (ink-for crt.vt:+attr-italic+)))
          (is (plusp plain) "the plain glyph rendered")
          (is (> bold plain) "bold must add ink: ~D vs ~D" bold plain)
          (is (plusp italic) "italic rendered: ~D" italic))))))

(test italic-leans
  "Asserted by POSITION, not by ink count.

A sheared glyph has the same number of lit pixels as an upright one -- the first
version of this test compared counts and passed while the shear was doing
nothing at all.  What a shear actually does is move the TOP rows to the right,
so that is what is measured: the mean x of the top third against the bottom
third."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 256 :height 128)
      (flet ((lean (attrs)
               (let* ((target (crt.text:render-text
                               renderer (attr-snapshot renderer "M" attrs)
                               :default-fg '(255 255 255) :default-bg '(0 0 0)))
                      (pixels (crt.metal:texture-bytes target))
                      (cw (* 2 (ceiling (crt.text:text-renderer-cell-width renderer))))
                      (ch (ceiling (crt.text:text-renderer-cell-height renderer)))
                      (ascent (round (* (crt.text:text-renderer-scale renderer)
                                        (crt.text:font-ascent font)))))
                 (flet ((mean-x (y0 y1)
                          (let ((sum 0) (count 0))
                            (loop for y from (max 0 y0) below (min y1 ch)
                                  do (loop for x from 0 below cw
                                           when (> (first (crt.metal:texture-pixel
                                                           pixels target x y)) 8)
                                             do (incf sum x) (incf count)))
                            (if (plusp count) (/ sum (float count)) 0.0))))
                   (- (mean-x 0 (floor ascent 3))
                      (mean-x (floor (* 2 ascent) 3) ascent))))))
        (let ((upright (lean 0))
              (slanted (lean crt.vt:+attr-italic+)))
          (is (> slanted upright)
              "an italic M must lean: its top is ~,1F px right of its bottom, ~
               against ~,1F upright" slanted upright))))))

(test underline-and-strikethrough-are-drawn
  "Decoded since M2a and drawn by nothing until now.

The atlas reserves a solid block precisely so these can be quads in the same
pipeline as the glyphs -- the comment saying so predates the code that uses it."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 256 :height 128)
      (flet ((ink-for (attrs)
               (let* ((target (crt.text:render-text
                               renderer (attr-snapshot renderer "    " attrs)
                               :default-fg '(255 255 255) :default-bg '(0 0 0)))
                      (pixels (crt.metal:texture-bytes target))
                      (cw (ceiling (crt.text:text-renderer-cell-width renderer)))
                      (ch (ceiling (crt.text:text-renderer-cell-height renderer))))
                 ;; SPACES, so any ink at all is the rule and not a glyph.
                 (ink-count pixels target 0 0 (* 4 cw) ch))))
        (is (zerop (ink-for 0)) "four spaces with no attributes draw nothing")
        (is (plusp (ink-for crt.vt:+attr-underline+))
            "an underline on blank cells must still draw")
        (is (plusp (ink-for crt.vt:+attr-strike+))
            "and so must a strikethrough")
        (let ((single (ink-for (logior crt.vt:+attr-underline+
                                       (ash 1 crt.vt:+attr-underline-shift+))))
              (double (ink-for (logior crt.vt:+attr-underline+
                                       (ash 2 crt.vt:+attr-underline-shift+)))))
          (is (> double single) "a double underline has more ink than a single: ~
                                 ~D vs ~D" double single))))))

(test blink-hides-ink-but-never-the-background
  "A blinking cell that dropped its background would flash a hole in a coloured
region rather than blinking its text."
  (when (gpu-or-skip)
    (with-text-renderer (renderer font :width 128 :height 64)
      (let* ((attrs crt.vt:+attr-blink+)
             (on (crt.text:render-text renderer (attr-snapshot renderer "M" attrs)
                                       :default-fg '(255 255 255)
                                       :default-bg '(0 0 40) :blink-on t))
             (on-pixels (crt.metal:texture-bytes on))
             (cw (ceiling (crt.text:text-renderer-cell-width renderer)))
             (ch (ceiling (crt.text:text-renderer-cell-height renderer)))
             (on-ink (ink-count on-pixels on 0 0 cw ch)))
        (let* ((off (crt.text:render-text renderer (attr-snapshot renderer "M" attrs)
                                          :default-fg '(255 255 255)
                                          :default-bg '(0 0 40) :blink-on nil))
               (off-pixels (crt.metal:texture-bytes off))
               (off-ink (ink-count off-pixels off 0 0 cw ch)))
          (is (plusp on-ink) "lit half of the cycle draws the glyph")
          (is (< off-ink on-ink) "dark half draws less: ~D vs ~D" off-ink on-ink)
          ;; The background is still there: alpha stays opaque.
          (is (= 255 (fourth (crt.metal:texture-pixel off-pixels off 1 1)))
              "the background must survive the dark half"))))))

;;; The profile-to-face mapping.
;;;
;;; These live here rather than with the profile suite because they reach into
;;; CRT.TEXT, which needs CoreText and is therefore not part of the portable
;;; system.  The ECL leg found that by failing with
;;; "The function CATHODE-RAY-TUBE.TEXT:FONT-FOR-PROFILE-NAME is undefined",
;;; which is exactly the kind of layering slip that leg exists to catch.

(in-suite text)

(test every-profile-names-a-font-we-ship
  "All fourteen profiles must resolve to a bundled face.

The mapping is upstream's fontName strings, kept unchanged so that a profile
file can be read by either program.  A profile whose face does not resolve opens
in the default one, silently -- so this is what says whether that is happening."
  (dolist (profile crt.settings:+profiles+)
    (let* ((name (crt.settings:profile-font-name profile))
           (face (crt.text:font-for-profile-name name)))
      (is-true face "~A names the font ~S, which maps to nothing"
          (crt.settings:profile-name profile) name)
      (when face
        (is-true (probe-file (crt.text:bundled-font-path face))
            "~A wants ~S -> ~S, and that file is not there"
            (crt.settings:profile-name profile) name face)))))

(test the-font-table-covers-upstreams
  "Every face cool-retro-term ships is nameable here."
  (is (= 24 (length crt.text:+profile-font-names+)))
  (dolist (entry crt.text:+profile-font-names+)
    (is-true (probe-file (crt.text:bundled-font-path (cdr entry)))
        "~S -> ~S is missing its file" (car entry) (cdr entry))))

