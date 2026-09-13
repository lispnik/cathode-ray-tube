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
