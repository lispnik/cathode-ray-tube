;;;; tools/gallery.lisp -- every profile, rendered headlessly.
;;;;
;;;; The whole chain into an offscreen texture rather than a drawable, so this
;;;; needs a GPU but no window.  It is how the port is judged -- fourteen
;;;; pictures beside fourteen pictures -- and the beginning of the fidelity
;;;; comparison against cool-retro-term itself.
;;;;
;;;;     make gallery                 -> docs/gallery/*.png
;;;;
;;;; EVERY IMAGE IS THE SAME SIZE and the grid is whatever fits, rather than
;;;; every image being 80x25 and the sizes varying.  The faces differ enormously
;;;; -- PetMe is an 8-pixel cell, Iosevka at 32 is nearer 19 -- so a fixed grid
;;;; would produce images from 640 to 1520 pixels wide, which is useless to look
;;;; at side by side and says nothing the font table does not.  A fixed window
;;;; is also what the application actually does.

(defpackage #:crt-gallery
  (:use #:cl)
  (:export #:render-profile #:render-all))

(in-package #:crt-gallery)

(defparameter +sample+
  '("cathode-ray-tube 0.1.0"
    ""
    "$ ls -la"
    "total 48"
    "drwxr-xr-x  18 mkennedy  staff   576 Sep 13 00:00 ."
    "drwxr-xr-x   6 mkennedy  staff   192 Sep 13 00:00 .."
    "drwxr-xr-x  38 mkennedy  staff  1216 Sep 13 00:00 src"
    "-rw-r--r--   1 mkennedy  staff  4096 Sep 13 00:00 README.md"
    ""
    "$ echo 'the quick brown fox jumps over the lazy dog'"
    "the quick brown fox jumps over the lazy dog"
    "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
    "0123456789 !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
    ""
    "$ ")
  "Something with the texture of real terminal output: a prompt, a listing,
mixed case, punctuation and whitespace.  A screen of lorem ipsum looks fine
under effects that a real session shows up -- the listing's columns are what
make jitter and horizontal sync visible, and the punctuation row is what shows
whether a face has been magnified or interpolated.")

(defun sample-snapshot (renderer &key cursor)
  (let* ((cols (crt.text:text-renderer-cols renderer))
         (rows (crt.text:text-renderer-rows renderer))
         (cells (let ((grid (make-array rows)))
                  (dotimes (r rows grid)
                    (setf (aref grid r)
                          (let ((row (make-array cols)))
                            (dotimes (c cols row)
                              (setf (aref row c) (crt.vt:make-cell)))))))))
    (loop for line in +sample+
          for r from 0 below rows
          do (loop for ch across line
                   for c from 0 below cols
                   do (setf (crt.vt:cell-char (aref (aref cells r) c)) ch)))
    (crt.terminal:make-snapshot
     :rows rows :cols cols :cells cells
     :dirty (make-array rows :element-type 'bit :initial-element 1)
     :cursor-visible (and cursor t)
     :cursor-row (min (1- rows) (1- (length +sample+)))
     :cursor-col 2
     :painted t)))

(defun profile-face (profile)
  "(values FACE SCALE) for PROFILE.

The face comes from the profile's own fontName; the scale is 2 for a
low-resolution face, matching what a Retina display gets, and 1 for the modern
ones, which are outlines rasterised at the size they are drawn."
  (let ((face (or (crt.text:font-for-profile-name
                   (crt.settings:profile-font-name profile))
                  :ibm-vga-8x16)))
    (values face (if (crt.text:bundled-font-low-resolution-p face) 2 1))))

(defun render-profile (profile &key (width 1000) (height 620) (time 1.37d0))
  "The whole chain, into a texture.  The caller releases it."
  (crt.ui:ensure-appkit)
  (multiple-value-bind (face scale) (profile-face profile)
    (let* ((font (crt.text:load-bundled-font
                  face :pixel-size (if (= scale 1)
                                       ;; 32 * baseFontScaling, which is what
                                       ;; upstream renders a modern face at.
                                       24
                                       (crt.text:bundled-font-native-size face))))
           (renderer (crt.text:make-text-renderer
                      :font font :width width :height height :scale scale
                      :line-spacing (crt.settings:profile-line-spacing profile)
                      ;; The same two the application uses, so a gallery picture
                      ;; is the picture: fontWidth is 1.25 on both Commodores and
                      ;; the fallback chain is what keeps a 128-character face
                      ;; from drawing blanks.
                      :font-width (crt.settings:profile-font-width profile)
                      :fallbacks (crt.text:font-fallback-chain
                                  face
                                  :pixel-size (crt.text:font-pixel-size font))))
           (graph (crt.effects:make-graph :profile profile :width width
                                          :height height))
           (target (crt.metal:make-texture :width width :height height
                                           :label "gallery")))
      (unwind-protect
           (let ((text (crt.text:render-text renderer
                                             (sample-snapshot renderer :cursor t)
                                             :default-fg '(255 255 255)
                                             :default-bg '(0 0 0))))
             ;; TWICE.  The burn-in accumulator starts empty, so one frame shows
             ;; no trail at all and the burn-in-heavy profiles would look
             ;; identical to the rest -- which is exactly the difference these
             ;; pictures exist to show.
             (multiple-value-bind (vw vh)
                 (crt.text:text-renderer-virtual-size renderer)
               (dotimes (i 2)
                 (crt.effects:render-effects graph text target
                                             :time (+ time (* i 0.016d0)) :painted t
                                             :virtual-width vw
                                             :virtual-height vh)))
             (values target
                     (crt.text:text-renderer-cols renderer)
                     (crt.text:text-renderer-rows renderer)
                     face scale))
        (crt.effects:release-graph graph)
        (crt.text:release-text-renderer renderer)
        (crt.text:release-font font)))))

(defun profile-file-name (profile)
  "A filename for PROFILE: lowercase, hyphenated, punctuation dropped.

\"Apple ][\" is the one that needs the second rule and the reason it is here --
the first attempt produced apple-.png."
  (let* ((name (crt.settings:profile-name profile))
         (clean (map 'string (lambda (c) (if (alphanumericp c) c #\-)) name))
         (parts (remove "" (uiop:split-string clean :separator '(#\-))
                        :test #'string=)))
    (string-downcase (format nil "~{~A~^-~}" parts))))

(defun render-all (&key (directory #p"docs/gallery/") (width 1000) (height 620))
  (ensure-directories-exist directory)
  (format t "~&~24A ~18A ~10A ~A~%" "profile" "face" "grid" "file")
  (dolist (profile crt.settings:+profiles+)
    (multiple-value-bind (target cols rows face scale)
        (render-profile profile :width width :height height)
      (unwind-protect
           (let ((path (merge-pathnames (format nil "~A.png"
                                                (profile-file-name profile))
                                        directory)))
             (crt-snapshot:write-texture-png target path)
             (format t "~&~24A ~18A ~2Dx~2D @~Dx  ~A~%"
                     (crt.settings:profile-name profile) face cols rows scale
                     (file-namestring path)))
        (crt.metal:release-texture target)))))
