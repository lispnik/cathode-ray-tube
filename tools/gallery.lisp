;;;; tools/gallery.lisp -- render a fixed screen under every profile.
;;;;
;;;; Headless: the whole chain runs into an offscreen texture rather than a
;;;; drawable, so this needs a GPU but no window.  It is how the port is judged
;;;; -- fourteen pictures next to fourteen pictures -- and the beginning of the
;;;; fidelity comparison against cool-retro-term itself.

(defpackage #:crt-gallery
  (:use #:cl)
  (:local-nicknames (#:settings #:cathode-ray-tube.settings)
                    (#:metal #:cathode-ray-tube.metal)
                    (#:text #:cathode-ray-tube.text)
                    (#:term #:cathode-ray-tube.terminal)
                    (#:vt #:cathode-ray-tube.vt)
                    (#:effects #:cathode-ray-tube.effects))
  (:export #:render-profile #:render-all))

(in-package #:crt-gallery)

(defparameter +sample+
  '("cathode-ray-tube"
    ""
    "$ ls -la /usr/local"
    "total 48"
    "drwxr-xr-x  18 root  wheel   576 Jan  1 00:00 ."
    "drwxr-xr-x   6 root  wheel   192 Jan  1 00:00 .."
    "drwxr-xr-x  38 root  wheel  1216 Jan  1 00:00 bin"
    ""
    "$ echo 'the quick brown fox jumps over the lazy dog'"
    "the quick brown fox jumps over the lazy dog"
    ""
    "$ _")
  "Something with the texture of real terminal output: a prompt, a listing,
mixed case, punctuation and whitespace.  A screen of lorem ipsum would look
fine under effects that a real session shows up.")

(defun sample-snapshot (renderer)
  (let* ((cols (text::text-renderer-cols renderer))
         (rows (text::text-renderer-rows renderer))
         (cells (let ((grid (make-array rows)))
                  (dotimes (r rows grid)
                    (setf (aref grid r)
                          (let ((row (make-array cols)))
                            (dotimes (c cols row)
                              (setf (aref row c) (vt:make-cell)))))))))
    (loop for line in +sample+
          for r from 0 below rows
          do (loop for ch across line
                   for c from 0 below cols
                   do (setf (vt:cell-char (aref (aref cells r) c)) ch)))
    (term:make-snapshot :rows rows :cols cols :cells cells
                        :dirty (make-array rows :element-type 'bit :initial-element 1)
                        :cursor-visible nil :painted t)))

(defun render-profile (profile &key (width 720) (height 440) (time 1.37d0)
                                    (font :ibm-vga-8x16))
  "The whole chain, into a texture.  The caller releases it."
  (crt.ui:ensure-appkit)
  (let* ((loaded (text::load-bundled-font font))
         (renderer (text:make-text-renderer :font loaded :width width :height height))
         (graph (effects:make-graph :profile profile :width width :height height))
         (target (metal:make-texture :width width :height height :label "gallery")))
    (unwind-protect
         (let ((snapshot (sample-snapshot renderer)))
           (let ((text (text:render-text renderer snapshot
                                         :default-fg '(255 255 255)
                                         :default-bg '(0 0 0))))
             ;; Twice: the burn-in accumulator starts empty, so a single frame
             ;; shows no trail at all and the burn-in-heavy profiles would look
             ;; identical to the rest.
             (dotimes (i 2)
               (effects:render-effects graph text target
                                       :time (+ time (* i 0.016d0)) :painted t
                                       :virtual-width (* (text::text-renderer-cols renderer)
                                                         (text::text-renderer-cell-width renderer))
                                       :virtual-height (* (text::text-renderer-rows renderer)
                                                          (text::text-renderer-cell-height renderer))))
             target))
      (effects:release-graph graph)
      (text:release-text-renderer renderer)
      (text:release-font loaded))))

(defun render-all (directory &key (width 720) (height 440))
  (ensure-directories-exist directory)
  (dolist (profile settings:+profiles+)
    (let* ((name (substitute #\- #\Space (settings:profile-name profile)))
           (name (remove-if-not (lambda (c) (or (alphanumericp c) (char= c #\-))) name))
           (path (merge-pathnames (format nil "~A.png" (string-downcase name)) directory))
           (target (render-profile profile :width width :height height)))
      (unwind-protect
           (progn (crt-snapshot:write-texture-png target path)
                  (format t "~&~24A -> ~A~%" (settings:profile-name profile) path))
        (metal:release-texture target)))))
