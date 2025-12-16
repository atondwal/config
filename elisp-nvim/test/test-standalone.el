;;; test-standalone.el --- Standalone tests for nvim-bridge -*- lexical-binding: t; -*-

;; Test that the bridge loads and basic functions work

(load-file "elisp-nvim/elisp/nvim-bridge.el")

(princ "\n=== Testing nvim-bridge.el ===\n\n")

;; Test 1: Bridge loaded
(princ "Test 1: Bridge loaded... ")
(if (fboundp 'nvim-bridge-start-server)
    (princ "✓ PASS\n")
  (princ "✗ FAIL\n"))

;; Test 2: RPC handler registration
(princ "Test 2: RPC handlers registered... ")
(if (hash-table-p nvim-bridge-handlers)
    (princ (format "✓ PASS (%d handlers)\n" (hash-table-count nvim-bridge-handlers)))
  (princ "✗ FAIL\n"))

;; Test 3: Eval handler exists
(princ "Test 3: Eval handler exists... ")
(if (gethash "eval" nvim-bridge-handlers)
    (princ "✓ PASS\n")
  (princ "✗ FAIL\n"))

;; Test 4: Ping handler exists
(princ "Test 4: Ping handler exists... ")
(if (gethash "ping" nvim-bridge-handlers)
    (princ "✓ PASS\n")
  (princ "✗ FAIL\n"))

;; Test 5: Eval handler works
(princ "Test 5: Eval handler works... ")
(let* ((handler (gethash "eval" nvim-bridge-handlers))
       (result (funcall handler '((code . "(+ 1 2 3)")))))
  (if (and result (string= (cdr (assoc 'result result)) "6"))
      (princ "✓ PASS\n")
    (princ (format "✗ FAIL (got: %S)\n" result))))

;; Test 6: Ping handler works
(princ "Test 6: Ping handler works... ")
(let* ((handler (gethash "ping" nvim-bridge-handlers))
       (result (funcall handler nil)))
  (if (string= (cdr (assoc 'status result)) "ok")
      (princ "✓ PASS\n")
    (princ (format "✗ FAIL (got: %S)\n" result))))

;; Test 7: Dired function exists
(princ "Test 7: Dired function exists... ")
(if (fboundp 'nvim-bridge-dired)
    (princ "✓ PASS\n")
  (princ "✗ FAIL\n"))

;; Test 8: Dired can generate listing
(princ "Test 8: Dired can generate listing... ")
(require 'dired)
(let ((dired-buf (dired-noselect ".")))
  (if (buffer-live-p dired-buf)
      (progn
        (princ "✓ PASS\n")
        (princ (format "   Generated %d lines\n"
                       (with-current-buffer dired-buf
                         (count-lines (point-min) (point-max))))))
    (princ "✗ FAIL\n")))

(princ "\n=== All Tests Complete ===\n")
