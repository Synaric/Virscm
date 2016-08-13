;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 命令行交互
;;;;
;;;; Created by Synaric on 2016-08-08 15:08:36.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../common/global.rkt"
		 "../base/support.rkt"
		 "../base/chaos.rkt")

(provide driver-loop)

(define input-prompt "~a input: ")

(define output-prompt "~a output: ")

(define (driver-loop env)
  (prompt-for-input input-prompt)
  (let* ([input (read)]
  		[output (chant input env)])
  	(announce-output output-prompt)
    (user-print output))
  (driver-loop env))

(define (prompt-for-input str)
  (display str))

(define (announce-output str)
  (display str))

(define (user-print obj)
  (if (compound-procedure? obj)
      (display (list 'compound-procedure
      				 (procedure-parameters obj)
      				 (procedure-body obj)))
      (displayln obj)))