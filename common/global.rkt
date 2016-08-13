;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 项目信息相关
;;;;
;;;; Created by Synaric on 2016-08-08 15:12:01.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(provide (all-defined-out))

(define project-name "VirtualScheme")

(define project-short-name "virscm")

; 全局环境，在installer中初始化
(define the-global-enviroment #f)

(define flag-global-env-initialized #f)

(define (init-enviroment env)
  (cond (flag-global-env-initialized (error "Global enviroment has already initialized")))
  (set! the-global-enviroment env)
  (set! flag-global-env-initialized #t))