;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 解释器环境操作
;;;;
;;;; Created by Synaric on 2016-08-08 11:32:45.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "support.rkt"
         compatibility/mlist)

(provide lookup-variable-value
		 extend-enviroment
		 set-variable-value!
		 define-variable!
             setup-enviroment)

; 定义空环境
(define the-empty-enviroment '())

; 获取环境的外围环境，空环境用null表示
(define (enclosing-enviroment env)
  (mcdr env))

; 获取当前最精确环境
(define (first-frame env)
  (mcar env))

; 构造框架。框架由一个变量名的表和一个值的表构成
(define (make-frame variables values)
  (mcons variables values))

; 获取框架的变量名表
(define (frame-variables frame)
  (mcar frame))

; 获取框架的变量值的表
(define (frame-values frame)
  (mcdr frame))

; 变量名与变量值绑定
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

; 新建子环境
(define (extend-enviroment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (error "Unmatched variables for binding values." vars vals)))

; 在环境中查找变量，如果没有找到，则到外围环境寻找。
(define (lookup-variable-value var env)
  (env-looper var env null (lambda (vals) (mcar vals))))

; 在环境中修改指定变量的值
(define (set-variable-value! var val env)
  (env-looper var env null (lambda (vals) (set-mcar! vals val))))

(define (define-variable! var val env)
  (env-looper var env (lambda (frame) (add-binding-to-frame! var val frame))
  					  (lambda (vals) (set-mcar! vals val))))

; 设置全局环境
(define (setup-enviroment)
  (let ([initial-env (extend-enviroment (primitive-procedure-names)
  										(primitive-procedure-objects)
  										the-empty-enviroment)])
  	(define-variable! 'true #t initial-env)
  	(define-variable! 'false false initial-env)
    (displayln "Setup enviroment success.")
    (displayln "--------------Welcome to Virscm--------------")
    initial-env))

(define (env-looper var env on-null on-result)
  (define (scan vars vals frame)
    (cond ((null? vars) (if (null? on-null)
        (env-loop (enclosing-enviroment env))
        (on-null frame)))
          ((eq? var (mcar vars)) (on-result vals))
      	  (else (scan (mcdr vars) (mcdr vals) frame))))
  (define (env-loop env)
    (if (eq? env the-empty-enviroment)
        (error "Unbound variable" var)
        (let ([frame (first-frame env)])
        	(scan (frame-variables frame)
        		  (frame-values frame)
        		  frame))))
  (env-loop env))

