;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 解释器核心过程
;;;;
;;;; Created by Synaric on 2016-08-05 16:57:09.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "support.rkt" "table.rkt" "env.rkt")

(provide (all-defined-out))

(define (chant exp env)
  (if (self-evaluating? exp)
      exp
      (let ([eval-proc (get 'eval (expression-type exp))])
      	(eval-proc exp env))))

(define (execute procedure arguments)
  (cond ([primitive-procedure? procedure]
  		 (apply-primitive-procedure procedure arguments))
        ([compound-procedure? procedure]
         (chant-sequence (procedure-body procedure)
         				(extend-enviroment (procedure-parameters procedure)
         								   arguments
         								   (procedure-enviroment procedure))))
    	(else (error "Unknown procedure type -- APPLY" procedure))))

; 以组合式的运算对象为参数，求值各个对象，返回这些值的表
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (chant (first-operand exps) env)
      		(list-of-values (rest-operands exps) env))))

; 求值if表达式
(define (chant-if exp env)
  (if (true? (chant (if-predicate exp) env))
      (chant (if-consequent exp) env)
      (chant (if-alternative exp) env)))

; 求值过程体中的表达式序列
(define (chant-sequence exps env)
  (cond ((last-exp? exps) (chant (first-exp exps) env))
        (else (chant (first-exp exps) env)
        	  (chant-sequence (rest-exps exps) env))))

; 处理赋值
(define (chant-assignment exp env)
  (set-variable-value! (assignment-variable exp)
  					   (chant (assignment-value exp) env)
  					   env)
  'ok)

; 处理变量定义
(define (chant-definition exp env)
  (define-variable! (definition-variable exp)
  					(chant (definition-value exp) env)
  					env)
  'ok)

; 求值and表达式
(define (chant-and exps env)
  (cond ((null? exps) #t)
        ((last-exp? (first-exp exps)) (chant (first-exp exps)))
    	((true? (chant (first-exp exps) env)) (chant-and (rest-exps exps) env))
    	(else #f)))

; 求值or表达式
(define (chant-or exps env)
  (cond ((null? exps) #f)
        ((last-exp? (first-exp exps)) (chant (first-exp exps)))
    	((true? (chant (first-exp exps) env)) #t)
    	(else (chant-and (rest-exps exps) env))))

