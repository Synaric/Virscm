;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 将解释器必要方法加入局部表中
;;;;
;;;; Created by Synaric on 2016-08-06 16:51:52.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "../base/chaos.rkt"
     "../base/table.rkt"
     "../base/support.rkt"
     "../base/env.rkt"
     "../common/global.rkt"
     compatibility/mlist)

(provide do-install)

(define (do-install)
  (install-env)
  (install-eval-maps))

(define (install-eval-maps)
  (put 'eval
       'variable
       (lambda (exp env) (lookup-variable-value exp env)))
  (put 'eval 
       'quote
       (lambda (exp env) (text-of-quotation exp)))
  (put 'eval
       'assignment
       (lambda (exp env) (chant-assignment exp env)))
  (put 'eval
       'definition
       (lambda (exp env) (chant-definition exp env)))
  (put 'eval
       'if
       (lambda (exp env) (chant-if exp env)))
  (put 'eval
       'lambda
       (lambda (exp env) (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env)))
  (put 'eval
       'begin
       (lambda (exp env) (chant-sequence (begin-actions exp) env)))
  (put 'eval
       'cond
       (lambda (exp env) (chant (cond->if exp) env)))
  (put 'eval
       'application
       (lambda (exp env) (execute
                      (let ([result (chant (operator exp) env)])
                        (cond ([mlist? result] (mlist->list result))
                              (else result)))
                      (list-of-values (operands exp) env)))))

(define (install-env)
  (init-enviroment (setup-enviroment)))
