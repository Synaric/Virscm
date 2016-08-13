;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 安装过程
;;;;
;;;; Created by Synaric on 2016-08-05 18:07:52.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require compatibility/mlist)

(provide (all-defined-out))

; 求值mcar两次的简写
(define (mcaar mlst)
  (mcar (mcar mlst)))

(define (mcadr mlst)
  (mcar (mcdr mlst)))

; 如果数据是pair，则转换weight
(define (list->mlist-if-pair? p)
  (if (pair? p)
      (list->mlist p)
      p))


