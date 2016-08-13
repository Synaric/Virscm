;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 局部表，实现为一个二维表，通过key对查找确定符号。
;;;; 表中比较符号相等性的比较器可以自定义，用于模糊查找或特定类型的查找。
;;;;
;;;; Created by Synaric on 2016-08-05 17:26:55.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket
(require compatibility/mlist)
(require "../install/func.rkt")

(provide (all-defined-out))

; 以指定比较器生成局部表
(define (make-table same-key?)
    (let ((local-table (mlist '*table*)))
        (define (smassoc key records)
            (cond ((null? records)
                    #f)
            	; 使用 same-key? 对比键
                  ((same-key? key (mcaar records))
                    (mcar records))
                  (else
                    (smassoc key (mcdr records)))))

        (define (lookup key-1 key-2)
            (let ((subtable (smassoc key-1 (mcdr local-table))))
                (if subtable
                    (let ((record (smassoc key-2 (mcdr subtable))))
                        (if record
                            (mcdr record)
                            #f))
                    #f)))

        (define (insert! key-1 key-2 value)
            (let ((subtable (smassoc key-1 (mcdr local-table))))
                (if subtable
                    (let ((record (smassoc key-2 (mcdr subtable))))
                        (if record
                            (set-mcdr! record value)
                            (set-mcdr! subtable
                                      (mcons (mcons key-2 value)
                                            (mcdr subtable)))))
                    (set-mcdr! local-table
                              (mcons (mlist key-1
                                          (mcons key-2 value))
                                    (mcdr local-table)))))
            'ok)

        (define (show)
            local-table)

        (define (dispatch m)
            (cond ((eq? m 'lookup-proc) lookup)
                  ((eq? m 'insert-proc!) insert!)
                  ((eq? m 'show) show)
                  (else 
                    (error "Unknown operation -- TABLE" m))))

        dispatch))

; 默认表
(define default-table (make-table equal?))

; 以数字作为关键字的表
; (define number-table (make-table =))

; 以符号作为关键字的表
; (define symbol-table (make-table eq?))

; 将符号插入局部表
(define get (default-table 'lookup-proc))

; 通过二维key对查找局部表
(define put (default-table 'insert-proc!))


