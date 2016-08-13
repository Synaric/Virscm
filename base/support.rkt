;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 描述了求值过程的特殊符号
;;;;
;;;; Created by Synaric on 2016-08-06 14:17:38.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require compatibility/mlist
         "../install/func.rkt")

(provide (all-defined-out))

; 确定表的开始是否是给定符号
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

; 获取求值序列
(define (exps exp)
  (cdr exp))

; 获取求值序列第一项
(define (first-exp exp)
  (car exp))

; 获取求值序列除第一项外的其余项
(define (rest-exps exp)
  (cdr exp))

; 获取表达式操作
(define (expression-type exp)
  (cond ((variable? exp) 'variable)
        ((quoted? exp) 'quoted)
    	((assignment? exp) 'assignment)
    	((definition? exp) 'definition)
    	((if? exp) 'if)
    	((lambda? exp) 'lambda)
    	((begin? exp) 'begin)
    	((cond? exp) 'cond)
    	((application? exp) 'application)
    	(else (error "Unknown expression type." exp))))

; 获取表达式参数
(define (expression-content exp)
  (if (pair? exp)
      (rest-exps exp)
      exp))

; 是否具有自求值性
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
    	(else #f)))

; 是否是变量
(define (variable? exp)
  (symbol? exp))

; 是否是引号表达式
(define (quoted? exp)
  (tagged-list? exp 'quote))

; 获取引号表达式的内容
(define (text-of-quotation exp)
  (cadr exp))

; 是否是赋值表达式
(define (assignment? exp)
  (tagged-list? exp 'set!))

; 获取赋值表达式的变量
(define (assignment-variable exp)
  (cadr exp))

; 获取赋值表达式的值
(define (assignment-value exp)
  (caddr exp))

; 是否是定义表达式
(define (definition? exp)
  (tagged-list? exp 'define))

; 获取定义表达式的变量
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

; 获取定义表达式的值
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
      			   (cddr exp))))

; 是否是lambda表达式
(define (lambda? exp)
  (tagged-list? exp 'lambda))

; 获取lambda表达式的参数
(define (lambda-parameters exp)
  (cadr exp))

; 获取lambda表达式的体
(define (lambda-body exp)
  (cddr exp))

; 构造lambda表达式
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; 是否是条件式
(define (if? exp)
  (tagged-list? exp 'if))

; 获取条件式的条件
(define (if-predicate exp)
  (cadr exp))

; 获取条件为真时执行的表达式
(define (if-consequent exp)
  (caddr exp))

; 获取条件为假时执行的表达式。如果没有，返回'false
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

; 构造条件式
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; 是否是begin表达式
(define (begin? exp)
  (tagged-list? exp 'begin))

; 获取begin表达式的执行序列
(define (begin-actions exp)
  (cdr exp))

; 检查执行序列是否非空
(define (last-exp? seq)
  (null? (cdr seq)))

; 构造begin表达式
(define (make-begin seq)
  (cons 'begin seq))

; 将一个序列变换为表达式
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
    	(else (make-begin seq))))

; 是否是过程应用
(define (application? exp)
  (pair? exp))

; 操作符
(define (operator exp)
  (car exp))

; 操作数
(define (operands exp)
  (cdr exp))

; 是否存在操作数
(define (no-operands? ops)
  (null? ops))

; 获取首个操作数
(define (first-operand ops)
  (car ops))

; 获取除了首个操作数外剩余的操作数
(define (rest-operands ops)
  (cdr ops))

; 是否为真
(define (true? x)
  (not (eq? x #f)))

; 是否为假
(define (false? x)
  (eq? x #f))

; 是否是cond表达式
(define (cond? exp)
  (tagged-list? exp 'cond))

; 获取cond表达式的分支
(define (cond-clauses exp)
  (cdr exp))

; cond表达式是否存在else子句
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

; 获取分支中第一个条件
(define (cond-predicate clause)
  (car clause))

; 获取分支的动作
(define (cond-actions clause)
  (cdr clause))

; 将cond表达式转换为if表达式
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

; 将分支序列转换为if表达式
(define (expand-clauses clauses)
  (if (null? clauses)
      'flase
      (let ([first (car clauses)]
            [rest (cdr clauses)])
      	(if (cond-else-clause? first)
      	    (if (null? rest)
      	        (sequence->exp (cond-actions first))
      	        (error "ELSE clause isn't last -- COND_IF"))
      	    (make-if (cond-predicate first)
      	    		 (sequence->exp (cond-actions first))
      	    		 (expand-clauses rest))))))

; 是否是and表达式
(define (and? exp)
  (tagged-list? exp 'and))

; 是否是or表达式
(define (or? exp)
  (tagged-list? exp 'or))

; 定义内建过程
(define primitive-procedures
	(mlist (mlist 'car car)
		  (mlist 'cdr cdr)
		  (mlist 'cons cons)
		  (mlist 'null? null?)
		  (mlist '+ +)
		  (mlist '- -)
		  (mlist '* *)
		  (mlist '/ /)))

; 获取所有内建过程名
(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

; 获取所有内建过程
(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc)))
  	   primitive-procedures))

; 是否为内建过程
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

; 应用给定内建过程
(define (apply-primitive-procedure proc args)
  (apply (procedure-parameters proc) args))

; 获取内建过程的操作
(define (primitive-implementation proc)
  (mcadr proc))

; 构造可解析过程
(define (make-procedure parameters body env)
  (mlist 'procedure
         (list->mlist-if-pair? parameters)
         (list->mlist-if-pair? body)
         env))

; 是否是复合过程
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

; 获取过程的参数
(define (procedure-parameters proc)
  (cadr proc))

; 获取过程的体
(define (procedure-body proc)
  (caddr proc))

; 获取过程的环境
(define (procedure-enviroment proc)
  (cadddr proc))