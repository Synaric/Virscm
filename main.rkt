;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 主逻辑
;;;;
;;;; Created by Synaric on 2016-08-06 09:58:17.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket

(require "install/installer.rkt"
	 	 "common/global.rkt"
		 "io/cmd.rkt")

; 初始化操作，包括初始化局部表，初始化全局环境等
(do-install)

; 启动驱动循环
(driver-loop the-global-enviroment)
