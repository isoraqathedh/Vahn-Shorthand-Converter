(defpackage :info.isoraqathedh.vahn-converter.asdf
  (:use #:cl #:asdf))
(in-package :info.isoraqathedh.vahn-converter.asdf)

(defsystem vahn-converter
  :name "Vahn Shorthand Converter"
  :author "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :maintainer "Isoraķatheð Zorethan <isoraqathedh.zorethan@gmail.com>"
  :version "1.0.0"
  :licence "MIT"
  :description "Expander of shorthand for the constructed language Vahn."
  :serial t
  :depends-on (:cl-slice :plump)
  :components ((:file "vahn-expander")))
