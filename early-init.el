;; Max out GC and disable package.el
(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum)
