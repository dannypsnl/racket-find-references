#lang racket/gui
(require drracket/check-syntax
         syntax/modread
         data/interval-map
         try-catch-finally)

(struct binding
  (name start end external?)
  #:prefab)

(define project-dir "test-dir")
(define files (find-files (lambda (path) (path-has-extension? path #".rkt")) project-dir))
(define project-files
  ((compose
      list->set
      (lambda (x) (map path->complete-path x)))
    files))
project-files

(define projectwise-references (make-hash))

(define collector%
  (class (annotations-mixin object%)
    (init-field src text)

    (define bindings (make-interval-map))
    (define defs (make-hash))
    (define requires (make-hash))

    (define/override (syncheck:find-source-object stx) (and (equal? src (syntax-source stx)) src))

    (define/override (syncheck:add-require-open-menu source-obj start end required-file)
      (hash-set! requires required-file (list start end)))

    (define/override (syncheck:add-arrow/name-dup start-src-obj start-left start-right
                                                  end-src-obj end-left end-right
                                                  actual?
                                                  level
                                                  require-arrow?
                                                  name-dup?)
      (define id (string->symbol (send text get-text end-left end-right)))
      (unless require-arrow?
        (interval-map-set! bindings
                           end-left
                           (add1 end-right)
                           (binding id start-left start-right #f))))

    (define/override (syncheck:add-jump-to-definition source-obj start end id filename submods)
      (when (set-member? project-files filename)
        (println "Reference to other file!")
        (hash-update! projectwise-references
          (list filename id)
          (lambda (refs)
            (set-add refs (list src start (add1 end))))
          (set)))
      (interval-map-set! bindings start (add1 end) (binding id #f #f filename)))

    (define/override (syncheck:add-definition-target source-obj start end id mods)
      (hash-set! defs id (binding id start end src)))

    (define/public (get-bindings)
      bindings)
    (define/public (get-defs)
      defs)
    (define/public (get-requires)
      requires)

    (super-new)))

(define (collect-from path ns)
  (define text (new text%))
  (send text load-file path)
  (define collector (new collector% [src path] [text text]))
  (define-values (src-dir file dir?) (split-path path))
  (define in (open-input-string (send text get-text)))

  (try (define-values (add-syntax done) (make-traversal ns src-dir))
       (parameterize ([current-annotations collector]
                      [current-namespace ns]
                      [current-load-relative-directory src-dir])
         (define stx (expand (with-module-reading-parameterization (λ () (read-syntax path in)))))
         (add-syntax stx)
         (done))
       (catch _ (error "collect-from path: ~a failed" path)))
  collector)

(define ns (make-base-namespace))
(for ([p project-files])
  (collect-from (normalize-path p) ns))

(define (lookup-references path symbol)
  (hash-ref projectwise-references (list path symbol)))

(lookup-references (path->complete-path "test-dir/a.rkt") 'xxxxxx)
