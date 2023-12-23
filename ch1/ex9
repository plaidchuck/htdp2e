(define (cond-test in)
(cond [(string? in) (string-length in)]
      [(image? in) (* (image-height in)(image-width in))]
      [(number? in)(if (< in 1) (abs in) (- in 1))]
      [(boolean? in)(if in 10 20)]))
