; Project "Synthesizer" extends the sound synthesizer code that have been developed previously into a more full featured digital synthesizer. I have implemented several additional features for the synthesizer, more closely approximating the features found in analog synthesizers such as the Korg Minilogue or the Moog Matriarch.
; Author: Anh Thuc (Christina) Vu
; Date: 2022-11-11
; Acknowledgements: Dieu Anh (Audrey) Trinh 

(define middle-c 261.63); return the frequency with the value of 16000
(import audio) 
; (index-to-square n total-samples): vector?
; n: number?
; total-samples: number?
; Returns the the resulting samples that are only in the range of valid samples: [-1.0, 1.0]
(define index-to-square
  (lambda (n total-samples)
    (if (< n (/ total-samples 2))
      -1
      1))) ; Audrey 
; (square-sample sample-rate frequency duration): vector?
; sample-rate: number?
; frequency: number?
; duration : number?
; Returns a vector that represents a square waveform
(define square-sample
  (lambda (sample-rate frequency duration)
    (let ([samples-per-clip (* sample-rate duration)]
        [samples-per-wave (/ sample-rate frequency)])
      (|> (vector-range 0 samples-per-clip)
        (lambda (vec)
          (vector-map
            (lambda (n)
              (index-to-square (remainder n samples-per-wave) samples-per-wave)) vec)))))) ; Christina and Audrey
; (square-sample 16000 middle-c 2 )

; (index-to-sawtooth n total-samples): vector?
; n: number?
; total-samples: number?
; Returns the the resulting samples that are only in the range of valid samples: [-1.0, 1.0]
(define index-to-sawtooth
  (lambda (n total-samples)
    (- (* n (/ 2 total-samples)) 1))) ; Audrey
; (sawtooth-sample sample-rate frequency duration): vector?
; sample-rate: number?
; frequency: number?
; duration : number? 
; Returns a vector that represents a sawtooth waveform
(define sawtooth-sample
  (lambda (sample-rate frequency duration)
    (let ([samples-per-clip (* sample-rate duration)]
        [samples-per-wave (/ sample-rate frequency)])
      (|> (vector-range 0 samples-per-clip)
        (lambda (vec)
          (vector-map
            (lambda (n)
              (index-to-sawtooth (remainder n samples-per-wave) samples-per-wave)) vec)))))) ; Christina and Audrey
; (index-to-triangle n total-samples): vector?
; n: number?
; total-samples: number?
; Returns the the resulting samples that are only in the range of valid samples: [-1.0, 1.0]
(define index->triangle-sample
  (lambda (n total-samples)
    (if (< n (/ total-samples 2))
      (- (* 4 n) 1)
      (+ (* -4 n) 3)))) ; Audrey
; (triangle-sample sample-rate frequency duration): vector?
; sample-rate: number?
; frequency: number?
; duration : number? 
; Returns a vector that represents a triangle waveform
(define triangle-sample
  (lambda (sample-rate frequency duration)
    (|> (vector-range 0 1 (/ 1 (* sample-rate duration)))
      (lambda (vec)
        (vector-map
          (lambda (n) (index->triangle-sample n 1))
          vec))))) ; Christina and Audrey
; (sine-sample sample-rate frequency duration): vector?
; sample-rate: number?
; frequency: number?
; duration : number? 
; Returns a vector that represents a sine waveform
(define sine-sample
  (lambda (sample-rate frequency duration)
    (vector-map sin 
      (vector-range 0 (* 2 3.14) (/ (* 2 3.14) (* sample-rate duration)))))) ;Audrey
;(sine-sample 10 1 1 )

(define apply-envelope
  (lambda (clip envelope)
    (vector-map (lambda (x y) (* x y)) clip envelope)))
; (simple-envelope total-samples: vector?
  ; total-samples: number? 
  ; Returns a simple, linearly decaying envelope consisting of n samples.
  
  (define simple-envelope
    (lambda (total-samples)
      (vector-map
        (lambda (x) (+ 1 (* x (/ -1 total-samples))))
        (vector-range total-samples)))) ; Christina and Audrey
  
  ; (synthesize-note waveform sample-rate frequency duration) -> vector?
  ; waveform: string? one of "square", "sawtooth", "triangle", or "sine"
  ; sample-rate: number?, a non-negative integer
  ; frequency: number?, a non-negative number
  ; duration: number?, a non-negative number
  ; Returns a vector of samples representing a single note syntheiszed from
  ; the given parameters.
  (define synthesize-note
    (lambda (waveform sample-rate frequency duration)
      (|> (cond 
          [(equal? waveform "square")
            (square-sample sample-rate frequency duration)]
          [(equal? waveform "sawtooth")
            (sawtooth-sample sample-rate frequency duration)]
          [(equal? waveform "triangle")
            (triangle-sample sample-rate frequency duration)]
          [ else 
            (sine-sample sample-rate frequency duration)])
        (lambda (v) (apply-envelope v (simple-envelope (* sample-rate duration))))))) ; Audrey
  ; (amplitude x ): vector? 
  ; x: number? 
  ; Returns the the resulting samples that are only in the range of valid samples: [-1.0, 1.0]
  (define amplitude
    (lambda (x)
      (cond [ (> x 1) 1] 
        [ (< x -1) -1] 
        [else x]))) 
  ; (make-sample sample-rate frequency duration waveforms ): audio? 
  ; sample-rate: number?
  ; frequency: number?
  ; duration : number?
  ; waveforms: list? 
  ; Returns the customized sound of the sample according to the synthesizer-note function.
  (define make-sample
    (lambda ( sample-rate frequency duration waveforms )
      (let ([notes (map (lambda (str) 
                (synthesize-note str sample-rate frequency duration )) 
              waveforms)])
        (sample-node 
          (vector-map amplitude 
            (match notes 
              [(cons vec-1 (cons vec-2 (cons vec-3 null))) 
                (vector-map + vec-1 vec-2 vec-3)] 
              [(cons vec-1 (cons vec-2 null))
                (vector-map + vec-1 vec-2)]
              [(cons vec null) vec]
              )))))) 
  (make-sample 16000 middle-c 2 (list "sine" "triangle" "sawtooth"))
  (make-sample 16000 middle-c 2 (list "sine" "triangle" "square"))
  ; (decay total-samples): vector?
  ; total-samples: number? 
  ; Returns a simple, linearly decaying envelope consisting of total-samples.
  (define decay
    (lambda (total-samples)
      (vector-map
        (lambda (x) (+ 1 (* x (/ -1 total-samples))))
        (vector-range total-samples))))
  
  ; (decay total-samples): vector?
  ; total-samples: number? 
  ; Returns a simple, linearly increasing envelope consisting of total-samples.
  (define attack
    (lambda (total-samples)
      (vector-map
        (lambda (x) x)
        (vector-range total-samples))))
  
  
  ; (sustain total-sample): vector? 
  ; total-samples: number? 
  ; Returns a simple, linearly sustainable envelope consisting of total-samples.
  
  (define sustain 
    (lambda (total-samples)
      (vector-map
        (lambda (x) x)
        (vector-range total-samples))))
  
  ; (synthesize-note-1 adsr sample-rate frequency duration) -> vector?
  ; waveform: string? one of "square", "sawtooth", "triangle", or "sine"
  ; sample-rate: number?, a non-negative integer
  ; frequency: number?, a non-negative number
  ; duration: number?, a non-negative number
  ; Returns a vector of samples representing a single note syntheiszed from
  ; the given parameters. 
  
  (define synthesize-note-1
    (lambda (adsr sample-rate frequency duration)
      (|> (cond 
          [(equal? adsr "decay")
            (triangle-sample sample-rate frequency duration)]
          [(equal? adsr "attack")
            (sine-sample sample-rate frequency duration)]
          [(equal? adsr "sustain")
            (square-sample sample-rate frequency duration)]
          [ else 
            (sawtooth-sample sample-rate frequency duration)])
        (lambda (v) (apply-envelope v (simple-envelope (* sample-rate duration)))))))
  
  ; (full-ADSR sample-rate frequency duration waveforms ): audio? 
  ; sample-rate: number?
  ; frequency: number?
  ; duration : number?
  ; adsr: list? 
  ; Returns the ADSR envelope that can customize the attack, sustain, decay, and subsequent release. 
  (define full-ADSR
    (lambda ( sample-rate frequency duration adsr )
      (let ([notes (map (lambda (str) 
                (synthesize-note-1 str sample-rate frequency duration)) 
              adsr)]) 
        (sample-node
          (vector-map amplitude
            (match notes 
              [(cons vec-1 (cons vec-2 (cons vec-3 null))) 
                (vector-map + vec-1 vec-2 vec-3)] 
              [(cons vec-1 (cons vec-2 null))
                (vector-map + vec-1 vec-2)]
              [(cons vec null) vec]
              )))))) 
  (full-ADSR 16000 middle-c 2 (list "decay" "attack" "sustain" )) 