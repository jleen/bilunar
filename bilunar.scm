;;;;;;;;;;;;;;;;;;;
;; bilunar-date ADT

(define (make-bilunar-date year month day)
  (list year month day))

(define (bilunar-date-year date) (car date))
(define (bilunar-date-month date) (cadr date))
(define (bilunar-date-day date) (caddr date))


;;;;;;;;;;;;;;;;;;;;
;; bilunar-month ADT

(define (make-bilunar-month name starting-day-of-year)
  (list name starting-day-of-year))

(define (bilunar-month-name month) (car month))
(define (bilunar-month-start month) (cadr month))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lunarian calendar (days of the month and year)

(define (nth-month n)
  (list-ref *bilunar-months* (+ n 1)))

(define (serial-date-year serial)
  (+ 1 (quotient (- serial 1) days-per-luna-year)))

(define (serial-date-doy serial)
  (+ 1 (remainder (- serial 1) days-per-luna-year)))

(define (year-and-doy->serial-date year doy)
  (+ (first-serial-date-of-year year)
     (- doy 1)))

(define (bilunar-date->serial-date bilunar)
  (+ (* days-per-luna-year (- (bilunar-date-year bilunar) 1))
     (- (bilunar-month-start (bilunar-date-month bilunar)) 1)
     (bilunar-date-day bilunar)))

(define (serial-date->bilunar-date serial)
  (let* ((year (serial-date-year serial))
         (day-of-year (serial-date-doy serial))
         (month (day-of-year->month day-of-year))
         (day-of-month (+ 1 (- day-of-year (bilunar-month-start month)))))
    (make-bilunar-date year month day-of-month)))

(define (bilunar-date->string bilunar-date)
  (string-append (bilunar-month-name (bilunar-date-month bilunar-date))
                 " "
                 (number->string (bilunar-date-day
                                  bilunar-date))
                 ", "
                 (number->string (bilunar-date-year
                                  bilunar-date))
                 " "
                 *year-suffix*
                 ))

(define (day-of-year->month day)
  (define (find-month day months prev)
    (cond ((null? months) prev)
          ((< day (bilunar-month-start (car months)))
           prev)
          (else (find-month day (cdr months) (car months)))))
  (find-month day (cdr *bilunar-months*) (car *bilunar-months*)))

(define (serial-date-luna-phase serial)
  (remainder (+ 0 (round (* serial luna-phases-per-day)))
             phases-per-cycle))

(define (first-serial-date-of-year year)
  (+ 1 (* days-per-luna-year (- year 1))))

(define (last-serial-date-of-year year)
  (* days-per-luna-year year))

(define (serial-dates-of-year year)
  (integer-range (first-serial-date-of-year year)
                 (last-serial-date-of-year year)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selenian calendar (days of the week)

(define (serial-date-selene-phase serial)
  (remainder (round (* serial selene-phases-per-day))
             phases-per-cycle))

(define (needed-intercalary-days-for-year year)
  (count-dups (map serial-date-selene-phase
                   (serial-dates-of-year year))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conventional calendar (pseudo-Selenian)

(define (serial-date-unintercalated-selene-phase serial-date)
  (let ((date (serial-date->bilunar-date serial-date))
        (doy (serial-date-doy serial-date)))
    (remainder (- (+ doy
                     (* (- (bilunar-date-year date) 1)
                        desired-excess-selene-phases))
                  (count-ad-hoc-intercalary-days-before serial-date))
               phases-per-cycle)))
    
(define (intercalary-days-of-year year)
  (map (lambda (spec num)
         (serial-date-doy
          (find-closest-bilunar-phase-coincidence
           (year-and-doy->serial-date year (car spec))
           (cdr spec)
           num)))
       *intercalary-day-of-year-targets*
       (integer-range 0 (- (length *intercalary-day-of-year-targets*)
                           1))))

(define (find-closest-bilunar-phase-coincidence target-serial-date
                                                search-start-relative-position
                                                num-previous)
  (let ((target-phase (serial-date-luna-phase target-serial-date)))
    (let find-phase ((serial-date (+ target-serial-date
                                     search-start-relative-position)))
      (if (= (- (serial-date-unintercalated-selene-phase serial-date)
                num-previous)
             target-phase)
          serial-date
          (find-phase (+ 1 serial-date))))))

(define (serial-date-conventional-selene-phase serial-date)
  (remainder (+ (* (- (serial-date-year serial-date) 1)
                   conventional-selene-phases-per-luna-year)
                (let ((day-of-year (serial-date-doy serial-date))
                      (year (serial-date-year serial-date)))
                  (- day-of-year
                     (+ (count-yearly-intercalary-days-before day-of-year year)
                        (count-ad-hoc-intercalary-days-before serial-date)))))
             phases-per-cycle))

(define (count-yearly-intercalary-days-before day-of-year year)
  (filtered-count (lambda (intercalary-day)
                    (> day-of-year intercalary-day))
                  (intercalary-days-of-year year)))

(define (count-ad-hoc-intercalary-days-before serial-date)
  (filtered-count (lambda (intercalary-day)
                    (>= serial-date intercalary-day))
                  *ad-hoc-intercalary-days*))

(define (serial-date-selene-phase-error serial-date)
  (- (serial-date-conventional-selene-phase serial-date)
     (serial-date-selene-phase serial-date)))

(define (serial-date-intercalation-needed serial-date)
  (- (serial-date-conventional-selene-phase serial-date)
     (serial-date-unintercalated-selene-phase serial-date)))

(define (first-leap-year-from year)
  (if (not (= 0 (serial-date-selene-phase-error
                 (first-serial-date-of-year year))))
      year
      (first-leap-year-from (+ 1 year))))

;;;;;;;;;
;; output

(define (make-celestial-calendar year)
  (map (lambda (serial-date)
         (list (serial-date->bilunar-date serial-date)
               (serial-date-selene-phase serial-date)
               (serial-date-luna-phase serial-date)
               ))
       (serial-dates-of-year year)))

(define (make-conventional-calendar year)
  (map (lambda (serial-date)
         (list (serial-date->bilunar-date serial-date)
               (serial-date-conventional-selene-phase serial-date)
               (serial-date-luna-phase serial-date)
               ))
       (serial-dates-of-year year)))

(define (pretty-print-calendar cal)
  (for-each (lambda (cal-entry)
              (display (bilunar-date->string (car cal-entry)))
              (display ".")
              (newline)
              (display "   ")
              (display *selene-name*)
              (display " ")
              (display (phase->string (cadr cal-entry)))
              (display ", ")
              (display *luna-name*)
              (display " ")
              (display (phase->string (caddr cal-entry)))
              (display ".")
              (newline))
            cal))


;;;;;;;;;;;;;;;;;;;;;
;; Phases of the moon

(define *phase-names*
  (list "Full"
        "Waning Gibbous"
        "Last Quarter"
        "Waning Crescent"
        "New"
        "Waxing Crescent"
        "First Quarter"
        "Waxing Gibbous"))

(define (phase->string phase)
  (list-ref *phase-names* phase))


;;;;;;;;;;;;
;; Utilities

(define (integer-range lowest highest)
  (let iter ((lowest (floor lowest))
             (highest (floor highest))
             (range '()))
    (if (> lowest highest)
        range
        (iter lowest (- highest 1) (cons highest range)))))

(define (indices-of-dups lst)
  (let iter ((lst lst)
             (n 1)
             (dups '()))
    (cond ((null? lst) dups)
          ((null? (cdr lst)) dups)
          ((= (car lst) (cadr lst))
           (iter (cdr lst) (+ n 1) (cons n dups)))
          (else (iter (cdr lst) (+ n 1) dups)))))

(define (count-dups lst)
  (let iter ((lst lst)
             (count 0))
    (cond ((null? lst) count)
          ((null? (cdr lst)) count)
          ((= (car lst) (cadr lst))
           (iter (cdr lst) (+ count 1)))
          (else (iter (cdr lst) count)))))

(define (filtered-count pred items)
  (let iter ((pred pred)
             (items items)
             (count 0))
    (cond ((null? items) count)
          ((pred (car items)) (iter pred (cdr items) (+ 1 count)))
          (else (iter pred (cdr items) count)))))


;;;;;;;;;;;;;;;;;;;;;;
;; Celestial constants

(define days-per-selene-year 362)
(define days-per-luna-year 365)

(define days-per-week 8)
(define months-per-year 12)
(define phases-per-cycle 8)

(define selene-cycles-per-selene-year 45)
(define selene-phases-per-selene-year
  (* phases-per-cycle selene-cycles-per-selene-year))

(define selene-phases-per-day
  (/ selene-phases-per-selene-year days-per-selene-year))

(define luna-phases-per-luna-year
  (* phases-per-cycle months-per-year))

(define luna-phases-per-day
  (/ luna-phases-per-luna-year days-per-luna-year))

(define selene-phases-per-luna-year
  (* selene-phases-per-day days-per-luna-year))

(define excess-selene-phases
  (- selene-phases-per-luna-year selene-phases-per-selene-year))

(define desired-excess-selene-phases
  (- days-per-luna-year days-per-selene-year))

(define selene-phase-deficit
  (- desired-excess-selene-phases excess-selene-phases))

(define selene-deficit-with-centennary
  (- selene-phase-deficit 0.01))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conventional constants

(define *month-starting-days-of-year*
  (list 1 31 62 92 122 153 183 214 245 275 305 336))

(define *intercalary-day-of-year-targets*
  (list (cons 121 0)
        (cons 320 -3)))

(define *ad-hoc-intercalary-days*
  (list (year-and-doy->serial-date 33 130)
        (year-and-doy->serial-date 100 130)))

(define conventional-selene-phases-per-luna-year
  (+ selene-phases-per-selene-year
     desired-excess-selene-phases))


;;;;;;;;
;; Names

(define *year-suffix-abbrev* "MD")
(define *year-suffix* "meta Draconton")

(define *selene-name* "Selene")
(define *luna-name* "Luna")

(define *month-names*
  (list "Firstspring"
        "Midspring"
        "Lastspring"
        "Firstsummer"
        "Midsummer"
        "Lastsummer"
        "Firstautumn"
        "Midautumn"
        "Lastautumn"
        "Firstwinter"
        "Midwinter"
        "Lastwinter"
        ))

(define *bilunar-months*
  (map make-bilunar-month *month-names* *month-starting-days-of-year*))
