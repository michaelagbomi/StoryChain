;; StoryChain: Narrative Writing and Story Exchange Platform
;; Version: 1.0.0

(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-STORY-NOT-FOUND (err u2))
(define-constant ERR-ALREADY-PUBLISHED (err u3))
(define-constant ERR-INVALID-STATUS (err u4))
(define-constant ERR-INVALID-WORD-COUNT (err u5))
(define-constant ERR-INVALID-GENRE (err u6))
(define-constant ERR-INVALID-MATURITY (err u7))
(define-constant ERR-INVALID-STORY-TITLE (err u8))
(define-constant ERR-INVALID-NARRATIVE (err u9))

(define-constant MIN-WORD-COUNT u100)

(define-data-var next-story-id uint u1)

(define-map story-anthology
    uint
    {
        author: principal,
        story-title: (string-utf8 50),
        narrative: (string-utf8 200),
        genre: (string-utf8 15),
        maturity: (string-utf8 10),
        publication-status: (string-utf8 15),
        word-count: uint
    })

(define-private (validate-genre (genre (string-utf8 15)))
    (or 
        (is-eq genre u"Fiction")
        (is-eq genre u"Mystery")
        (is-eq genre u"Romance")
        (is-eq genre u"Fantasy")
        (is-eq genre u"SciFi")
        (is-eq genre u"Horror")
    ))

(define-private (validate-maturity (maturity (string-utf8 10)))
    (or 
        (is-eq maturity u"General")
        (is-eq maturity u"Teen")
        (is-eq maturity u"Young Adult")
        (is-eq maturity u"Adult")
        (is-eq maturity u"Mature")
    ))

(define-private (validate-text-structure (text (string-utf8 200)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-public (publish-story 
    (story-title (string-utf8 50))
    (narrative (string-utf8 200))
    (genre (string-utf8 15))
    (maturity (string-utf8 10))
    (word-count uint))
    (let
        (
            (story-id (var-get next-story-id))
        )
        (asserts! (validate-text-structure story-title u3 u50) ERR-INVALID-STORY-TITLE)
        (asserts! (validate-text-structure narrative u10 u200) ERR-INVALID-NARRATIVE)
        (asserts! (>= word-count MIN-WORD-COUNT) ERR-INVALID-WORD-COUNT)
        (asserts! (validate-genre genre) ERR-INVALID-GENRE)
        (asserts! (validate-maturity maturity) ERR-INVALID-MATURITY)
        
        (map-set story-anthology story-id {
            author: tx-sender,
            story-title: story-title,
            narrative: narrative,
            genre: genre,
            maturity: maturity,
            publication-status: u"published",
            word-count: word-count
        })
        (var-set next-story-id (+ story-id u1))
        (ok story-id)
    ))

(define-public (unpublish-story (story-id uint))
    (let
        (
            (story (unwrap! (map-get? story-anthology story-id) ERR-STORY-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get author story)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get publication-status story) u"published") ERR-INVALID-STATUS)
        (ok (map-set story-anthology story-id (merge story { publication-status: u"unpublished" })))
    ))

(define-read-only (get-story (story-id uint))
    (ok (map-get? story-anthology story-id)))

(define-read-only (get-author (story-id uint))
    (ok (get author (unwrap! (map-get? story-anthology story-id) ERR-STORY-NOT-FOUND))))

(define-read-only (get-total-stories)
    (ok (- (var-get next-story-id) u1)))

(define-read-only (get-publication-status (story-id uint))
    (ok (get publication-status (unwrap! (map-get? story-anthology story-id) ERR-STORY-NOT-FOUND))))