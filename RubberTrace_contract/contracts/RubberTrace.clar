
;; title: RubberTrace
;; version: 1.0.0
;; summary: Supply chain tracking smart contract for natural rubber origin and fair trade verification
;; description: RubberTrace enables transparent tracking of rubber batches from producers to consumers,
;;              ensuring origin authenticity and fair trade compliance verification.

;; traits
;;

;; token definitions
;;

;; constants
;;
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-batch (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-status (err u105))

;; data vars
;;
(define-data-var next-batch-id uint u1)
(define-data-var next-producer-id uint u1)

;; data maps
;;
;; Producer information mapping
(define-map producers
    { producer-id: uint }
    {
        name: (string-ascii 100),
        location: (string-ascii 200),
        certification-status: (string-ascii 50),
        registration-date: uint,
        is-active: bool,
        fair-trade-certified: bool
    }
)

;; Rubber batch information mapping
(define-map rubber-batches
    { batch-id: uint }
    {
        producer-id: uint,
        origin-location: (string-ascii 200),
        harvest-date: uint,
        quantity-kg: uint,
        quality-grade: (string-ascii 20),
        current-status: (string-ascii 50),
        fair-trade-verified: bool,
        creation-timestamp: uint,
        last-updated: uint
    }
)

;; Supply chain tracking for each batch
(define-map supply-chain-events
    { batch-id: uint, event-id: uint }
    {
        event-type: (string-ascii 50),
        location: (string-ascii 200),
        handler: principal,
        timestamp: uint,
        notes: (string-ascii 500)
    }
)

;; Track event count per batch
(define-map batch-event-count
    { batch-id: uint }
    { count: uint }
)

;; Authorization mapping for supply chain participants
(define-map authorized-handlers
    { handler: principal }
    {
        name: (string-ascii 100),
        role: (string-ascii 50),
        is-active: bool
    }
)

;; public functions
;;

;; Register a new rubber producer
(define-public (register-producer
    (name (string-ascii 100))
    (location (string-ascii 200))
    (certification-status (string-ascii 50))
    (fair-trade-certified bool))
    (let ((producer-id (var-get next-producer-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set producers
            { producer-id: producer-id }
            {
                name: name,
                location: location,
                certification-status: certification-status,
                registration-date: block-height,
                is-active: true,
                fair-trade-certified: fair-trade-certified
            }
        )
        (var-set next-producer-id (+ producer-id u1))
        (ok producer-id)
    )
)

;; Create a new rubber batch
(define-public (create-rubber-batch
    (producer-id uint)
    (origin-location (string-ascii 200))
    (harvest-date uint)
    (quantity-kg uint)
    (quality-grade (string-ascii 20)))
    (let ((batch-id (var-get next-batch-id))
          (producer-info (map-get? producers { producer-id: producer-id })))
        ;; Verify producer exists and is active
        (asserts! (is-some producer-info) err-not-found)
        (asserts! (get is-active (unwrap-panic producer-info)) err-unauthorized)

        ;; Create the batch
        (map-set rubber-batches
            { batch-id: batch-id }
            {
                producer-id: producer-id,
                origin-location: origin-location,
                harvest-date: harvest-date,
                quantity-kg: quantity-kg,
                quality-grade: quality-grade,
                current-status: "harvested",
                fair-trade-verified: (get fair-trade-certified (unwrap-panic producer-info)),
                creation-timestamp: block-height,
                last-updated: block-height
            }
        )

        ;; Initialize event count
        (map-set batch-event-count
            { batch-id: batch-id }
            { count: u0 }
        )

        ;; Add initial supply chain event
        (unwrap-panic (add-supply-chain-event batch-id "harvested" origin-location tx-sender "Initial harvest recorded"))

        (var-set next-batch-id (+ batch-id u1))
        (ok batch-id)
    )
)

;; Add a supply chain event
(define-public (add-supply-chain-event
    (batch-id uint)
    (event-type (string-ascii 50))
    (location (string-ascii 200))
    (handler principal)
    (notes (string-ascii 500)))
    (let ((batch-info (map-get? rubber-batches { batch-id: batch-id }))
          (event-count-info (map-get? batch-event-count { batch-id: batch-id }))
          (current-count (default-to u0 (get count event-count-info))))

        ;; Verify batch exists
        (asserts! (is-some batch-info) err-not-found)

        ;; Add the event
        (map-set supply-chain-events
            { batch-id: batch-id, event-id: current-count }
            {
                event-type: event-type,
                location: location,
                handler: handler,
                timestamp: block-height,
                notes: notes
            }
        )

        ;; Update event count
        (map-set batch-event-count
            { batch-id: batch-id }
            { count: (+ current-count u1) }
        )

        ;; Update batch status and timestamp
        (map-set rubber-batches
            { batch-id: batch-id }
            (merge (unwrap-panic batch-info)
                   { current-status: event-type, last-updated: block-height })
        )

        (ok current-count)
    )
)

;; Update batch status
(define-public (update-batch-status
    (batch-id uint)
    (new-status (string-ascii 50))
    (location (string-ascii 200))
    (notes (string-ascii 500)))
    (let ((batch-info (map-get? rubber-batches { batch-id: batch-id })))
        (asserts! (is-some batch-info) err-not-found)

        ;; Add supply chain event for status change
        (unwrap-panic (add-supply-chain-event batch-id new-status location tx-sender notes))

        (ok true)
    )
)

;; Authorize a supply chain handler
(define-public (authorize-handler
    (handler principal)
    (name (string-ascii 100))
    (role (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-handlers
            { handler: handler }
            {
                name: name,
                role: role,
                is-active: true
            }
        )
        (ok true)
    )
)

;; Deactivate a producer
(define-public (deactivate-producer (producer-id uint))
    (let ((producer-info (map-get? producers { producer-id: producer-id })))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-some producer-info) err-not-found)

        (map-set producers
            { producer-id: producer-id }
            (merge (unwrap-panic producer-info) { is-active: false })
        )
        (ok true)
    )
)

;; read only functions
;;

;; Get producer information
(define-read-only (get-producer (producer-id uint))
    (map-get? producers { producer-id: producer-id })
)

;; Get rubber batch information
(define-read-only (get-rubber-batch (batch-id uint))
    (map-get? rubber-batches { batch-id: batch-id })
)

;; Get supply chain event
(define-read-only (get-supply-chain-event (batch-id uint) (event-id uint))
    (map-get? supply-chain-events { batch-id: batch-id, event-id: event-id })
)

;; Get total events for a batch
(define-read-only (get-batch-event-count (batch-id uint))
    (default-to u0 (get count (map-get? batch-event-count { batch-id: batch-id })))
)

;; Get authorized handler info
(define-read-only (get-authorized-handler (handler principal))
    (map-get? authorized-handlers { handler: handler })
)

;; Check if producer is fair trade certified
(define-read-only (is-fair-trade-certified (batch-id uint))
    (match (map-get? rubber-batches { batch-id: batch-id })
        batch-info (some (get fair-trade-verified batch-info))
        none
    )
)

;; Get batch origin verification
(define-read-only (verify-batch-origin (batch-id uint))
    (match (map-get? rubber-batches { batch-id: batch-id })
        batch-info (let ((producer-info (map-get? producers { producer-id: (get producer-id batch-info) })))
            (match producer-info
                producer (some {
                    origin-location: (get origin-location batch-info),
                    producer-name: (get name producer),
                    producer-location: (get location producer),
                    certification-status: (get certification-status producer),
                    fair-trade-certified: (get fair-trade-certified producer)
                })
                none
            )
        )
        none
    )
)

;; Get current contract statistics
(define-read-only (get-contract-stats)
    {
        total-batches: (- (var-get next-batch-id) u1),
        total-producers: (- (var-get next-producer-id) u1),
        contract-owner: contract-owner
    }
)

;; private functions
;;

