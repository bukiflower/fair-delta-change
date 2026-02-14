;; Fair Delta Change - Disaster Relief Platform
;; A decentralized disaster relief smart contract for transparent aid distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-already-validated (err u104))
(define-constant err-invalid-status (err u105))

;; Data Variables
(define-data-var disaster-counter uint u0)
(define-data-var validator-threshold uint u3)

;; Data Maps
(define-map disasters
    uint
    {
        name: (string-ascii 100),
        location: (string-ascii 100),
        funds-requested: uint,
        funds-raised: uint,
        funds-distributed: uint,
        status: (string-ascii 20),
        beneficiary: principal,
        created-at: uint
    }
)

(define-map validators
    principal
    {
        is-active: bool,
        validations-count: uint,
        reputation-score: uint
    }
)

(define-map aid-distributions
    {disaster-id: uint, distribution-id: uint}
    {
        amount: uint,
        recipient: principal,
        description: (string-ascii 200),
        validations: uint,
        is-verified: bool,
        timestamp: uint
    }
)

(define-map distribution-validators
    {disaster-id: uint, distribution-id: uint, validator: principal}
    bool
)

(define-map disaster-distribution-counter
    uint
    uint
)

;; Read-only functions

(define-read-only (get-disaster (disaster-id uint))
    (map-get? disasters disaster-id)
)

(define-read-only (get-validator-info (validator principal))
    (map-get? validators validator)
)

(define-read-only (get-distribution (disaster-id uint) (distribution-id uint))
    (map-get? aid-distributions {disaster-id: disaster-id, distribution-id: distribution-id})
)

(define-read-only (has-validator-verified (disaster-id uint) (distribution-id uint) (validator principal))
    (default-to false (map-get? distribution-validators {disaster-id: disaster-id, distribution-id: distribution-id, validator: validator}))
)

(define-read-only (get-disaster-count)
    (var-get disaster-counter)
)

;; Public functions

(define-public (register-disaster (name (string-ascii 100)) (location (string-ascii 100)) (funds-requested uint) (beneficiary principal))
    (let
        (
            (disaster-id (+ (var-get disaster-counter) u1))
        )
        (map-set disasters disaster-id
            {
                name: name,
                location: location,
                funds-requested: funds-requested,
                funds-raised: u0,
                funds-distributed: u0,
                status: "active",
                beneficiary: beneficiary,
                created-at: block-height
            }
        )
        (map-set disaster-distribution-counter disaster-id u0)
        (var-set disaster-counter disaster-id)
        (ok disaster-id)
    )
)

(define-public (donate-to-disaster (disaster-id uint) (amount uint))
    (let
        (
            (disaster (unwrap! (map-get? disasters disaster-id) err-not-found))
        )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set disasters disaster-id
            (merge disaster {funds-raised: (+ (get funds-raised disaster) amount)})
        )
        (ok true)
    )
)

(define-public (register-validator)
    (begin
        (map-set validators tx-sender
            {
                is-active: true,
                validations-count: u0,
                reputation-score: u100
            }
        )
        (ok true)
    )
)

(define-public (distribute-aid (disaster-id uint) (amount uint) (recipient principal) (description (string-ascii 200)))
    (let
        (
            (disaster (unwrap! (map-get? disasters disaster-id) err-not-found))
            (distribution-id (+ (default-to u0 (map-get? disaster-distribution-counter disaster-id)) u1))
            (contract-balance (stx-get-balance (as-contract tx-sender)))
        )
        (asserts! (is-eq tx-sender (get beneficiary disaster)) err-unauthorized)
        (asserts! (>= contract-balance amount) err-insufficient-funds)
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        (map-set aid-distributions {disaster-id: disaster-id, distribution-id: distribution-id}
            {
                amount: amount,
                recipient: recipient,
                description: description,
                validations: u0,
                is-verified: false,
                timestamp: block-height
            }
        )
        (map-set disaster-distribution-counter disaster-id distribution-id)
        (map-set disasters disaster-id
            (merge disaster {funds-distributed: (+ (get funds-distributed disaster) amount)})
        )
        (ok distribution-id)
    )
)

(define-public (validate-distribution (disaster-id uint) (distribution-id uint))
    (let
        (
            (validator-info (unwrap! (map-get? validators tx-sender) err-unauthorized))
            (distribution (unwrap! (map-get? aid-distributions {disaster-id: disaster-id, distribution-id: distribution-id}) err-not-found))
            (already-validated (has-validator-verified disaster-id distribution-id tx-sender))
        )
        (asserts! (get is-active validator-info) err-unauthorized)
        (asserts! (not already-validated) err-already-validated)
        (map-set distribution-validators {disaster-id: disaster-id, distribution-id: distribution-id, validator: tx-sender} true)
        (let
            (
                (new-validations (+ (get validations distribution) u1))
                (is-now-verified (>= new-validations (var-get validator-threshold)))
            )
            (map-set aid-distributions {disaster-id: disaster-id, distribution-id: distribution-id}
                (merge distribution {
                    validations: new-validations,
                    is-verified: is-now-verified
                })
            )
            (map-set validators tx-sender
                (merge validator-info {
                    validations-count: (+ (get validations-count validator-info) u1),
                    reputation-score: (+ (get reputation-score validator-info) u10)
                })
            )
            (ok is-now-verified)
        )
    )
)

(define-public (update-disaster-status (disaster-id uint) (new-status (string-ascii 20)))
    (let
        (
            (disaster (unwrap! (map-get? disasters disaster-id) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set disasters disaster-id
            (merge disaster {status: new-status})
        )
        (ok true)
    )
)

(define-public (set-validator-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set validator-threshold new-threshold)
        (ok true)
    )
)

;; Initialize contract
(begin
    (map-set validators contract-owner
        {
            is-active: true,
            validations-count: u0,
            reputation-score: u100
        }
    )
)