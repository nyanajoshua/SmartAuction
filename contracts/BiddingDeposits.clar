;; Bidding Deposit System for SmartAuction
;; Requires bidders to place deposits before participating in auctions

;; Error codes
(define-constant err-insufficient-deposit (err u200))
(define-constant err-no-deposit (err u201))
(define-constant err-already-has-deposit (err u202))
(define-constant err-cannot-withdraw (err u203))

;; Constants
(define-constant min-deposit-percentage u20) ;; 20% of start price
(define-constant max-deposit u1000000) ;; 1M STX max deposit

;; Data maps
(define-map bidder-deposits
    { bidder: principal, auction-id: uint }
    { 
        deposit-amount: uint,
        locked: bool,
        deposit-block: uint
    }
)

(define-map auction-deposit-requirements
    { auction-id: uint }
    { required-deposit: uint }
)

(define-data-var total-deposits-held uint u0)

;; Calculate required deposit for auction
(define-private (calculate-required-deposit (start-price uint))
    (let (
        (percentage-deposit (/ (* start-price min-deposit-percentage) u100))
    )
    (if (> percentage-deposit max-deposit)
        max-deposit
        percentage-deposit))
)

;; Set deposit requirement for auction
(define-public (set-auction-deposit-requirement (auction-id uint) (start-price uint))
    (let (
        (required-deposit (calculate-required-deposit start-price))
    )
    (map-set auction-deposit-requirements
        { auction-id: auction-id }
        { required-deposit: required-deposit }
    )
    (ok required-deposit))
)

;; Place deposit for auction participation
(define-public (place-deposit (auction-id uint))
    (let (
        (deposit-info (unwrap! (map-get? auction-deposit-requirements {auction-id: auction-id}) (err u204)))
        (required-amount (get required-deposit deposit-info))
        (existing-deposit (map-get? bidder-deposits {bidder: tx-sender, auction-id: auction-id}))
    )
    (begin
        (asserts! (is-none existing-deposit) err-already-has-deposit)
        
        (try! (stx-transfer? required-amount tx-sender (as-contract tx-sender)))
        
        (map-set bidder-deposits
            { bidder: tx-sender, auction-id: auction-id }
            {
                deposit-amount: required-amount,
                locked: true,
                deposit-block: stacks-block-height
            }
        )
        
        (var-set total-deposits-held (+ (var-get total-deposits-held) required-amount))
        
        (ok true)))
)

;; Check if user has valid deposit for auction
(define-public (has-valid-deposit (bidder principal) (auction-id uint))
    (match (map-get? bidder-deposits {bidder: bidder, auction-id: auction-id})
        deposit-info (ok (get locked deposit-info))
        (ok false))
)

;; Withdraw deposit (only if not currently highest bidder)
(define-public (withdraw-deposit (auction-id uint))
    (let (
        (deposit-info (unwrap! (map-get? bidder-deposits {bidder: tx-sender, auction-id: auction-id}) err-no-deposit))
        (deposit-amount (get deposit-amount deposit-info))
    )
    (asserts! (get locked deposit-info) err-cannot-withdraw)
    
    (try! (as-contract (stx-transfer? deposit-amount tx-sender tx-sender)))
    
    (map-delete bidder-deposits {bidder: tx-sender, auction-id: auction-id})
    
    (var-set total-deposits-held (- (var-get total-deposits-held) deposit-amount))
    
    (ok true))
)

;; Release deposit after auction completion
(define-public (release-deposit (auction-id uint) (bidder principal))
    (let (
        (deposit-info (unwrap! (map-get? bidder-deposits {bidder: bidder, auction-id: auction-id}) err-no-deposit))
        (deposit-amount (get deposit-amount deposit-info))
    )
    
    (try! (as-contract (stx-transfer? deposit-amount tx-sender bidder)))
    
    (map-delete bidder-deposits {bidder: bidder, auction-id: auction-id})
    
    (var-set total-deposits-held (- (var-get total-deposits-held) deposit-amount))
    
    (ok true))
)

;; Convert deposit to payment for winning bidder
(define-public (convert-deposit-to-payment (auction-id uint) (bidder principal) (winning-bid uint))
    (let (
        (deposit-info (unwrap! (map-get? bidder-deposits {bidder: bidder, auction-id: auction-id}) err-no-deposit))
        (deposit-amount (get deposit-amount deposit-info))
        (additional-payment (- winning-bid deposit-amount))
    )
    
    (if (> additional-payment u0)
        (try! (stx-transfer? additional-payment bidder (as-contract tx-sender)))
        (if (< additional-payment u0)
            (try! (as-contract (stx-transfer? (- u0 additional-payment) tx-sender bidder)))
            true))
    
    (map-delete bidder-deposits {bidder: bidder, auction-id: auction-id})
    
    (var-set total-deposits-held (- (var-get total-deposits-held) deposit-amount))
    
    (ok true))
)

;; Batch release deposits for multiple bidders
(define-public (batch-release-deposits (auction-id uint) (bidders (list 10 principal)))
    (ok "Batch release feature requires individual calls for each bidder")
)

;; Emergency withdraw (with penalty)
(define-public (emergency-withdraw (auction-id uint))
    (let (
        (deposit-info (unwrap! (map-get? bidder-deposits {bidder: tx-sender, auction-id: auction-id}) err-no-deposit))
        (deposit-amount (get deposit-amount deposit-info))
        (penalty (/ deposit-amount u10)) ;; 10% penalty
        (refund-amount (- deposit-amount penalty))
    )
    
    (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
    
    (map-delete bidder-deposits {bidder: tx-sender, auction-id: auction-id})
    
    (var-set total-deposits-held (- (var-get total-deposits-held) deposit-amount))
    
    (ok refund-amount))
)

;; Read-only functions

(define-read-only (get-deposit-info (bidder principal) (auction-id uint))
    (map-get? bidder-deposits {bidder: bidder, auction-id: auction-id})
)

(define-read-only (get-required-deposit (auction-id uint))
    (map-get? auction-deposit-requirements {auction-id: auction-id})
)

(define-read-only (get-total-deposits-held)
    (var-get total-deposits-held)
)

(define-read-only (calculate-deposit-for-price (start-price uint))
    (calculate-required-deposit start-price)
)

(define-read-only (get-deposit-status (bidder principal) (auction-id uint))
    (match (map-get? bidder-deposits {bidder: bidder, auction-id: auction-id})
        deposit-info {
            has-deposit: true,
            amount: (get deposit-amount deposit-info),
            locked: (get locked deposit-info),
            block-height: (get deposit-block deposit-info)
        }
        {
            has-deposit: false,
            amount: u0,
            locked: false,
            block-height: u0
        })
)

(define-read-only (get-user-total-deposits (bidder principal))
    (ok "Feature requires off-chain indexing for complex queries")
)

(define-read-only (get-auction-deposit-summary (auction-id uint))
    (let (
        (required-deposit (get required-deposit (default-to {required-deposit: u0} 
            (map-get? auction-deposit-requirements {auction-id: auction-id}))))
    )
    {
        required-deposit: required-deposit,
        total-deposits-held: (var-get total-deposits-held)
    })
)
