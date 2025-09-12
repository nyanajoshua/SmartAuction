;; Auction Insurance System Contract
;; Provides sellers with insurance coverage against auction risks

;; Error constants
(define-constant err-unauthorized (err u400))
(define-constant err-policy-not-found (err u401))
(define-constant err-policy-already-exists (err u402))
(define-constant err-invalid-premium (err u403))
(define-constant err-invalid-coverage (err u404))
(define-constant err-claim-already-processed (err u405))
(define-constant err-invalid-claim (err u406))
(define-constant err-insufficient-funds (err u407))
(define-constant err-policy-expired (err u408))

;; Constants
(define-constant insurance-admin tx-sender)
(define-constant base-premium-rate u5) ;; 5% of insured value
(define-constant max-coverage-percentage u80) ;; Maximum 80% of expected value
(define-constant min-coverage-amount u100) ;; Minimum coverage amount
(define-constant claim-processing-period u144) ;; 24 hours to process claims

;; Insurance policy types
(define-constant COVERAGE_TYPE_NO_SALE "no-sale")
(define-constant COVERAGE_TYPE_LOW_PRICE "low-price") 
(define-constant COVERAGE_TYPE_EARLY_CANCEL "early-cancel")

;; Data variables
(define-data-var policy-id-nonce uint u1)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var insurance-pool-balance uint u0)

;; Insurance policies for auctions
(define-map insurance-policies
  uint ;; policy-id
  {
    policy-id: uint,
    auction-id: uint,
    seller: principal,
    coverage-type: (string-ascii 20),
    insured-value: uint,
    coverage-amount: uint,
    premium-paid: uint,
    issue-date: uint,
    expiry-date: uint,
    active: bool,
    claimed: bool
  }
)

;; Insurance claims
(define-map insurance-claims
  uint ;; policy-id
  {
    policy-id: uint,
    claim-amount: uint,
    claim-reason: (string-ascii 50),
    claim-date: uint,
    processed: bool,
    approved: bool,
    payout-amount: uint,
    processing-date: (optional uint)
  }
)

;; Policy lookup by auction
(define-map auction-policies
  uint ;; auction-id
  {policies: (list 3 uint)} ;; Max 3 policies per auction
)

;; Purchase insurance policy for auction
(define-public (purchase-insurance
  (auction-id uint)
  (coverage-type (string-ascii 20))
  (insured-value uint)
  (coverage-percentage uint))
  (let
    (
      (policy-id (var-get policy-id-nonce))
      (coverage-amount (/ (* insured-value coverage-percentage) u100))
      (premium-amount (/ (* coverage-amount base-premium-rate) u100))
      (current-policies (default-to {policies: (list)} (map-get? auction-policies auction-id)))
    )
    ;; Validate inputs
    (asserts! (or (is-eq coverage-type COVERAGE_TYPE_NO_SALE)
                  (or (is-eq coverage-type COVERAGE_TYPE_LOW_PRICE)
                      (is-eq coverage-type COVERAGE_TYPE_EARLY_CANCEL))) err-invalid-coverage)
    (asserts! (and (> coverage-percentage u0) (<= coverage-percentage max-coverage-percentage)) err-invalid-coverage)
    (asserts! (>= coverage-amount min-coverage-amount) err-invalid-coverage)
    (asserts! (> premium-amount u0) err-invalid-premium)
    
    ;; Collect premium
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    
    ;; Create insurance policy
    (map-set insurance-policies policy-id
      {
        policy-id: policy-id,
        auction-id: auction-id,
        seller: tx-sender,
        coverage-type: coverage-type,
        insured-value: insured-value,
        coverage-amount: coverage-amount,
        premium-paid: premium-amount,
        issue-date: stacks-block-height,
        expiry-date: (+ stacks-block-height u1008), ;; ~7 days coverage
        active: true,
        claimed: false
      }
    )
    
    ;; Update auction policies list
    (map-set auction-policies auction-id
      {policies: (unwrap-panic (as-max-len? 
        (append (get policies current-policies) policy-id) u3))}
    )
    
    ;; Update insurance pool
    (var-set total-premiums-collected (+ (var-get total-premiums-collected) premium-amount))
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) premium-amount))
    (var-set policy-id-nonce (+ policy-id u1))
    
    (ok policy-id)
  )
)

;; Submit insurance claim
(define-public (submit-claim
  (policy-id uint)
  (claim-amount uint)
  (claim-reason (string-ascii 50)))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (current-block stacks-block-height)
    )
    ;; Validate claim
    (asserts! (is-eq tx-sender (get seller policy)) err-unauthorized)
    (asserts! (get active policy) err-policy-expired)
    (asserts! (not (get claimed policy)) err-claim-already-processed)
    (asserts! (< current-block (get expiry-date policy)) err-policy-expired)
    (asserts! (<= claim-amount (get coverage-amount policy)) err-invalid-claim)
    (asserts! (> claim-amount u0) err-invalid-claim)
    
    ;; Record claim
    (map-set insurance-claims policy-id
      {
        policy-id: policy-id,
        claim-amount: claim-amount,
        claim-reason: claim-reason,
        claim-date: current-block,
        processed: false,
        approved: false,
        payout-amount: u0,
        processing-date: none
      }
    )
    
    (ok true)
  )
)

;; Process insurance claim (admin function)
(define-public (process-claim (policy-id uint) (approved bool) (payout-amount uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (claim (unwrap! (map-get? insurance-claims policy-id) err-claim-already-processed))
      (current-block stacks-block-height)
    )
    ;; Only admin can process claims
    (asserts! (is-eq tx-sender insurance-admin) err-unauthorized)
    (asserts! (not (get processed claim)) err-claim-already-processed)
    (asserts! (get active policy) err-policy-expired)
    
    ;; Update claim status
    (map-set insurance-claims policy-id
      (merge claim {
        processed: true,
        approved: approved,
        payout-amount: payout-amount,
        processing-date: (some current-block)
      })
    )
    
    ;; Process payout if approved
    (if approved
      (begin
        (asserts! (<= payout-amount (var-get insurance-pool-balance)) err-insufficient-funds)
        (try! (as-contract (stx-transfer? payout-amount tx-sender (get seller policy))))
        (var-set insurance-pool-balance (- (var-get insurance-pool-balance) payout-amount))
        (var-set total-claims-paid (+ (var-get total-claims-paid) payout-amount))
        
        ;; Mark policy as claimed
        (map-set insurance-policies policy-id
          (merge policy {claimed: true, active: false})
        )
      )
      true
    )
    
    (ok approved)
  )
)

;; Cancel insurance policy (partial refund)
(define-public (cancel-policy (policy-id uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
      (current-block stacks-block-height)
      (blocks-elapsed (- current-block (get issue-date policy)))
      (total-coverage-period (- (get expiry-date policy) (get issue-date policy)))
      (usage-percentage (/ (* blocks-elapsed u100) total-coverage-period))
      (refund-percentage (- u100 usage-percentage))
      (refund-amount (/ (* (get premium-paid policy) refund-percentage) u100))
    )
    ;; Only policy holder can cancel
    (asserts! (is-eq tx-sender (get seller policy)) err-unauthorized)
    (asserts! (get active policy) err-policy-expired)
    (asserts! (not (get claimed policy)) err-claim-already-processed)
    
    ;; Process refund
    (if (> refund-amount u0)
      (begin
        (try! (as-contract (stx-transfer? refund-amount tx-sender (get seller policy))))
        (var-set insurance-pool-balance (- (var-get insurance-pool-balance) refund-amount))
      )
      true
    )
    
    ;; Deactivate policy
    (map-set insurance-policies policy-id
      (merge policy {active: false})
    )
    
    (ok refund-amount)
  )
)

;; Admin function to fund insurance pool
(define-public (fund-insurance-pool (amount uint))
  (begin
    (asserts! (is-eq tx-sender insurance-admin) err-unauthorized)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) amount))
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies policy-id)
)

(define-read-only (get-insurance-claim (policy-id uint))
  (map-get? insurance-claims policy-id)
)

(define-read-only (get-auction-policies (auction-id uint))
  (map-get? auction-policies auction-id)
)

(define-read-only (calculate-premium 
  (coverage-type (string-ascii 20))
  (insured-value uint)
  (coverage-percentage uint))
  (let
    (
      (coverage-amount (/ (* insured-value coverage-percentage) u100))
      (base-premium (/ (* coverage-amount base-premium-rate) u100))
      (risk-multiplier (if (is-eq coverage-type COVERAGE_TYPE_NO_SALE) u100
                       (if (is-eq coverage-type COVERAGE_TYPE_LOW_PRICE) u150
                           u120))) ;; Early cancel has 120% risk multiplier
    )
    (/ (* base-premium risk-multiplier) u100)
  )
)

(define-read-only (get-insurance-pool-balance)
  (var-get insurance-pool-balance)
)

(define-read-only (get-total-premiums-collected)
  (var-get total-premiums-collected)
)

(define-read-only (get-total-claims-paid)
  (var-get total-claims-paid)
)

(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? insurance-policies policy-id)
    policy (and
            (get active policy)
            (< stacks-block-height (get expiry-date policy))
            (not (get claimed policy)))
    false
  )
)

(define-read-only (get-policy-refund-amount (policy-id uint))
  (match (map-get? insurance-policies policy-id)
    policy (let
            (
              (current-block stacks-block-height)
              (blocks-elapsed (- current-block (get issue-date policy)))
              (total-period (- (get expiry-date policy) (get issue-date policy)))
              (usage-pct (/ (* blocks-elapsed u100) total-period))
              (refund-pct (- u100 usage-pct))
            )
            (/ (* (get premium-paid policy) refund-pct) u100))
    u0
  )
)

(define-read-only (get-last-policy-id)
  (- (var-get policy-id-nonce) u1)
)
