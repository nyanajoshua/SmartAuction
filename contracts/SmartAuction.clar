;; SmartAuction - Decentralized auction platform
;; Handles auction listings, bidding, and settlements

;; Constants
(define-constant contract-owner tx-sender)
(define-constant listing-fee u100) ;; in STX
(define-constant min-bid-increment u10) ;; minimum bid increase
(define-constant auction-duration u144) ;; ~24 hours in blocks

;; Error codes
(define-constant err-not-owner (err u100))
(define-constant err-auction-ended (err u101))
(define-constant err-auction-active (err u102))
(define-constant err-low-bid (err u103))
(define-constant err-no-auction (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-already-claimed (err u106))

;; Data Types
(define-map auctions
    { auction-id: uint }
    {
        seller: principal,
        item-name: (string-ascii 50),
        description: (string-ascii 200),
        start-price: uint,
        end-block: uint,
        highest-bid: uint,
        highest-bidder: (optional principal),
        status: (string-ascii 20),
        claimed: bool
    }
)

(define-map user-bids
    { auction-id: uint, bidder: principal }
    { bid-amount: uint }
)

;; Data Variables
(define-data-var auction-nonce uint u0)
(define-data-var total-auctions uint u0)
(define-data-var platform-fees uint u0)

;; Public Functions

;; Create new auction listing
(define-public (create-auction (item-name (string-ascii 50)) 
                             (description (string-ascii 200)) 
                             (start-price uint))
    (let
        (
            (auction-id (+ (var-get auction-nonce) u1))
            (end-block (+ stacks-block-height auction-duration))
        )
        (try! (stx-transfer? listing-fee tx-sender contract-owner))
        (map-set auctions
            { auction-id: auction-id }
            {
                seller: tx-sender,
                item-name: item-name,
                description: description,
                start-price: start-price,
                end-block: end-block,
                highest-bid: u0,
                highest-bidder: none,
                status: "active",
                claimed: false
            }
        )
        (var-set auction-nonce auction-id)
        (var-set total-auctions (+ (var-get total-auctions) u1))
        (ok auction-id)
    )
)

;; Place bid on auction
(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let
        (
            (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
            (current-highest-bid (get highest-bid auction))
            (min-valid-bid (+ current-highest-bid min-bid-increment))
        )
        (asserts! (< stacks-block-height (get end-block auction)) (err err-auction-ended))
        (asserts! (>= bid-amount min-valid-bid) (err err-low-bid))
        
        ;; Transfer bid amount
        ;; (try! (stx-transfer? bid-amount tx-sender (contract-address)))
        
        ;; Refund previous highest bidder if exists
        ;; (match (get highest-bidder auction)
        ;;     prev-bidder (try! (as-contract (stx-transfer? current-highest-bid (as-contract tx-sender) prev-bidder)))
        ;;     none true
        ;; )
        
        ;; Update auction details
        (map-set auctions
            { auction-id: auction-id }
            (merge auction {
                highest-bid: bid-amount,
                highest-bidder: (some tx-sender)
            })
        )
        
        ;; Record user bid
        (map-set user-bids
            { auction-id: auction-id, bidder: tx-sender }
            { bid-amount: bid-amount }
        )
        
        (ok true)
    )
)

;; Claim auction item (for winner)
(define-public (claim-item (auction-id uint))
    (let
        (
            (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
        )
        (asserts! (> stacks-block-height (get end-block auction)) (err err-auction-active))
        (asserts! (is-eq (some tx-sender) (get highest-bidder auction)) (err err-unauthorized))
        (asserts! (not (get claimed auction)) (err err-already-claimed))
        
        ;; Transfer winning bid to seller
        ;; (try! (as-contract (stx-transfer? (get highest-bid auction) (as-contract tx-sender) (get seller auction))))
        
        ;; Mark auction as claimed
        (map-set auctions
            { auction-id: auction-id }
            (merge auction {
                status: "completed",
                claimed: true
            })
        )
        
        (ok true)
    )
)

;; Read-only functions

(define-read-only (get-auction-details (auction-id uint))
    (map-get? auctions {auction-id: auction-id})
)

(define-read-only (get-user-bid (auction-id uint) (bidder principal))
    (map-get? user-bids {auction-id: auction-id, bidder: bidder})
)

(define-read-only (get-total-auctions)
    (var-get total-auctions)
)

(define-read-only (is-auction-active (auction-id uint))
    (match (map-get? auctions {auction-id: auction-id})
        auction (< stacks-block-height (get end-block auction))
        false
    )
)

(define-constant err-reserve-not-met (err u107))

(define-map auction-reserve-prices 
    { auction-id: uint }
    { reserve-price: uint }
)

(define-public (set-reserve-price (auction-id uint) (reserve-price uint))
    (let (
        (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
    )
    (asserts! (is-eq tx-sender (get seller auction)) (err err-not-owner))
    (map-set auction-reserve-prices 
        {auction-id: auction-id}
        {reserve-price: reserve-price}
    )
    (ok true))
)

(define-read-only (get-reserve-price (auction-id uint))
    (map-get? auction-reserve-prices {auction-id: auction-id})
)


(define-constant err-above-buy-now (err u108))

(define-map buy-now-prices
    { auction-id: uint }
    { buy-now-price: uint }
)

(define-public (set-buy-now-price (auction-id uint) (price uint))
    (let (
        (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
    )
    (asserts! (is-eq tx-sender (get seller auction)) (err err-not-owner))
    (map-set buy-now-prices 
        {auction-id: auction-id}
        {buy-now-price: price}
    )
    (ok true))
)

(define-public (buy-now (auction-id uint))
    (let (
        (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
        (buy-now-data (unwrap! (map-get? buy-now-prices {auction-id: auction-id}) (err err-no-auction)))
    )
    (asserts! (< stacks-block-height (get end-block auction)) (err err-auction-ended))
    (map-set auctions
        {auction-id: auction-id}
        (merge auction {
            highest-bid: (get buy-now-price buy-now-data),
            highest-bidder: (some tx-sender),
            status: "completed",
            claimed: true
        })
    )
    (ok true))
)


(define-map auction-categories
    { auction-id: uint }
    { category: (string-ascii 20) }
)

(define-public (set-auction-category (auction-id uint) (category (string-ascii 20)))
    (let (
        (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
    )
    (asserts! (is-eq tx-sender (get seller auction)) (err err-not-owner))
    (map-set auction-categories
        {auction-id: auction-id}
        {category: category}
    )
    (ok true))
)

(define-read-only (get-auction-category (auction-id uint))
    (map-get? auction-categories {auction-id: auction-id})
)


(define-map user-watchlist
    { user: principal, auction-id: uint }
    { watching: bool }
)

(define-public (toggle-watchlist (auction-id uint))
    (let (
        (current-status (default-to false (get watching (map-get? user-watchlist {user: tx-sender, auction-id: auction-id}))))
    )
    (map-set user-watchlist
        {user: tx-sender, auction-id: auction-id}
        {watching: (not current-status)}
    )
    (ok true))
)

(define-read-only (is-watching (user principal) (auction-id uint))
    (get watching (default-to {watching: false} 
        (map-get? user-watchlist {user: user, auction-id: auction-id})))
)


(define-map seller-ratings
    { seller: principal }
    { total-rating: uint, rating-count: uint }
)

(define-public (rate-seller (seller principal) (rating uint))
    (let (
        (current-rating (default-to {total-rating: u0, rating-count: u0} 
            (map-get? seller-ratings {seller: seller})))
    )
    (asserts! (<= rating u5) (err u109))
    (map-set seller-ratings
        {seller: seller}
        {
            total-rating: (+ (get total-rating current-rating) rating),
            rating-count: (+ (get rating-count current-rating) u1)
        }
    )
    (ok true))
)

(define-read-only (get-seller-rating (seller principal))
    (map-get? seller-ratings {seller: seller})
)

(define-constant bid-time-extension u10)

(define-public (place-bid-with-extension (auction-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
        (current-highest-bid (get highest-bid auction))
        (min-valid-bid (+ current-highest-bid min-bid-increment))
        (blocks-remaining (- (get end-block auction) stacks-block-height))
    )
    (asserts! (< stacks-block-height (get end-block auction)) (err err-auction-ended))
    (asserts! (>= bid-amount min-valid-bid) (err err-low-bid))
    
    (if (<= blocks-remaining u3)
        (map-set auctions
            {auction-id: auction-id}
            (merge auction {
                highest-bid: bid-amount,
                highest-bidder: (some tx-sender),
                end-block: (+ (get end-block auction) bid-time-extension)
            }))
        (map-set auctions
            {auction-id: auction-id}
            (merge auction {
                highest-bid: bid-amount,
                highest-bidder: (some tx-sender)
            }))
    )
    (ok true))
)


(define-map auction-quantities
    { auction-id: uint }
    { total-quantity: uint, remaining-quantity: uint }
)

(define-public (create-multi-item-auction (item-name (string-ascii 50)) 
                                        (description (string-ascii 200)) 
                                        (start-price uint)
                                        (quantity uint))
    (let (
        (auction-id (+ (var-get auction-nonce) u1))
        (end-block (+ stacks-block-height auction-duration))
    )
    (try! (stx-transfer? listing-fee tx-sender contract-owner))
    (map-set auctions
        {auction-id: auction-id}
        {
            seller: tx-sender,
            item-name: item-name,
            description: description,
            start-price: start-price,
            end-block: end-block,
            highest-bid: u0,
            highest-bidder: none,
            status: "active",
            claimed: false
        }
    )
    (map-set auction-quantities
        {auction-id: auction-id}
        {
            total-quantity: quantity,
            remaining-quantity: quantity
        }
    )
    (var-set auction-nonce auction-id)
    (var-set total-auctions (+ (var-get total-auctions) u1))
    (ok auction-id))
)

(define-read-only (get-auction-quantity (auction-id uint))
    (map-get? auction-quantities {auction-id: auction-id})
)


(define-constant err-invalid-duration (err u110))
(define-constant min-duration u72)
(define-constant max-duration u720)

(define-public (update-auction-duration (auction-id uint) (new-duration uint))
    (let (
        (auction (unwrap! (map-get? auctions {auction-id: auction-id}) (err err-no-auction)))
        (current-block stacks-block-height)
        (new-end-block (+ current-block new-duration))
    )
    (asserts! (is-eq tx-sender (get seller auction)) (err err-not-owner))
    (asserts! (is-eq (get status auction) "active") (err err-auction-ended))
    (asserts! (>= new-duration min-duration) (err err-invalid-duration))
    (asserts! (<= new-duration max-duration) (err err-invalid-duration))
    
    (map-set auctions
        {auction-id: auction-id}
        (merge auction {
            end-block: new-end-block
        })
    )
    (ok true))
)


(define-constant err-empty-bundle (err u130))
(define-constant max-bundle-items u5)

(define-map auction-bundles
    { bundle-id: uint }
    { 
        item-names: (list 5 (string-ascii 50)),
        item-count: uint
    }
)

(define-public (create-bundle-auction
    (items (list 5 (string-ascii 50)))
    (description (string-ascii 200))
    (start-price uint))
    
    (let (
        (auction-id (+ (var-get auction-nonce) u1))
        (item-count (len items))
    )
    (asserts! (> item-count u0) (err err-empty-bundle))
    ;; (asserts! (<= item-count max-bundle-items) (err u131))
    ;; (try! (stx-transfer? listing-fee tx-sender contract-owner))
    
    (map-set auctions
        { auction-id: auction-id }
        {
            seller: tx-sender,
            item-name: "Bundle",
            description: description,
            start-price: start-price,
            end-block: (+ stacks-block-height auction-duration),
            highest-bid: u0,
            highest-bidder: none,
            status: "active",
            claimed: false
        }
    )
    
    (map-set auction-bundles
        { bundle-id: auction-id }
        {
            item-names: items,
            item-count: item-count
        }
    )
    
    (var-set auction-nonce auction-id)
    (var-set total-auctions (+ (var-get total-auctions) u1))
    (ok auction-id))
)

(define-read-only (get-bundle-details (bundle-id uint))
    (map-get? auction-bundles {bundle-id: bundle-id})
)


(define-constant err-invalid-start-time (err u120))

(define-map scheduled-auctions
    { auction-id: uint }
    { start-block: uint }
)

(define-public (create-scheduled-auction 
    (item-name (string-ascii 50))
    (description (string-ascii 200))
    (start-price uint)
    (start-block uint))
    
    (let (
        (auction-id (+ (var-get auction-nonce) u1))
        (current-block stacks-block-height)
    )
    (asserts! (> start-block current-block) (err err-invalid-start-time))
    ;; (try! (stx-transfer? listing-fee tx-sender contract-owner))
    
    (map-set auctions
        { auction-id: auction-id }
        {
            seller: tx-sender,
            item-name: item-name,
            description: description,
            start-price: start-price,
            end-block: (+ start-block auction-duration),
            highest-bid: u0,
            highest-bidder: none,
            status: "scheduled",
            claimed: false
        }
    )
    
    (map-set scheduled-auctions
        { auction-id: auction-id }
        { start-block: start-block }
    )
    
    (var-set auction-nonce auction-id)
    (var-set total-auctions (+ (var-get total-auctions) u1))
    (ok auction-id))
)

(define-read-only (get-auction-start-time (auction-id uint))
    (map-get? scheduled-auctions {auction-id: auction-id})
)