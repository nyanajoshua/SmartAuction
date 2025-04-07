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
