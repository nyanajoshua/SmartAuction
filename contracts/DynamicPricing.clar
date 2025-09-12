;; Dynamic Auction Pricing Engine for SmartAuction
;; Analyzes market trends and provides intelligent pricing recommendations

;; Constants
(define-constant contract-owner tx-sender)
(define-constant price-adjustment-window u144) ;; 24 hours in blocks
(define-constant min-sample-size u5)
(define-constant max-price-adjustment-percentage u50) ;; 50% max adjustment

;; Error codes
(define-constant err-insufficient-data (err u300))
(define-constant err-invalid-category (err u301))
(define-constant err-invalid-price-range (err u302))
(define-constant err-unauthorized-access (err u303))
(define-constant err-pricing-disabled (err u304))

;; Data maps for market analysis
(define-map category-market-data
    { category: (string-ascii 20) }
    {
        avg-selling-price: uint,
        success-rate: uint,
        total-listings: uint,
        total-sales: uint,
        price-trend: (string-ascii 10),
        last-updated: uint
    }
)

(define-map price-history
    { category: (string-ascii 20), week: uint }
    {
        weekly-avg-price: uint,
        weekly-volume: uint,
        weekly-listings: uint,
        demand-score: uint
    }
)

(define-map pricing-recommendations
    { seller: principal, auction-id: uint }
    {
        recommended-start-price: uint,
        recommended-reserve: uint,
        confidence-score: uint,
        market-demand: (string-ascii 11),
        pricing-strategy: (string-ascii 12)
    }
)

(define-map bidding-velocity-tracker
    { auction-id: uint }
    {
        bids-per-hour: uint,
        last-bid-block: uint,
        velocity-trend: (string-ascii 12),
        predicted-final-price: uint
    }
)

(define-map demand-indicators
    { category: (string-ascii 20) }
    {
        watchers-per-listing: uint,
        bid-frequency: uint,
        completion-rate: uint,
        premium-percentage: uint
    }
)

(define-map seller-performance
    { seller: principal }
    {
        average-markup: uint,
        success-percentage: uint,
        optimal-price-range: { min: uint, max: uint },
        preferred-categories: (list 3 (string-ascii 20))
    }
)

;; Data variables
(define-data-var pricing-engine-enabled bool true)
(define-data-var global-market-sentiment (string-ascii 11) "neutral")
(define-data-var total-market-analysis uint u0)

;; Core market analysis functions

(define-public (update-category-data (category (string-ascii 20)) 
                                   (selling-price uint) 
                                   (was-successful bool))
    (let (
        (current-data (default-to 
            {avg-selling-price: u0, success-rate: u0, total-listings: u0, 
             total-sales: u0, price-trend: "stable", last-updated: u0}
            (map-get? category-market-data {category: category})))
        (new-listings (+ (get total-listings current-data) u1))
        (new-sales (if was-successful (+ (get total-sales current-data) u1) (get total-sales current-data)))
        (new-avg-price (if was-successful 
            (/ (+ (* (get avg-selling-price current-data) (get total-sales current-data)) selling-price) new-sales)
            (get avg-selling-price current-data)))
        (new-success-rate (/ (* new-sales u100) new-listings))
    )
    
    (map-set category-market-data
        {category: category}
        {
            avg-selling-price: new-avg-price,
            success-rate: new-success-rate,
            total-listings: new-listings,
            total-sales: new-sales,
            price-trend: (calculate-price-trend category new-avg-price),
            last-updated: stacks-block-height
        }
    )
    
    (var-set total-market-analysis (+ (var-get total-market-analysis) u1))
    (ok true))
)

(define-private (calculate-price-trend (category (string-ascii 20)) (current-price uint))
    (let (
        (current-week (/ stacks-block-height u1008)) ;; ~1 week in blocks
        (prev-week (- current-week u1))
        (prev-data (map-get? price-history {category: category, week: prev-week}))
    )
    
    (match prev-data
        prev-week-data (let (
            (prev-price (get weekly-avg-price prev-week-data))
            (price-change-pct (if (> prev-price u0) 
                (/ (* (- current-price prev-price) u100) prev-price) 
                u0))
        )
        (if (> price-change-pct u10) "rising"
        (if (< price-change-pct (- u0 u10)) "falling" "stable")))
        "stable"))
)

(define-public (calculate-pricing-recommendation (seller principal) 
                                               (category (string-ascii 20))
                                               (base-price uint)
                                               (auction-id uint))
    (if (not (var-get pricing-engine-enabled))
        (err err-pricing-disabled)
        (let (
            (market-data (map-get? category-market-data {category: category}))
        )
        
        (match market-data
            category-data (let (
                (market-avg (get avg-selling-price category-data))
                (success-rate (get success-rate category-data))
                (demand-score (get-demand-score category))
                (adjustment-factor (calculate-adjustment-factor demand-score success-rate))
                (recommended-start (apply-price-adjustment base-price adjustment-factor))
                (recommended-reserve (/ (* recommended-start u80) u100)) ;; 80% of start price
                (confidence (calculate-confidence-score category-data demand-score))
            )
            
            (begin
                (map-set pricing-recommendations
                    {seller: seller, auction-id: auction-id}
                    {
                        recommended-start-price: recommended-start,
                        recommended-reserve: recommended-reserve,
                        confidence-score: confidence,
                        market-demand: (get-demand-level demand-score),
                        pricing-strategy: (get-optimal-strategy success-rate demand-score)
                    }
                )
                
                (ok {
                    start-price: recommended-start,
                    reserve-price: recommended-reserve,
                    confidence: confidence
                })))
            
            (err err-insufficient-data)))))

(define-private (calculate-adjustment-factor (demand-score uint) (success-rate uint))
    (let (
        (base-adjustment u100)
        (demand-modifier (if (> demand-score u70) u110 
                        (if (< demand-score u30) u90 u100)))
        (success-modifier (if (> success-rate u80) u105
                         (if (< success-rate u50) u95 u100)))
    )
    (/ (* demand-modifier success-modifier) u100))
)

(define-private (apply-price-adjustment (base-price uint) (adjustment-factor uint))
    (let (
        (adjusted-price (/ (* base-price adjustment-factor) u100))
        (max-increase (/ (* base-price (+ u100 max-price-adjustment-percentage)) u100))
        (max-decrease (/ (* base-price (- u100 max-price-adjustment-percentage)) u100))
    )
    (if (> adjusted-price max-increase) max-increase
    (if (< adjusted-price max-decrease) max-decrease adjusted-price)))
)

(define-private (calculate-confidence-score (market-data {avg-selling-price: uint, success-rate: uint, 
                                           total-listings: uint, total-sales: uint, 
                                           price-trend: (string-ascii 10), last-updated: uint})
                                          (demand-score uint))
    (let (
        (data-quality (if (>= (get total-listings market-data) min-sample-size) u25 u10))
        (freshness (if (<= (- stacks-block-height (get last-updated market-data)) price-adjustment-window) u25 u10))
        (success-confidence (/ (get success-rate market-data) u4)) ;; Max 25 points
        (demand-confidence (/ demand-score u4)) ;; Max 25 points
    )
    (+ data-quality freshness success-confidence demand-confidence))
)

(define-private (get-demand-score (category (string-ascii 20)))
    (match (map-get? demand-indicators {category: category})
        demand-data (let (
            (watchers-raw (* (get watchers-per-listing demand-data) u3))
            (watchers-score (if (> watchers-raw u30) u30 watchers-raw))
            (frequency-raw (get bid-frequency demand-data))
            (frequency-score (if (> frequency-raw u30) u30 frequency-raw))
            (completion-raw (/ (* (get completion-rate demand-data) u2) u5))
            (completion-score (if (> completion-raw u40) u40 completion-raw))
        )
        (+ watchers-score frequency-score completion-score))
        u50)
)

(define-private (get-demand-level (demand-score uint))
    (if (>= demand-score u80) "high"
    (if (>= demand-score u60) "medium-high"
    (if (>= demand-score u40) "medium"
    (if (>= demand-score u20) "low-medium" "low"))))
)

(define-private (get-optimal-strategy (success-rate uint) (demand-score uint))
    (if (and (>= success-rate u80) (>= demand-score u70)) "aggressive"
    (if (and (>= success-rate u60) (>= demand-score u50)) "balanced"
    (if (>= success-rate u40) "conservative" "cautious")))
)

;; Bidding velocity tracking
(define-public (track-bidding-velocity (auction-id uint) (new-bid bool))
    (let (
        (current-velocity (default-to 
            {bids-per-hour: u0, last-bid-block: u0, velocity-trend: "stable", predicted-final-price: u0}
            (map-get? bidding-velocity-tracker {auction-id: auction-id})))
        (blocks-since-last (- stacks-block-height (get last-bid-block current-velocity)))
        (hourly-rate (if (> blocks-since-last u0) 
            (/ u6 blocks-since-last) ;; Approximate blocks per hour calculation
            u0))
    )
    
    (if new-bid
        (map-set bidding-velocity-tracker
            {auction-id: auction-id}
            {
                bids-per-hour: hourly-rate,
                last-bid-block: stacks-block-height,
                velocity-trend: (if (> hourly-rate (get bids-per-hour current-velocity)) "accelerating" "decelerating"),
                predicted-final-price: (predict-final-price auction-id hourly-rate)
            }
        )
        true)
    
    (ok true))
)

(define-private (predict-final-price (auction-id uint) (velocity uint))
    (let (
        (base-prediction u1000) ;; Simplified prediction logic
        (velocity-multiplier (+ u100 (* velocity u10)))
    )
    (/ (* base-prediction velocity-multiplier) u100))
)

;; Market sentiment analysis
(define-public (update-market-sentiment)
    (let (
        (recent-success-rates (get-recent-success-rates))
        (price-trends (get-recent-price-trends))
        (overall-sentiment (calculate-sentiment recent-success-rates price-trends))
    )
    
    (var-set global-market-sentiment overall-sentiment)
    (ok overall-sentiment))
)

(define-private (get-recent-success-rates)
    u75 ;; Simplified - would aggregate from recent auctions
)

(define-private (get-recent-price-trends)
    u60 ;; Simplified - would analyze price movements
)

(define-private (calculate-sentiment (success-rate uint) (price-trend uint))
    (if (and (>= success-rate u80) (>= price-trend u70)) "bullish"
    (if (and (>= success-rate u60) (>= price-trend u50)) "neutral"
    (if (and (>= success-rate u40) (>= price-trend u30)) "bearish" "pessimistic")))
)

;; Read-only functions
(define-read-only (get-category-market-data (category (string-ascii 20)))
    (map-get? category-market-data {category: category})
)

(define-read-only (get-pricing-recommendation (seller principal) (auction-id uint))
    (map-get? pricing-recommendations {seller: seller, auction-id: auction-id})
)

(define-read-only (get-bidding-velocity (auction-id uint))
    (map-get? bidding-velocity-tracker {auction-id: auction-id})
)

(define-read-only (get-demand-indicators (category (string-ascii 20)))
    (map-get? demand-indicators {category: category})
)

(define-read-only (get-market-sentiment)
    (var-get global-market-sentiment)
)

(define-read-only (get-seller-performance (seller principal))
    (map-get? seller-performance {seller: seller})
)

(define-read-only (is-pricing-engine-enabled)
    (var-get pricing-engine-enabled)
)

;; Administrative functions
(define-public (toggle-pricing-engine)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized-access)
        (var-set pricing-engine-enabled (not (var-get pricing-engine-enabled)))
        (ok (var-get pricing-engine-enabled)))
)

(define-public (update-demand-indicators (category (string-ascii 20))
                                        (watchers uint)
                                        (bid-freq uint)
                                        (completion uint)
                                        (premium uint))
    (begin
        (map-set demand-indicators
            {category: category}
            {
                watchers-per-listing: watchers,
                bid-frequency: bid-freq,
                completion-rate: completion,
                premium-percentage: premium
            }
        )
        (ok true))
)



