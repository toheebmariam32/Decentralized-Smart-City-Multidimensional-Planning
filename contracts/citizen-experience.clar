;; Citizen Experience Contract
;; Enhances multidimensional citizen experiences

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u400))
(define-constant ERR_NOT_FOUND (err u401))
(define-constant ERR_INVALID_RATING (err u402))

;; Contract references
(define-constant PLANNING_VERIFICATION_CONTRACT .planning-entity-verification)

;; Citizen data
(define-map citizen-profiles
  { citizen-id: principal }
  {
    name: (string-ascii 100),
    preferences: (string-ascii 200),
    interaction-count: uint,
    satisfaction-score: uint,
    registered-at: uint
  }
)

(define-map service-experiences
  { experience-id: uint }
  {
    citizen-id: principal,
    dimension-id: uint,
    service-type: (string-ascii 100),
    rating: uint,
    feedback: (string-ascii 300),
    timestamp: uint,
    status: (string-ascii 20)
  }
)

(define-map dimension-satisfaction
  { dimension-id: uint }
  {
    total-ratings: uint,
    average-rating: uint,
    feedback-count: uint,
    last-updated: uint
  }
)

(define-map citizen-requests
  { request-id: uint }
  {
    citizen-id: principal,
    dimension-id: uint,
    request-type: (string-ascii 100),
    description: (string-ascii 500),
    priority: uint,
    status: (string-ascii 20),
    created-at: uint,
    resolved-at: (optional uint)
  }
)

(define-data-var next-experience-id uint u1)
(define-data-var next-request-id uint u1)

;; Register citizen profile
(define-public (register-citizen
  (name (string-ascii 100))
  (preferences (string-ascii 200))
)
  (map-set citizen-profiles
    { citizen-id: tx-sender }
    {
      name: name,
      preferences: preferences,
      interaction-count: u0,
      satisfaction-score: u0,
      registered-at: block-height
    }
  )
  (ok true)
)

;; Submit service experience
(define-public (submit-experience
  (dimension-id uint)
  (service-type (string-ascii 100))
  (rating uint)
  (feedback (string-ascii 300))
)
  (let ((experience-id (var-get next-experience-id)))
    (asserts! (and (>= rating u1) (<= rating u5)) ERR_INVALID_RATING)

    ;; Create experience record
    (try! (map-set service-experiences
      { experience-id: experience-id }
      {
        citizen-id: tx-sender,
        dimension-id: dimension-id,
        service-type: service-type,
        rating: rating,
        feedback: feedback,
        timestamp: block-height,
        status: "submitted"
      }
    ))

    ;; Update citizen interaction count
    (match (map-get? citizen-profiles { citizen-id: tx-sender })
      profile (map-set citizen-profiles
                { citizen-id: tx-sender }
                (merge profile { interaction-count: (+ (get interaction-count profile) u1) })
              )
      false
    )

    ;; Update dimension satisfaction
    (try! (update-dimension-satisfaction dimension-id rating))

    (var-set next-experience-id (+ experience-id u1))
    (ok experience-id)
  )
)

;; Submit citizen request
(define-public (submit-citizen-request
  (dimension-id uint)
  (request-type (string-ascii 100))
  (description (string-ascii 500))
  (priority uint)
)
  (let ((request-id (var-get next-request-id)))
    (try! (map-set citizen-requests
      { request-id: request-id }
      {
        citizen-id: tx-sender,
        dimension-id: dimension-id,
        request-type: request-type,
        description: description,
        priority: priority,
        status: "pending",
        created-at: block-height,
        resolved-at: none
      }
    ))
    (var-set next-request-id (+ request-id u1))
    (ok request-id)
  )
)

;; Resolve citizen request
(define-public (resolve-request (request-id uint) (entity-id uint))
  (let ((request (unwrap! (map-get? citizen-requests { request-id: request-id }) ERR_NOT_FOUND)))
    (asserts! (contract-call? PLANNING_VERIFICATION_CONTRACT is-entity-verified entity-id) ERR_UNAUTHORIZED)

    (map-set citizen-requests
      { request-id: request-id }
      (merge request {
        status: "resolved",
        resolved-at: (some block-height)
      })
    )
    (ok true)
  )
)

;; Update dimension satisfaction (private function)
(define-private (update-dimension-satisfaction (dimension-id uint) (new-rating uint))
  (match (map-get? dimension-satisfaction { dimension-id: dimension-id })
    current-data
      (let
        (
          (total-ratings (+ (get total-ratings current-data) u1))
          (current-sum (* (get average-rating current-data) (get total-ratings current-data)))
          (new-average (/ (+ current-sum new-rating) total-ratings))
        )
        (map-set dimension-satisfaction
          { dimension-id: dimension-id }
          {
            total-ratings: total-ratings,
            average-rating: new-average,
            feedback-count: (+ (get feedback-count current-data) u1),
            last-updated: block-height
          }
        )
        (ok true)
      )
    ;; First rating for this dimension
    (begin
      (map-set dimension-satisfaction
        { dimension-id: dimension-id }
        {
          total-ratings: u1,
          average-rating: new-rating,
          feedback-count: u1,
          last-updated: block-height
        }
      )
      (ok true)
    )
  )
)

;; Read-only functions
(define-read-only (get-citizen-profile (citizen-id principal))
  (map-get? citizen-profiles { citizen-id: citizen-id })
)

(define-read-only (get-experience (experience-id uint))
  (map-get? service-experiences { experience-id: experience-id })
)

(define-read-only (get-dimension-satisfaction-score (dimension-id uint))
  (map-get? dimension-satisfaction { dimension-id: dimension-id })
)

(define-read-only (get-citizen-request (request-id uint))
  (map-get? citizen-requests { request-id: request-id })
)

(define-read-only (calculate-citizen-satisfaction (citizen-id principal))
  (match (map-get? citizen-profiles { citizen-id: citizen-id })
    profile (get satisfaction-score profile)
    u0
  )
)
