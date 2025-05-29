;; Dimensional Modeling Contract
;; Manages multidimensional city models

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_MODEL_NOT_FOUND (err u201))
(define-constant ERR_INVALID_DIMENSION (err u202))

;; Data structures
(define-map city-models
  { model-id: uint }
  {
    name: (string-ascii 100),
    creator: principal,
    dimensions: (list 20 (string-ascii 50)),
    created-at: uint,
    last-updated: uint,
    status: (string-ascii 20)
  }
)

(define-map dimensional-data
  { model-id: uint, dimension: (string-ascii 50) }
  {
    data-hash: (buff 32),
    parameters: (list 10 uint),
    metadata: (string-ascii 500),
    updated-by: principal,
    updated-at: uint
  }
)

(define-map model-access
  { model-id: uint, user: principal }
  { can-read: bool, can-write: bool, can-admin: bool }
)

(define-data-var next-model-id uint u1)

;; Public functions
(define-public (create-model (name (string-ascii 100)) (dimensions (list 20 (string-ascii 50))))
  (let ((model-id (var-get next-model-id)))
    (map-set city-models
      { model-id: model-id }
      {
        name: name,
        creator: tx-sender,
        dimensions: dimensions,
        created-at: block-height,
        last-updated: block-height,
        status: "active"
      }
    )
    (map-set model-access
      { model-id: model-id, user: tx-sender }
      { can-read: true, can-write: true, can-admin: true }
    )
    (var-set next-model-id (+ model-id u1))
    (ok model-id)
  )
)

(define-public (update-dimensional-data (model-id uint) (dimension (string-ascii 50)) (data-hash (buff 32)) (parameters (list 10 uint)) (metadata (string-ascii 500)))
  (begin
    (asserts! (default-to false (get can-write (map-get? model-access { model-id: model-id, user: tx-sender }))) ERR_UNAUTHORIZED)
    (map-set dimensional-data
      { model-id: model-id, dimension: dimension }
      {
        data-hash: data-hash,
        parameters: parameters,
        metadata: metadata,
        updated-by: tx-sender,
        updated-at: block-height
      }
    )
    (match (map-get? city-models { model-id: model-id })
      model-data (begin
        (map-set city-models
          { model-id: model-id }
          (merge model-data { last-updated: block-height })
        )
        (ok true)
      )
      ERR_MODEL_NOT_FOUND
    )
  )
)

(define-public (grant-model-access (model-id uint) (user principal) (can-read bool) (can-write bool) (can-admin bool))
  (begin
    (asserts! (default-to false (get can-admin (map-get? model-access { model-id: model-id, user: tx-sender }))) ERR_UNAUTHORIZED)
    (map-set model-access
      { model-id: model-id, user: user }
      { can-read: can-read, can-write: can-write, can-admin: can-admin }
    )
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-model (model-id uint))
  (map-get? city-models { model-id: model-id })
)

(define-read-only (get-dimensional-data (model-id uint) (dimension (string-ascii 50)))
  (map-get? dimensional-data { model-id: model-id, dimension: dimension })
)

(define-read-only (get-model-access (model-id uint) (user principal))
  (map-get? model-access { model-id: model-id, user: user })
)
