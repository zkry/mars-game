;;; terraform.el --- Board game like Terraforming Mars -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/zkry/terraform.el
;; Keywords:


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;

;;; Code:


;;; Players

(cl-defstruct
    (tr-player
     (:constructor tr-player-create
                   (&key id
                         (money 0)
                         (money-production 1)
                         (steel 0)
                         (steel-production 1)
                         (titanium 0)
                         (titanium-production 1)
                         (plant 0)
                         (plant-production 1)
                         (energy 0)
                         (energy-production 1)
                         (heat 0)
                         (heat-production 1)
                         (rating 20)
                         (corp-card nil)
                         (hand nil)))
     (:copier nil))
  "represents a player's in-game statistics."
  id
  money    money-production
  steel    steel-production
  titanium titanium-production
  plant    plant-production
  energy   energy-production
  heat     heat-production
  rating
  corp-card
  hand
  played
  next-turn-effects)

(defun tr-player-modifications (player)
  (seq-map
   (lambda (effect)
     (nth 2 effect))
   (seq-filter
    (lambda (effect)
      (and effect (eql (car effect) 'add-modifier)))
    (append
     (seq-map
      #'tr-card-continuous-effect
      (tr-player-played player))
     (list (tr-corporation-continuous-effect (tr-player-corp-card player)))))))

(defun tr-get-player-resource-sell-amount (player resource)
  (pcase resource
    ('titanium 3)
    ('steel 2)))

(defun tr--player-can-sell-heat-p (&optional player)
  (seq-find
   (lambda (mod)
     (eql mod 'heat-as-money))
   (tr-player-modifications (or player tr-active-player))))

(defun tr-get-player-greenery-cost (&optional player)
  (unless player
    (setq player tr-active-player))
  (let* ((default-cost 8)
         (modifications (tr-player-modifications player)))
    (dolist (mod modifications)
      (pcase mod
        (`(set-greenery-cost ,amt)
         (setq default-cost amt))))
    default-cost))

(defun tr-add-projects-to-players-hand (player projects)
  (seq-do
   (lambda (elt)
     (unless (tr-card-p elt)
       (error "invalid card: %s" elt)))
   projects)
  (unless (tr-player-p player)
    (error "invalid player: %s" player))
  (setf (tr-player-hand player)
        (append projects (tr-player-hand player))))

(defun tr-player-by-id (id)
  (seq-find
   (lambda (player)
     (eql (tr-player-id player) id))
   (tr-game-state-players tr-game-state)))


;;; game board

;; a game tile can have the following properties:
;; permanent attributes (things on the board):
;;   :bonus - a list of bonus resource types (ex. steel, plant, card)
;;   :type  - if not land, the type of tyle this is (ex. ocean)
;;   :name  - the name of the tile, used for some cards effects (ex. ascraeus-mons)
;;
;; dynamic attributes (tiles placed on the board):
;;   :player       - the id of the player that owns the tile (ex. player1, player2).
;;   :top          - the structure built on top of the tile (ex. ocean, forest, city, capital, special).
;;   :special-type - the type of special tile placed.

(defvar tr-gameboard nil)

(defun tr--gameboard-tile-at (pt)
  "return the gameboard tile at pt."
  (gethash pt (tr-gameboard-board (tr-game-state-gameboard tr-game-state))))


(defun tr--valid-coordinate-p (coord)
  "Retunr non-nil if COORD is a valid gameboard coordinate."
  (seq-let (q r s) coord
    (<= (+ (abs q) (abs r) (abs s)) 8)))


(defun tr--gameboard-adjacent-tiles (pt)
  (seq-let (q r s) pt
    (let* ((adjacents `((,q ,(1+ r) ,(1- s))
                        (,q ,(1- r) ,(1+ s))
                        (,(1+ q) ,(1- r) ,s)
                        (,(1- q) ,(1+ r) ,s)
                        (,(1+ q) ,r ,(1- s))
                        (,(1- q) ,r ,(1+ s))))
           (valid-adjacents (seq-filter #'tr--valid-coordinate-p adjacents)))
      (seq-map #'tr--gameboard-tile-at valid-adjacents))))

(defun tr--tile-empty-ocean-p (tile)
  (and (not (plist-get tile :top))
       (eql (plist-get tile :type) 'ocean)))

(cl-defstruct (tr-gameboard (:constructor tr-gameboard-create)
                            (:copier nil))
  board
  extra-spaces)

(defun tr--in-board-p (pt)
  (seq-let (q r s) pt
    (<= (/ (+ (abs q) (abs r) (abs s)) 2) 4)))

(defun tr--board-coordinates ()
  "return a list of board coordinates from top-left to bottom right."
  (seq-filter
   #'tr--in-board-p
   (cl-loop for q downfrom 0 to -8
            for r upfrom -4  to 4
            append (cl-loop for s downfrom 4 to -4
                            for dq from 0 to 8
                            collect (list (+ q dq) r s)))))

(defconst tr--board-tharsis
  '(;; top row
    (:bonus (steel steel)) (:bonus (steel steel) :type ocean) () (:bonus (card) :type ocean) (:type ocean)

    ;; second row
    () (:bonus (steel) :name tharsis) () () () (:bonus (card card) :type ocean)

    ;; third row
    (:bonus (card) :name ascraeus) () () () () () (:bonus (steel))

    ;; fourth row
    (:bonus (plant titanium) :name pavonis) (:bonus (plant))
    (:bonus (plant)) (:bonus (plant)) (:bonus (plant plant))
    (:bonus (plant)) (:bonus (plant)) (:bonus (plant plant) :type ocean)

    ;; equator
    (:bonus (plant plant) :name arsia) (:bonus (plant plant))
    (:bonus (plant plant) :name noctis) (:bonus (plant plant) :type ocean)
    (:bonus (plant plant) :type ocean) (:bonus (plant plant) :type ocean)
    (:bonus (plant plant)) (:bonus (plant plant)) (:bonus (plant plant))

    ;; sixth rown
    (:bonus (plant)) (:bonus (plant plant)) (:bonus (plant))
    (:bonus (plant)) (:bonus (plant)) (:bonus (plant) :type ocean)
    (:bonus (plant) :type ocean) (:bonus (plant) :type ocean)

    ;; seventh row
    () () () () () (:bonus (plant)) ()

    ;; eighth row
    (:bonus (steel steel)) () (:bonus (card)) (:bonus (card)) () (:bonus (titanium))

    ;; bottom row
    (:bonus (steel)) (:bonus (steel steel)) () () (:bonus (steel steel) :type ocean))
  "data for the tharsis game board.")

(defun tr--initial-board-tharsis ()
  "return initialized game board."
  (let* ((main-board (make-hash-table :test 'equal)))
    (seq-mapn
     (lambda (coords data)
       (puthash coords data main-board))
     (tr--board-coordinates)
     tr--board-tharsis)
    (tr-gameboard-create
     :board main-board
     :extra-spaces (make-hash-table :test 'equal))))

(defun tr--find-named-tile (name)
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (locations (hash-table-keys board)))
    (seq-find
     (lambda (location)
       (let* ((tile (gethash location board)))
         (eql (plist-get tile :name) name)))
     locations)))

(defun tr-game-board-count-greeneries ()
  (seq-count
   (lambda (coord)
     (eql (plist-get (tr--gameboard-tile-at coord) :top) 'greenery))
   (tr--board-coordinates)))

(defun tr-adjacent-players (coord)
  "Return a list of adjacent player IDs to COORD."
  (let* ((adjacent-tiles (tr--gameboard-adjacent-tiles coord)))
    (seq-filter
     #'identity
     (seq-map
      (lambda (tile)
        (plist-get tile :player))
      adjacent-tiles))))


;;; Cards

(cl-defstruct
    (tr-card
     (:constructor tr-card-create)
     (:copier nil))
  number
  name
  cost
  tags
  victory-points
  requirements
  tile
  type
  action
  continuous-effect
  effect
  resource-count
  used
  accepts)

(defconst tr--standard-projects
  `(,(tr-card-create
      :number 'power-plant
      :name "Power Plant"
      :type 'event
      :cost 11
      :effect '[(inc energy-production)])
    ,(tr-card-create
      :number 'asteroid
      :name "Asteroid"
      :type 'event
      :cost 14
      :effect [(inc-tempurature)])
    ,(tr-card-create
      :number 'aquifer
      :name "Aquifer"
      :type 'event
      :cost 18
      :effect [(add-ocean)])
    ,(tr-card-create
      :name "Greenery"
      :number 'greenery
      :type 'event
      :cost 23
      :effect [(add-greenery)])
    ,(tr-card-create
      :name "City"
      :number 'city
      :type 'event
      :cost 25
      :effect [(add-city)])))

(defun tr-card-by-id (id)
  "Return the card of the given ID."
  (seq-find (lambda (card)
              (= (tr-card-number card) id))
            (tr-game-state-all-cards tr-game-state)))

(defun tr-played-continuous-effects (effect-name)
  (seq-filter
   (lambda (effect)
     (equal (car effect) effect-name))
   (seq-map
    (lambda (card)
      (tr-card-continuous-effect card))
    (tr-player-played tr-active-player))))

(defun tr-card-effective-cost (card)
  "Return the cost of a card after applying discounts."
  (if (not tr-active-player)
      (tr-card-cost card)
    (let* ((base-cost (tr-card-cost card)))
      (seq-do
       (lambda (effect)
         (pcase effect
           (`(card-discount ,tag ,amt)
            (when (or (eql tag '_)
                      (member tag (tr-card-tags card)))
              (cl-decf base-cost amt)))))
       (seq-map
        (lambda (card)
          (tr-card-continuous-effect card))
        (tr-player-played tr-active-player)))
      (when (< base-cost 0)
        (setq base-cost 0))
      base-cost)))

(cl-defstruct (tr-corporation
               (:constructor tr-corporation-create)
               (:copier nil))
  number
  name
  tags
  effect
  continuous-effect
  action
  first-move)

(defun tr--extract-action (effect)
  "Return extra parameter required from a specific action."
  (let* ((effect-id (car effect))
         (effect-params (cdr effect))
         (registered-effect (tr-card-effect-by-id effect-id))
         (extra-action-func (tr-card-extra-action registered-effect))
         (immediate-action-func (tr-card-immediate-action registered-effect)))
    (cond
     (immediate-action-func
      (let ((value (apply immediate-action-func effect-params)))
        (cons 'value value)))
     (extra-action-func
      (when extra-action-func
      (apply extra-action-func effect-params))))))

(defun tr--extract-card-actions (card)
  (let* ((effects (tr-card-effect card)))
    (seq-filter #'identity (seq-map #'tr--extract-action effects))))

(defun tr--player-satisfies-action-requirement (player action)
  "Return non-nil if PLAYER satisfies the requirement section of ACTION."
  (let ((tr-active-player player))
    (pcase action
      (`(-> nil ,_effect) t)
      (`(-> (dec ,resource ,amt) ,_effect)
       (>= (or (tr-get-requirement-count resource) 0) amt))
      (`(-> (buy ,resource ,amt) ,_effect)
       (>= (+ (tr-get-requirement-count 'money)
              (* (tr-get-player-resource-sell-amount tr-active-player resource)
                 (tr-get-requirement-count resource))))))))

(defun tr-count-player-tag (player tag)
  (let* ((played (tr-player-played player)))
    (apply #'+
           (seq-map
            (lambda (project)
              (seq-count
               (lambda (proj-tag)
                 (eql proj-tag tag))
               (tr-card-tags project)))
            played))))

(defun tr-count-player-tags (player tags)
  (apply #'+ (seq-map (lambda (tag) (tr-count-player-tag player tag)) tags)))

(defun tr-card-accepts-resource-p (card resource)
  "Return non-nil if CARD can take on resource of type RESOURCE."
  (and (eql (tr-card-type card) 'active)
       (let* ((action (tr-card-action card))
              (continuous-effect (tr-card-continuous-effect card))))))



;;; Generic Functions

(defun tr-effect-to-string (clause)
  "Convert a card effect data description to a string."
  (if (not clause)
      ""
    (let* ((effect-id (car clause))
           (effect-params (cdr clause))
           (effect-entry (terraform-card-effect-by-id effect-id))
           (lighter-func (terraform-card-effect-lighter effect-entry)))
      (unless effect-entry
        (error "Unknown effect %s in %s" effect-id clause))
      (apply lighter-func effect-params))))

(defun tr-requirements-to-string (requirements)
  (if (not requirements)
      ""
    (pcase requirements
      (`(own-forest ,amt)
       (format "🌳>%d" amt))
      (`(and ,cases)
       (string-join
        (seq-map #'tr-requirements-to-string cases)))
      (`(,cmp ,left ,right)
       (let* ((left-str
               (pcase left
                 ('ocean "🌊")
                 ('titanium-production
                  (tr--char->prod tr--titanium-char))
                 ('steel-production
                  (tr--char->prod tr--steel-char))
                 ('oxygen "O₂")
                 ('tempurature "°C")
                 ('science-tag tr--science-tag)
                 ('greenery tr--greenery-indicator)
                 ('city tr--city-tag)
                 (`(tags ,tags)
                  (string-join (seq-map #'tr-tag-to-string tags)))
                 (_ (error "undefined left %s" left))))
              (cmp-str
               (pcase cmp
                 ('>= "≥")
                 ('> ">")
                 ('<= "≤")
                 ('< "<")
                 (_ (error "undefined comparator %s" cmp)))))
         (format "%s%s%d"
                 left-str
                 cmp-str
                 right)))
      )))

(defun tr-tag-to-string (tag)
  (pcase tag
    ('city tr--city-tag)
    ('microbe tr--microbe-tag)
    ('building tr--building-tag)
    ('space tr--space-tag)
    ('science tr--science-tag)
    ('power tr--power-tag)
    ('earth tr--earth-tag)
    ('jovian tr--jovian-tag)
    ('venus tr--venus-tag)
    ('plant tr--plant-tag)
    ('animal tr--animal-tag)
    ('wild tr--wild-tag)
    ('event tr--event-tag)))

(defun tr-effects-to-string (effect)
  "Convert a card effect data description to a string."
  (string-join
   (seq-map
    (lambda (clause)
      (tr-effect-to-string clause))
    effect)
   "; "))

(defun tr-continuous-effect-trigger-to-string (trigger-form)
  (pcase trigger-form
    ('event
     tr--event-tag)
    ('any-ocean
     (tr--char->decrease-any tr--ocean-indicator))
    ('ocean
     tr--ocean-indicator)
    ('city
     (tr--char->decrease-any tr--city-tag))
    ('any-city
     (tr--char->decrease-any tr--city-tag))
    (`(tags ,tags-list)
     (string-join (seq-map #'tr-tag-to-string tags-list) "|"))
    (`(spend ,amt)
     (format "%d%s" amt tr--money-char))
    (`(and ,cases)
     (string-join (seq-map #'tr-continuous-effect-trigger-to-string cases) "&"))))


(defun tr-continuous-effect-to-string (effect)
  "Convert a card's continuous effect to a string."
  (if effect
      (pcase effect
        (`(add-modifier ,label ,_mod) label)
        (`(resource-enrich ,resource ,amt)
         (format "%s worth +%d"
                 (tr-resource-type-to-string resource)
                 amt))
        (`(card-discount ,tag ,amt)
         (format "%s:-%d%s"
                 (if tag
                     (terraform-tag-to-string tag)
                   "")
                 amt
                 terraform--money-char))
        (`(on ,event ,effect)
         (format "%s:%s" (tr-continuous-effect-trigger-to-string event) (string-join (seq-map #'tr-effect-to-string effect) "; ")))
        (`(_??? ,amt)
         (format "± %d 🌊|°C|O₂ req." amt))
        (`(standard-project-discount ,amt)
         (format "-%d%s standard proj." amt tr--money-char)))
    "?C?"))

(cl-defgeneric tr-line-string (item)
  (:documentation "Display item on a single line."))

(cl-defmethod tr-line-string ((item tr-card))
  (let* ((card-type (tr-card-type item))
         (card-face (pcase card-type
                      ('active 'tr-active-face)
                      ('automated 'tr-automated-face)
                      ('event 'tr-event-face)))
         (card-name (tr-card-propertized-name item))
         (base (format "$%2d %5s %s %s %s %s "
                       (tr-card-effective-cost item)
                       (tr-requirements-to-string (tr-card-requirements item))
                       card-name
                       (tr-effects-to-string (tr-card-effect item))
                       (tr-effects-to-string (tr-card-action item))
                       (tr-continuous-effect-to-string (tr-card-continuous-effect item))))
         (tags (string-join (seq-map #'tr-tag-to-string (tr-card-tags item)))))
    (format "%-60s %s" base tags)))

(cl-defmethod tr-line-string ((item tr-corporation))
  (format "%s %s %s"
          (tr-corporation-name item)
          (tr-effects-to-string (tr-corporation-effect item))
          (tr-continuous-effect-to-string (tr-corporation-continuous-effect item))))

(defun tr-card-propertized-name (card)
  (let* ((card-type (tr-card-type card))
         (card-face (pcase card-type
                      ('active 'tr-active-face)
                      ('automated 'tr-automated-face)
                      ('event 'tr-event-face))))
    (propertize (tr-card-name card) 'font-lock-face card-face)))

(defun tr-card-short-line-string (card)
  "Return shorter string for CARD used for displaying after played."
  (let* ((card-name (tr-card-propertized-name card)))
    (format "%s %s %s %s"
            card-name
            (or (and (tr-card-resource-count card)
                     (format "%d" (tr-card-resource-count card))) "")
            (tr-effects-to-string (tr-card-action card))
            (tr-continuous-effect-to-string (tr-card-continuous-effect card)))))

(cl-defmethod tr-line-string ((item cons))
  (cdr item))



;;; Game State
;; The game state is stored in the tr-game-state struct which contains
;; all of the information about the current state of the game.  The
;; global parameters are on a scale of 0 to max, including the
;; tempurature.

(require 'terraform-card)

(defvar tr-game-state nil)

(defconst tr--game-state-max-tempurature 14)
(defconst tr--game-state-max-oxygen 19)
(defconst tr--game-state-max-ocean 9)

(defun tr-oxygen-to-ct (oxygen)
  (/ (+ oxygen 30) 2))

(defun tr-ct-to-tempurature (ct)
  "Convert tempurature index CT to tempurature in degrees celcius."
  (- (* ct 2) 30))

(defun tr-randomize (elts &optional from)
  "Shuffle list of ELTS from FROM to the end recursively. "
  (when (listp elts)
    (setq elts (seq-into elts 'vector)))
  (unless from
    (setq from 0))
  (when (< from (length elts))
    (let* ((random-idx (+ (random (- (length elts) from)) from))
           (swap-elt (aref elts random-idx))
           (from-elt (aref elts from)))
      (aset elts from swap-elt)
      (aset elts random-idx from-elt)
      (terraform-randomize elts (1+ from))))
  (seq-into elts 'list))

(defun terraform-card-generate-deck ()
  (let* ((ids (tr-randomize (hash-table-keys terraform-card-directory))))
    (seq-map
     (lambda (id)
       (apply #'terraform-card-create (gethash id terraform-card-directory)))
     ids)))

(defun terraform-card-generate-corporation-deck ()
  (let* ((ids (tr-randomize (hash-table-keys terraform-card-corporation-directory))))
    (seq-map
     (lambda (id)
       (apply #'terraform-corporation-create (gethash id terraform-card-corporation-directory)))
     ids)))

(cl-defstruct
    (tr-game-state
     (:constructor tr-game-state-create)
     (:copier nil))
  all-cards
  gameboard
  players
  deck
  corporation-deck
  param-ocean
  param-tempurature
  param-oxygen
  generation
  passed-players
  state
  exodeck ;; cards taken out of deck
  )

(defun tr--new-game-state (player-ct)
  (let ((initial-deck (tr-card-generate-deck)))
    (tr-game-state-create
     :gameboard (tr--initial-board-tharsis)
     :players (seq-map (lambda (player-no)
                         (tr-player-create
                          :id (intern (format "player%d" player-no))))
                       (number-sequence 1 player-ct))
     :param-ocean 0
     :param-tempurature 0
     :param-oxygen 0
     :generation 1
     :passed-players '()
     :all-cards initial-deck
     :deck initial-deck
     :state 'start
     :corporation-deck (tr-card-generate-corporation-deck)
     :exodeck '())))

(defun tr--game-state-player-ct ()
  (length (tr-game-state-players tr-game-state)))

;;
(setq tr-game-state (tr--new-game-state 1))
;; (tr-game-state-deck (tr--new-game-state 1))

(defun tr--production-resource-p (resource)
  (string-suffix-p "-production" (symbol-name resource)))

(defun tr--get-other-options (resource-type &optional amt)
  "Return all users with RESOURCE-TYPE."
  (let* ((players (tr-game-state-players tr-game-state))
         (card-resource-p (memq resource-type '(microbe animal)))
         (production-resource-p (tr--production-resource-p resource-type))
         (ress))
    (dolist (player players)
      (if card-resource-p
          (let ((played-projects (tr-player-played player)))
            (dolist (proj played-projects)
              (when (memq resource-type (tr-card-tags proj))
                (let* ((ct (tr-card-resource-count proj)))
                  (when (> ct 0)
                    (push (list player proj ct) ress))))))
        (let* ((tr-active-player player)
               (ct (tr-get-requirement-count resource-type)))
          (when (> ct 0)
            (push (list player ct) ress)))))
    (if amt
        (seq-filter
         (lambda (elt)
           (or (not production-resource-p)
               (>= (car (last elt)) amt)))
         ress)
      ress)))


;;; Board

(defface tr-player1-face
  '((t :background "#FF0000"))
  "Face for player 1's token."
  :group 'terraform)

(defface tr-player2-face
  '((t :background "#00FF00"))
  "Face for player 2's token."
  :group 'terraform)

(defface tr-player3-face
  '((t :background "#0000FF"))
  "Face for player 3's token."
  :group 'terraform)

(defface tr-player4-face
  '((t :background "#AAAAAA"))
  "Face for player 4's token."
  :group 'terraform)

;; terrain
(defface tr-land-face
  '((t :foreground "#DA9952"))
  "Face for land tiles"
  :group 'terraform)

(defface tr-ocean-face
  '((t :foreground "#2F539B"))
  "Face for ocean tiles"
  :group 'terraform)

(defface tr-greenery-face
  '((t :foreground "#719B11"))
  "Face for greenery tiles."
  :group 'terraform)

(defface tr-city-face
  '((t :foreground "gray"))
  "Face for city tiles"
  :group 'terraform)

;; resources
(defface tr-money-face
  '((t :foreground "#060500"
       :background "#F7E11C"))
  "Face for money face."
  :group 'terraform)

(defface tr-steel-face
  '((t :foreground "#640000"
       :background "#BD7135"))
  "Face for steel resource."
  :group 'terraform)

(defface tr-titanium-face
  '((t :foreground "#FBFB85"
       :background "#414141"))
  "Face for titanium resource."
  :group 'terraform)

(defface tr-plant-face
  '((t :foreground "#174704"
       :background "#12DF12"))
  "Face for plant resource."
  :group 'terraform)

(defface tr-energy-face
  '((t :foreground "#FEFEFE"
       :background "#860AC6"))
  "Face for energy resource."
  :group 'terraform)

(defface tr-heat-face
  '((t :foreground "#FBED21"
       :background "#F23900"))
  "Face for heat resource."
  :group 'terraform)

;; Project faces

(defface tr-event-face
  '((t :background "#D98A5A"
       :foreground "#000000"))
  "Face for event-type projects."
  :group 'terraform)

(defface tr-automated-face
  '((t :background "#57A257"
       :foreground "#000000"))
  "Face for automated-type projects."
  :group 'terraform)

(defface tr-active-face
  '((t :background "#5D9DD3"
       :foreground "#000000"))
  "Face for automated-type projects."
  :group 'terraform)

;; TODO: figure out how to add box faces
;; :box (:line-width (3 . 3)
;;             :color "brown")

(defconst tr--player1-token (propertize " " 'font-lock-face 'tr-player1-face))
(defconst tr--player2-token (propertize " " 'font-lock-face 'tr-player2-face))
(defconst tr--player4-token (propertize " " 'font-lock-face 'tr-player4-face))
(defconst tr--player3-token (propertize " " 'font-lock-face 'tr-player3-face))

(defconst tr--tiles
  '((ocean . (:border "~" :face tr-ocean-face))
    (ocean-filled . (:border "~"
                             :face tr-ocean-face
                             :top-line ".~.~."
                             :bottom-line ".~.~."))
    (greenery . (:border "♠"
                         :face tr-greenery-face
                         :top-line '(" ♣" owner "♣")
                         :bottom-line '(" ♣ ♣ ")))
    (city . (:border '(" ### " "-" "-" "-" "-" "-" "-" "-")
                     :face tr-city-face
                     :top-line '(" #" owner "# ")
                     :bottom-line " ### "))))

(defun tr--format-edge (pt1 pt2 edge-char)
  "Return the propertized EDGE-CHAR of grid between PT1 and PT2."
  (if (not (or (tr--in-board-p pt1)
               (tr--in-board-p pt2)))
      " "
    (let* ((tile1 (tr--gameboard-tile-at pt1))
           (tile2 (tr--gameboard-tile-at pt2))
           (tile1-type (plist-get tile1 :type))
           (tile2-type (plist-get tile2 :type)))
      edge-char)))

(defun tr--resource-character (resource-type)
  (pcase resource-type
    ('steel tr--steel-char)
    ('titanium tr--titanium-char)
    ('plant tr--plant-char)
    ('card tr--card-char)))

(defun tr--board-player-char (player)
  "Return the character token character for PLAYER."
  (pcase player
    ('player1 tr--player1-token)
    ('player2 tr--player2-token)
    ('player3 tr--player3-token)
    ('player4 tr--player4-token)))

(defun tr--board-format-bonus (bonus)
  (seq-let (bonus1 bonus2) bonus
    (cond
     ((not (or bonus1 bonus2)) "     ")
     ((and bonus1 bonus2) (concat " "
                                  (tr--resource-character bonus1)
                                  " "
                                  (tr--resource-character bonus2)
                                  " "))
     (bonus1 (concat "  "
                     (tr--resource-character bonus1)
                     "  ") ))))

(defun tr--board-line-top-ocean (_tile line-no)
  (nth line-no
       (list (propertize "~ ~ ~" 'font-lock-face 'tr-ocean-face)
             (propertize "~.~.~.~" 'font-lock-face 'tr-ocean-face)
             (propertize "~.~.~.~" 'font-lock-face 'tr-ocean-face)
             (format "%s_%s_%s"
                     (propertize "~" 'font-lock-face 'tr-ocean-face)
                     (propertize "~" 'font-lock-face 'tr-ocean-face)
                     (propertize "~" 'font-lock-face 'tr-ocean-face)))))

(defun tr--board-line-top-greenery (tile line-no)
  (let ((player (plist-get tile :player)))
    (nth line-no
         (list (propertize "♠ ♠ ♠" 'font-lock-face 'tr-greenery-face)
               (concat
                (propertize "♠ ♣" 'font-lock-face 'tr-greenery-face)
                (tr--board-player-char player)
                (propertize "♣ ♠" 'font-lock-face 'tr-greenery-face))
               (concat
                (propertize "♠ ♣" 'font-lock-face 'tr-greenery-face)
                (tr--board-player-char player)
                (propertize "♣ ♠" 'font-lock-face 'tr-greenery-face))
               (concat
                (propertize "♠" 'font-lock-face 'tr-greenery-face)
                "_"
                (propertize "♠" 'font-lock-face 'tr-greenery-face)
                "_"
                (propertize "♠" 'font-lock-face 'tr-greenery-face))))))

(defun tr--board-line-top-city (tile line-no)
  (let ((player (plist-get tile :player)))
    (nth line-no
         (list (propertize " ### " 'font-lock-face 'tr-city-face)
               (concat
                (propertize "^##" 'font-lock-face 'tr-city-face)
                (tr--board-player-char player)
                (propertize "##^" 'font-lock-face 'tr-city-face))
               (concat
                (propertize "^##" 'font-lock-face 'tr-city-face)
                (tr--board-player-char player)
                (propertize "##^" 'font-lock-face 'tr-city-face))
               (concat
                (propertize "-" 'font-lock-face 'tr-city-face)
                "_"
                (propertize "-" 'font-lock-face 'tr-city-face)
                "_"
                (propertize "-" 'font-lock-face 'tr-city-face))))))

(defun tr--board-line-top-special (tile line-no)
  (let* ((special-names '((lava-flows . "LAVA")
                          (industrial-center . "INDST")
                          (nuclear-zone . "NUCLR")
                          (commercial-district . "COMMR")
                          (restricted-area . "RESTR")
                          (mohole-area . "MOHOL")
                          (natural-preserve . "NATRL")
                          (ecological-zone . "ECOLO")
                          (mining-rights . "MINE")
                          (mining-area . "MINE")))
         (player (plist-get tile :player))
         (special-type (plist-get tile :special-type))
         (special-label (alist-get special-type special-names)))
    (nth line-no
         (list (format "%5s" special-label)
               (concat "   " (or (tr--board-player-char player) " ") "   ")
               (concat "   " (or (tr--board-player-char player) " ") "   ")
               " _ _ "))))

(defun tr--board-line-ocean (tile line-no)
  (let ((bonus (plist-get tile :bonus)))
    (nth line-no
         (list (propertize "~ ~ ~" 'font-lock-face 'tr-ocean-face)
               (concat (propertize "~" 'font-lock-face 'tr-ocean-face)
                       (tr--board-format-bonus bonus)
                       (propertize "~" 'font-lock-face 'tr-ocean-face))
               (concat (propertize "~" 'font-lock-face 'tr-ocean-face)
                       "     "
                       (propertize "~" 'font-lock-face 'tr-ocean-face))
               (concat (propertize "~" 'font-lock-face 'tr-ocean-face)
                       "_"
                       (propertize "~" 'font-lock-face 'tr-ocean-face)
                       "_"
                       (propertize "~" 'font-lock-face 'tr-ocean-face))))))



(defun tr--board-line-name (tile line-no)
  (let* ((display-names '((pavonis .  "Pavonis")
                          (ascraeus . "Ascraus")
                          (tharsis .  "Tharsis")
                          (arsia . "Arsia")
                          (noctis . "Noctis")))
         (bonus (plist-get tile :bonus))
         (name (plist-get tile :name))
         (display-name (alist-get name display-names)))
    (nth line-no
         (list (propertize "     " 'font-lock-face 'tr-ocean-face)
               (concat (propertize " " 'font-lock-face 'tr-ocean-face)
                       (tr--board-format-bonus bonus)
                       (propertize " " 'font-lock-face 'tr-ocean-face))
               (format "%7s" (or display-name ""))
               (concat " _ _ ")))))

(defun tr--highlight-tile (str)
  (dotimes (i (length str))
    (let ((char-at (aref str i)))
      (when (eql char-at ?\s)
        (aset str i ?v)
        (put-text-property i (1+ i) 'face 'alert-moderate-face str))))
  str)

(defun tr--board-adjacent-city-p (pt)
  (seq-find
   (lambda (top)
     (eql top 'city))
   (seq-map
    (lambda (tile)
      (plist-get tile :top))
    (tr--gameboard-adjacent-tiles pt))))

(defun tr--board-adjacent-city-ct (pt)
  (seq-count
   (lambda (top)
     (eql top 'city))
   (seq-map
    (lambda (tile)
      (plist-get tile :top))
    (tr--gameboard-adjacent-tiles pt))))

(defun tr--board-adjacent-to-own (pt)
  (seq-find
   (lambda (player)
     (eql player (tr-player-id tr-active-player)))
   (seq-map
    (lambda (tile)
      (plist-get tile :player))
    (tr--gameboard-adjacent-tiles pt))))

(defun tr--board-line (pt line-no)
  (let* ((line-length (if (memq line-no '(0 3)) 5 7))
         (content (if (= line-no 3)
                      (seq-let (q r s) pt
                        (let ((bottom-pt (list q (1+ r) (1- s))))
                          (if (or (tr--in-board-p pt)
                                  (tr--in-board-p bottom-pt))
                              (copy-sequence " _ _ ")
                            "     ")))
                    (make-string line-length ?\s)))
         (already-taken-p
          (lambda (pt)
            (member pt tr-current-args))))
    (if (not (tr--in-board-p pt))
        content
      (let* ((at-tile (tr--gameboard-tile-at pt))
             (type (plist-get at-tile :type))
             (top (plist-get at-tile :top))
             (name (plist-get at-tile :name))
             (bonus (plist-get at-tile :bonus)))
        (cond
         ((eql top 'ocean)
          (tr--board-line-top-ocean at-tile line-no))
         ((eql top 'greenery)
          (tr--board-line-top-greenery at-tile line-no))
         ((eql top 'city)
          (tr--board-line-top-city at-tile line-no))
         ((eql top 'special)
          (tr--board-line-top-special at-tile line-no))
         ((eql type 'ocean) ;; EMPTY OCEAN
          (if (and (eql (tr-current-pending-arg) 'empty-ocean)
                   (not (funcall already-taken-p pt)))
              (tr--highlight-tile
               (tr--board-line-ocean at-tile line-no))
            (tr--board-line-ocean at-tile line-no)))
         ((eql name 'noctis) ;; TODO : generalized reserved tiles.
          (tr--board-line-name at-tile line-no))
         (t ;; EMPTY LAND
          (let* ((line (tr--board-line-name at-tile line-no)))
            (if (not (tr--valid-coordinate-p pt))
                line
              (cond
               ((funcall already-taken-p pt)
                line)
               ((eql (tr-current-pending-arg) 'empty-land)
                (tr--highlight-tile line))
               ((and (eql (tr-current-pending-arg) 'mining-bonus)
                     (or (member 'steel bonus)
                         (member 'titanium bonus)))
                (tr--highlight-tile line))
               ((and (eql (tr-current-pending-arg) 'standard-city-placement)
                     (not (tr--board-adjacent-city-p pt)))
                (tr--highlight-tile line))
               ((and (eql (tr-current-pending-arg) 'standard-greenery-placement)
                     (tr--board-adjacent-to-own pt))
                (tr--highlight-tile line))
               ((and (eql (tr-current-pending-arg) 'city-placement-adj-2)
                     (>= (tr--board-adjacent-city-ct pt) 2))
                (tr--highlight-tile line))
               (t line))))))))))

(defun tr--display-board ()
  (let ((parts '((" "
                  (lambda (a b c atl abl)
                    (insert
                     (tr--format-edge atl a "/")
                     (propertize (tr--board-line a 0) 'tr-tile a)
                     (tr--format-edge a b "\\")
                     (propertize (tr--board-line b 2) 'tr-tile b))))
                 (""
                  (lambda (a b c atl abl)
                    (insert
                     (tr--format-edge atl a "/")
                     (propertize (tr--board-line a 1) 'tr-tile a)
                     (tr--format-edge a b "\\")
                     (propertize (tr--board-line b 3) 'tr-tile b))))
                 (""
                  (lambda (a b c atl abl)
                    (insert
                     (tr--format-edge abl a "\\")
                     (propertize (tr--board-line a 2) 'tr-tile a)
                     (tr--format-edge a c "/")
                     (propertize (tr--board-line c 0) 'tr-tile c))))
                 (" "
                  (lambda (a b c atl abl)
                    (insert
                     (tr--format-edge abl a "\\")
                     (propertize (tr--board-line a 3) 'tr-tile a)
                     (tr--format-edge a c "/")
                     (propertize (tr--board-line c 1) 'tr-tile c)))))))
    (dotimes (row 10)
      (let ((r (- row 3))
            (q -4)
            (s (- 7 row)))
        (dolist (part parts)
          (seq-let (init tile) part
            (insert init)
            (dotimes (x 5)
              ;; KEY:
              ;;atl     b
              ;;   /a \__
              ;;abl\__/ c
              (let* ((q* (+ q (* x 2)))
                     (r* (- r x))
                     (s* (- s x))
                     (a (list q* r* s*))
                     (b (list (1+ q*) (1- r*) s*))
                     (c (list (1+ q*) r* (1- s*)))
                     (atl (list (1- q*) r* (1+ s*)))
                     (abl (list (1- q*) (1+ r*) s*)))
                (funcall tile a b c atl abl)))
            (insert "\n")))))))


;;; Selection Engine
(defvar-local tr-current-selection nil)
(defvar-local tr-selection-spec nil)

(defun tr--process-selection (idx item)
  (let* ((slot-spec (cdr (aref (plist-get tr-selection-spec :items) idx)))
         (slot-type (plist-get slot-spec :type)))
    (pcase slot-type
      ('one
       (setf (nth idx tr-current-selection)
             item))
      ('multiple
       (if (not (member item (nth idx tr-current-selection)))
           (setf (nth idx tr-current-selection)
                 (cons item (nth idx tr-current-selection)))
         (setf (nth idx tr-current-selection)
               (remove item (nth idx tr-current-selection))))))))

(defun tr--selection-items (field-idx items)
  (let* ((type (car items)))
    (pcase type
      ('selection
       (let* ((props (cdr items))
              (title (plist-get props :title))
              (items (plist-get props :items))
              (type (plist-get props :type)))
         (insert title "\n")
         (dolist (item items)
           (pcase type
             ('one
              (insert " - "
                      (buttonize (format
                                  "(%s)"
                                  (if (equal item (nth field-idx tr-current-selection))
                                      "x"
                                    " "))
                                 (pcase-lambda (`(,idx ,item))
                                   (tr--process-selection idx item)
                                   (tr-display-board)) ;; Should display board call be here?
                                 (list field-idx item))
                      (tr-line-string item) "\n"))
             ('multiple
              (insert " - "
                      (buttonize (format
                                  "[%s]"
                                  (if (member item (nth field-idx tr-current-selection))
                                      "x"
                                    " "))
                                 (pcase-lambda (`(,idx ,item))
                                   (tr--process-selection idx item)
                                   (terraform-display-board))
                                 (list field-idx item))
                      (tr-line-string item) "\n")))))))))

(cl-defun tr-selection (&key title items validation on-confirm)
  (when (not tr-selection-spec)
    (setq tr-selection-spec
          (list :items items
                :validation validation
                :on-confirm on-confirm))
    (setq tr-current-selection (make-list (length items) nil)))
  (insert title "\n")
  (let ((i 0))
    (seq-do (lambda (item)
              (tr--selection-items i item)
              (cl-incf i))
            items))
  (pcase-let* ((passing-types '(warn info))
               (`(,type ,msg) (apply validation tr-current-selection)))
    (pcase type
      ('quiet-error nil)
      ('error (setq msg (propertize msg 'font-lock-face 'error)))
      ('warn (setq msg (propertize msg 'font-lock-face 'warning)))
      ('info nil))
    (insert "\n" (or msg "ERROR") "\n")
    (when (memq type passing-types)
      (insert (buttonize "[confirm selection]"
                         (lambda (_)
                           (apply on-confirm tr-current-selection)
                           (setq tr-selection-spec nil
                                 tr-current-selection nil)))))))



;;; Display
;; This page deals with composing the entire page.

(defun tr--display-parameters ()
  "Display game parameters."
  (insert (format "Generation: %d   Ocean:%d/9   Temp:%s/8   O₂:%s/14\n"
                  (tr-game-state-generation tr-game-state)
                  (tr-game-state-param-ocean tr-game-state)
                  (tr-ct-to-tempurature (tr-game-state-param-tempurature tr-game-state))
                  (tr-game-state-param-oxygen tr-game-state))))

(defun tr--display-player-panel ()
  "Display the player stats summary panels."
  (let ((panel-lines
         '((lambda (_player)
             "+------------+")
           (lambda (player)
             (let ((id (tr-player-id player)))
               (format "|%2s%10s|"
                       (propertize "  "
                                   'font-lock-face
                                   (pcase id
                                     ('player1 'tr-player1-face)
                                     ('player2 'tr-player2-face)
                                     ('player3 'tr-player3-face)
                                     ('player4 'tr-player4-face)))
                       (or (if (tr-player-corp-card player)
                               (truncate-string-to-width (tr-corporation-name (tr-player-corp-card player)) 9)
                             "???")))))
           (lambda (_player)
             "+------------+")
           (lambda (player)
             (format "| TR %7d |" (tr-player-rating player)))
           (lambda (player)
             (format "| %s [%2d]%4d |" tr--money-char
                     (tr-player-money-production player)
                     (tr-player-money player)))
           (lambda (player)
             (format "| %s [%2d]%4d |" tr--steel-char
                     (tr-player-steel-production player)
                     (tr-player-steel player)))
           (lambda (player)
             (format "| %s [%2d]%4d |" tr--titanium-char
                     (tr-player-titanium-production player)
                     (tr-player-titanium player)))
           (lambda (player)
             (format "| %s [%2d]%4d |" tr--plant-char
                     (tr-player-plant-production player)
                     (tr-player-plant player)))
           (lambda (player)
             (format "| %s [%2d]%4d |" tr--energy-char
                     (tr-player-energy-production player)
                     (tr-player-energy player)))
           (lambda (player)
             (format "| %s [%2d]%4d |" tr--heat-char
                     (tr-player-heat-production player)
                     (tr-player-heat player)))
           (lambda (_player)
             "+------------+")))
        (players (tr-game-state-players tr-game-state)))
    (dolist (line-gen panel-lines)
      (dolist (player players)
        (insert "  ")
        (insert (funcall line-gen player)))
      (insert "\n"))))

(defun tr--prompt-user-sell (card)
  "If applicable, prompt user to sell resources for CARD.
Result is an alist of (resource . amt) with (:total . amt) for the total sell price."
  ;; HEAT sell
  (let* ((tags (tr-card-tags card))
         (cost (tr-card-cost card))
         (total 0)
         (sell-amt 0)
         (heat-sell-amt 0)
         (sell-resource (cond
                         ((memq 'space tags)
                          'titanium)
                         ((memq 'building tags)
                          'steel))))

    (when sell-resource
      (let* ((sell-price (tr-get-player-resource-sell-amount tr-active-player sell-resource))
             (player-resource (terraform-get-requirement-count sell-resource))
             (max-sell-amt (min (ceiling (/ (float cost) sell-price)) player-resource))
             )
        (when (> max-sell-amt 0)
          (let* ((resp (read-number (format "%s to sell @ %d$ (0-%d):"
                                            (tr-resource-type-to-string sell-resource)
                                            sell-price
                                            max-sell-amt))))
            (unless (<= 0 resp max-sell-amt)
              (user-error "Invalid amount %d, should be between 0 and %d" resp max-sell-amt))
            (setq sell-amt resp)
            (cl-incf total (* sell-amt sell-price))))))
    (when (tr--player-can-sell-heat-p)
      (let* ((heat-resource (tr-get-requirement-count 'heat)))
        (when (> heat-resource 0)
          (let* ((max-sell-amt (min (- cost total) heat-resource))
                 (resp (read-number (format "%s to sell @ 1$ (0-%d):"
                                            tr--heat-char
                                            max-sell-amt))))
            (unless (<= 0 resp max-sell-amt)
              (user-error "Invalid amount %d, should be between 0 and %d" resp max-sell-amt))
            (setq heat-sell-amt resp)
            (cl-incf total heat-sell-amt)))))
    (if sell-resource
        `((,sell-resource . ,sell-amt)
          (heat . ,heat-sell-amt)
          (:total . ,total))
      `((heat . ,heat-sell-amt)
        (:total . ,total)))))

(defun tr--display-project-selection (project-ids)
  (insert "Select a Project:\n")
  (dolist (effect (terraform-player-next-turn-effects terraform-active-player))
    (pcase effect
      (`(discount ,amt)
       (insert (format "Next card has a discount of %d%s\n"
                       amt tr--money-char)))))
  (dolist (proj-id project-ids)
    (let* ((card (tr-card-by-id proj-id)))
      (insert "   " (button-buttonize "[ ]"
                                      (lambda (card)
                                        (let* ((sell-alist (tr--prompt-user-sell card)))
                                          (tr-get-args
                                           (tr--extract-card-actions card)
                                           (lambda (args)
                                             (tr-submit-response (car tr-pending-request)
                                                                 (list (list 'projects
                                                                             (tr-card-number card) (cons sell-alist args))))))))
                                      card)
              " "
              (tr-line-string card)
              "\n"))))

(defun tr--display-project-actions-selection (project-ids)
  (insert "Select a Project Action:\n")
  (dolist (project-id project-ids)
    (let* ((project (tr-card-by-id project-id))
           (actions (tr-card-action project)))
      (seq-do
       (lambda (action)
         ;; TODO - Normally UI shouldn't be filtering this selection
         (when (tr--player-satisfies-action-requirement tr-active-player action)
           (insert
            "   "
            (button-buttonize "[ ]"
                              (pcase-lambda (`(,project ,action))
                                (pcase-let ((`(-> ,cost ,effect) action))
                                  (tr-get-args ;; TODO - bad name: action of card and action of user
                                   (seq-filter #'identity (seq-map #'tr--extract-action (vector cost effect)))
                                   (lambda (args)
                                     (tr-submit-response
                                      (car tr-pending-request)
                                      (list (list 'project-action project action args)))))))
                              (list project action))
            (format " %s %s\n" (tr-card-propertized-name project) (tr-effect-to-string action)))))
       actions)))
  (insert "\n"))

(defun tr--display-standard-project-selection (standard-project-ids)
  (insert "Select a Standard Project:\n")
  (dolist (standard-project tr--standard-projects)
    (let ((label (tr-card-name standard-project))
          (cost (tr-card-cost standard-project))
          (proj-id (tr-card-number standard-project))
          ;; TODO centralize this logic
          (discount (apply #'+ (seq-map #'cadr (tr-played-continuous-effects 'standard-project-discount)))))
      (when (member proj-id standard-project-ids)
        (insert "   " (button-buttonize (format "[%2d] %s"
                                                (- cost discount)
                                                label)
                                        (lambda (standard-project)
                                          (tr-get-args
                                           (tr--extract-card-actions standard-project)
                                           (lambda (args)
                                             (tr-submit-response
                                              (car tr-pending-request)
                                              (list (list 'standard-projects (tr-card-number standard-project) args))))))
                                        standard-project)
                "\n")))))

(defun tr--display-extra-selection (extras)
  (insert "Other Actions:\n")
  (dolist (extra extras)
    (pcase extra
      ('pass (insert "   "
                     (button-buttonize "Pass"
                                       (lambda (_)
                                         (tr-submit-response (car tr-pending-request)
                                                             '((extra pass))))
                                       nil)
                     "\n"))
      ('skip (insert "   "
                     (button-buttonize "Skip"
                                       (lambda (_)
                                         (tr-submit-response (car tr-pending-request)
                                                             '((extra skip))))
                                       nil)
                     "\n"))
      ('heat-to-tempurature (insert "   "
                                    (button-buttonize
                                     "Convert heat to raise tempurature" ;; TODO amount
                                     (lambda (_)
                                       (tr-submit-response (car tr-pending-request)
                                                           '((extra heat-to-tempurature))))
                                     nil)
                                    "\n"))
      ('plant-to-greenery (insert "   "
                                    (button-buttonize
                                     "Convert plant to make greenery" ;; TODO amount
                                     (lambda (_)
                                       (tr-get-args (list (tr--extract-action '(add-greenery)))
                                                    (lambda (args)
                                                      (tr-submit-response
                                                       (car tr-pending-request)
                                                       (list (list 'extra 'plant-to-greenery args))))))
                                     nil)
                                    "\n")))))

(defun tr--display-standard-action-selection (actions)
  "Display the standard action selection input."
  (insert (format "Action %d/2\n" (1+ tr-action-no)))
  (dolist (action actions)
    (insert "\n")
    (let* ((action-symbol (car action))
           (action-params (cdr action)))
      (pcase action-symbol
        ('projects (tr--display-project-selection action-params))
        ('project-actions (tr--display-project-actions-selection action-params))
        ('standard-projects (tr--display-standard-project-selection action-params))
        ('extra (tr--display-extra-selection action-params))))))

(defun tr--select-starting-cards-selection (corps projects)
  ""
  (tr-selection
   :title "Select Starting Cards"
   :items `[(selection :title "Corporation"
                       :items ,corps
                       :type one)
            (selection :title "Projects"
                       :items ,projects
                       :type multiple)]
   :validation (lambda (corp projects)
                 ;; TODO: confirm costs...
                 (cond
                  ((and (not corp) (not projects))
                   '(quiet-error "Select corporation and project."))
                  ((not corp)
                   '(error "Select corporation"))
                  ((not projects)
                   '(warn "No projects selected"))
                  (t
                   (let* ((proj-count (length projects))
                          (msg (format "%d project(s) selected. Cost: $%d."
                                       proj-count
                                       (* proj-count 3))))
                     (list 'info msg)))))
   :on-confirm (lambda (corp projects)
                 (tr-submit-response
                  tr-active-player
                  (list corp projects)))))

(defun tr--select-research-cards (projects)
  (tr-selection
   :title "Select Research Cards"
   :items `[(selection :title "Projects"
                       :items ,projects
                       :type multiple)]
   :validation (lambda (projects)
                 (let* ((money (tr-player-money tr-active-player)))
                   (cond
                    ((< money (* 3 (length projects)))
                     '(error "Not enough Money"))
                    (t '(info "Select cards to purchase")))))
   :on-confirm (lambda (projects)
                 (tr-submit-response
                  tr-active-player
                  (list projects)))))

;; (cadr terraform-pending-request)
(defun tr--display-action-selection ()
  "Display the section asking user to make input."
  (let* ((request (cadr terraform-pending-request)))
    (pcase request
      (`(action ,actions)
       (tr--display-standard-action-selection actions))
      (`(select-starting-cards ,corps ,cards)
       (tr--select-starting-cards-selection corps cards))
      (`(select-research-cards ,cards)
       (tr--select-research-cards cards))
      ((pred (lambda (expr) (eql (car expr) 'selection)))
       (apply #'tr-selection (cdr request))))))

(defun tr--display-arg-selection ()
  "Display the main message prompting the user to select an argument."
  (insert "\n")
  (pcase (tr-current-pending-arg)
    ('city-placement-adj-2
     (insert "Please select land tile adjacent to two cities."))
    ('mining-bonus
     (insert (format "Please select a tile with a %s/%s bonus"
                     tr--steel-char tr--titanium-char)))
    ('empty-land
     (insert "Please select an empty land tile."))
    ('empty-ocean
     (insert "Please select an empty ocean tile."))
    ('standard-city-placement
     (insert "Please select a title to place city."))
    ('standard-greenery-placement
     (insert "Please select a title to place greenery."))
    ((pred (lambda (expr) (eql (car expr) 'selection)))
     (apply #'tr-selection (cdr (tr-current-pending-arg))))))

(defun tr--display-hand ()
  "Display the players current hand."
  (when tr-active-player
    (let ((hand (tr-player-hand tr-active-player)))
      (insert "\n\n")
      (insert (format "Hand (%d):\n" (length hand)))
      (dolist (project hand)
        (insert "- " (tr-line-string project) "\n")))))

(defun tr--display-in-front ()
  "Display the cards played in front of the user."
  (when tr-active-player
    (let ((in-front-projects (tr-player-played tr-active-player)))
      (insert "\n")
      (insert (format "Played Cards (%d)\n" (length in-front-projects)))
      ;; TODO: sort so that active are on top
      (dolist (proj in-front-projects)
        (insert "- " (tr-card-short-line-string proj) "\n")))))

(defconst tr-main-buffer "*terraforming*") ;; TODO: support multiple instances of the game

(defun tr-display-board ()
  "Display the board according to the current request."
  (with-current-buffer (get-buffer-create tr-main-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (tr--display-board)
      (tr--display-parameters)
      (tr--display-player-panel)
      (if (tr-current-pending-arg)
          (tr--display-arg-selection)
        (tr--display-action-selection))
      ;;; extra information
      (tr--display-hand)
      (tr--display-in-front))))


;;; Game Flow

(defvar tr-interstitial-action nil)

;; States:
;; start

(defvar tr-pending-request nil)
(defvar tr-active-player nil)
(defvar tr-active-card nil)
(defvar tr-action-no nil)

(defvar tr-pending-arg-request nil
  "The remaining arguments that a user must provide.")
(defvar tr-arg-callback nil  ;; TODO should I be making all of these private variables
  "The remaining arguments that a user must provide.")
(defvar tr-current-args nil
  "The current list of arguments a user has provided to a cards action.")

(defun tr--process-arg-selection (selection)
  (when (tr-current-pending-arg)
    (let ((ok t)) ;; TODO should I do validation here???
      (when ok
        (tr-push-arg selection)
        (when (not tr-pending-arg-request)
          (funcall tr-arg-callback tr-current-args))
        (when (and (listp (tr-current-pending-arg))
                   (eql (car (tr-current-pending-arg)) 'value))
          (tr--process-arg-selection (cdr (tr-current-pending-arg)))))))
  (tr-display-board))

(defun tr-current-pending-arg ()
  (car tr-pending-arg-request))

(defun tr-push-arg (new-arg)
  "Add NEW-ARG to list of in-progress args."
  (setq tr-current-args
        (append tr-current-args
                (list new-arg)))
  (setq tr-pending-arg-request
        (cdr tr-pending-arg-request)))

(defun tr-get-args (actions callback)
  "Add arg-fetch state, fetching ACTIONS, calling CALLBACK when done."
  (if (= (length actions) 0)
      (funcall callback nil)
    (setq tr-pending-arg-request actions)
    (setq tr-current-args nil)
    (setq tr-arg-callback callback)
    (tr-display-board)
    (let ((pending-arg (tr-current-pending-arg)))
      (when (and (listp pending-arg) (eql (car pending-arg) 'value))
        (tr--process-arg-selection (cdr (tr-current-pending-arg)))))))

(defun tr-!draw-corporations ()
  ""
  (let* ((corp-deck (tr-game-state-corporation-deck tr-game-state))
         (top-two (seq-take corp-deck 2)))
    (setf (tr-game-state-corporation-deck tr-game-state) (seq-drop corp-deck 2))
    top-two))

(defun tr-!reshuffle-deck ()
  (let* ((new-deck '())
         (exodeck (tr-game-state-exodeck tr-game-state))
         (players (tr-game-state-players tr-game-state))
         (player-projects (append (seq-mapcat #'tr-player-hand players)
                                  (seq-mapcat #'tr-player-played players))))
    (dolist (project exodeck)
      (when (not (member project player-projects))
        (push project new-deck)))
    (let* ((shuffled-deck (tr-randomize new-deck)))
      (setf (tr-game-state-deck tr-game-state) shuffled-deck))))

(defun tr-!draw-cards (n)
  "Return N cards from the top of the deck.  Cards are removed from the deck."
  (if (= n 0)
      '()
    (let* ((deck (tr-game-state-deck tr-game-state)))
      (if (< (length deck) n)
          (progn
            (let ((cards1 (tr-!draw-cards (length deck))))
              (tr-!reshuffle-deck)
              (append cards1 (tr-!draw-cards (- n (length deck))))))
        (let* ((exodeck (tr-game-state-exodeck tr-game-state))
               (top (seq-take deck n)))
          (setf (tr-game-state-exodeck tr-game-state) (append exodeck top))
          (setf (tr-game-state-deck tr-game-state) (seq-drop deck n))
          top)))))

(defun tr-!discard-cards (card-ids)
  (let* ((hand (tr-player-hand tr-active-player))
         (new-hand (seq-remove
                    (lambda (card) (member (tr-card-number card) card-ids))
                    hand)))
    (setf (tr-player-hand tr-active-player) new-hand)))

(defun tr-!play-in-front (project-id)
  "Move a card from the players hand to in front of them."
  (let* ((hand (tr-player-hand tr-active-player))
         (card (seq-find (lambda (card) (= (tr-card-number card) project-id)) hand))
         (new-hand (seq-remove
                    (lambda (card) (= (tr-card-number card) project-id))
                    hand))
         (played (tr-player-played tr-active-player)))
    (setf (tr-player-hand tr-active-player) new-hand)
    (setf (tr-player-played tr-active-player)
          (cons card played))))

(defun tr-!increment-user-resource (resource amount)
  "Increase the current users RESOURCE by AMOUNT."
  (unless tr-active-player
    (error "No active player's turn."))
  (pcase resource
    ('money
     (setf (tr-player-money tr-active-player)
           (max (+ (tr-player-money tr-active-player) amount) 0)))
    ('money-production
     (cl-incf (tr-player-money-production tr-active-player) amount))
    ('steel
     (setf (tr-player-steel tr-active-player)
           (max (+ (tr-player-steel tr-active-player) amount) 0)))
    ('steel-production
     (setf (tr-player-steel-production tr-active-player)
           (max (+ (tr-player-steel-production tr-active-player) amount) 0)))
    ('titanium
     (setf (tr-player-titanium tr-active-player)
           (max (+ (tr-player-titanium tr-active-player) amount) 0)))
    ('titanium-production
     (setf (tr-player-titanium-production tr-active-player)
           (max (+ (tr-player-titanium-production tr-active-player) amount) 0)))
    ('plant
     (setf (tr-player-plant tr-active-player)
           (max (+ (tr-player-plant tr-active-player) amount) 0)))
    ('plant-production
     (setf (tr-player-plant-production tr-active-player)
           (max (+ (tr-player-plant-production tr-active-player) amount) 0)))
    ('energy
     (setf (tr-player-energy tr-active-player)
           (max (+ (tr-player-energy tr-active-player) amount) 0)))
    ('energy-production
     (setf (tr-player-energy-production tr-active-player)
           (max (+ (tr-player-energy-production tr-active-player) amount) 0)))
    ('heat
     (setf (tr-player-heat tr-active-player)
           (max (+ (tr-player-heat tr-active-player) amount) 0)))
    ('heat-production
     (setf (tr-player-heat-production tr-active-player)
           (max (+ (tr-player-heat-production tr-active-player) amount) 0)))
    ('card
     (let ((cards (tr-!draw-cards amount)))
       (setf (tr-player-hand tr-active-player)
             (append (tr-player-hand tr-active-player)
                     cards))))
    ('rating
     (cl-incf (tr-player-rating tr-active-player) amount))
    (_ (error "incrementing unknown resource: %s" resource))))

(defun tr-!increase-tempurature (amt)
  (cl-incf (tr-game-state-param-tempurature tr-game-state) amt)
  (cl-incf (tr-player-rating tr-active-player) amt))

(defun tr-!increase-oxygen (amt)
  (cl-incf (tr-game-state-param-oxygen tr-game-state) amt)
  (cl-incf (tr-player-rating tr-active-player) amt))

(defun tr-!placement-bonus (coord)
  (let ((tile (tr--gameboard-tile-at coord)))
    ;; Tile Bonus
    (let* ((bonus (plist-get tile :bonus))
           (gain-steel-prod-p (member 'tile-placement-gain-steel-prod (tr-player-modifications tr-active-player))))
      (dolist (bonus-item bonus)
        (tr-!increment-user-resource bonus-item 1))
      (when (and gain-steel-prod-p
                 (or (member 'steel bonus)
                     (member 'titanium bonus)))
        (tr-!increment-user-resource 'steel-production 1)))
    ;; Ocean bonus
    (let* ((adj-tiles (tr--gameboard-adjacent-tiles coord))
           (ocean-bonus (* 2 (length (seq-filter (lambda (tile) (eql (plist-get tile :top) 'ocean)) adj-tiles)))))
      (tr-!increment-user-resource 'money ocean-bonus))))

(defun tr-!place-ocean (coord)
  ;; TODO: validate coord is ok
  (cl-incf (tr-game-state-param-ocean tr-game-state))
  (cl-incf (tr-player-rating tr-active-player))
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash coord board)))
    (tr-!placement-bonus coord)
    (puthash coord (plist-put tile :top 'ocean) board)
    (tr-!trigger-continuous-effects `(ocean-placed) tr-active-player)))

(defun tr-!place-special (coord id)
  ;; TODO: validate coord is ok
  (cl-incf (tr-game-state-param-ocean tr-game-state))
  (cl-incf (tr-player-rating tr-active-player))
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash coord board)))
    (tr-!placement-bonus coord)
    (puthash coord (plist-put (plist-put (plist-put tile :top 'special)
                                         :special-type id)
                              :player (tr-player-id tr-active-player))
             board)
    (tr-!trigger-continuous-effects `(special-placed) tr-active-player)))

(defun tr-!place-greenery (coord)
  ;; TODO: validate coord is ok
  (when (< (tr-game-state-param-oxygen tr-game-state) 14)
    (cl-incf (tr-game-state-param-oxygen tr-game-state))
    (cl-incf (tr-player-rating tr-active-player)))
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash coord board)))
    (puthash coord (plist-put (plist-put tile :top 'greenery) :player (tr-player-id tr-active-player))
             board)
    (tr-!trigger-continuous-effects `(greenery-placed) tr-active-player)))

(defun tr-!place-city (coord)
  ;; TODO: validate coord is ok
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash coord board)))
    (puthash coord (plist-put (plist-put tile :top 'city) :player (tr-player-id tr-active-player))
             board)
    (tr-!placement-bonus coord)
    (tr-!trigger-continuous-effects `(city-placed) tr-active-player)))

(defun tr-!place-city-noplace (city-id)
  "Simulates the placement of a city but does not actually place a city.
This is for the side-effect of city-placement."
  (let ((extra-spaces-ht (tr-gameboard-extra-spaces (tr-game-state-gameboard tr-game-state))))
    (puthash city-id (tr-player-id tr-active-player) extra-spaces-ht))
  (tr-!trigger-continuous-effects `(city-placed) tr-active-player))

(defun tr--count-thing (thing)
  (pcase thing
    ('every-city
     (let* ((coords (tr--board-coordinates))
            (ct 0))
       (dolist (coord coords)
         (let* ((tile (tr--gameboard-tile-at coord)))
           (when (eql (plist-get tile :top) 'city)
             (cl-incf ct))))
       ct))
    (`(tags ,tag-list)
     (let* ((played (tr-player-played tr-active-player)))
       (apply
        #'+
        (seq-map
         (lambda (card)
           (seq-count
            (lambda (tag)
              (member tag tag-list))
            (tr-card-tags card)))
         played))))
    (`(tags/ ,tag-list ,div-by)
     (let ((count (tr--count-thing (list 'tags tag-list))))
       (truncate count div-by)))
    (`(opponents-tags ,tag-list)
     (let* ((opponents (seq-filter
                        (lambda (player)
                          (not (equal playe tr-active-player)))
                        (tr-game-state-players tr-game-state))))
       (apply
        #'+
        (seq-map
         (lambda (opponent)
           (let ((tr-active-player opponent))
             (tr--count-thing (list 'tag tag-list))))
         opponents))))
    ('every-event
     (let* ((players (tr-game-state-players tr-game-state)))
       (apply
        #'+
        (seq-map
         (lambda (player)
           (seq-count
            (lambda (card)
              (eql (tr-card-type card) 'event))
            (tr-player-played player)))
         players))))))

(defun tr--project-action-inventors-guild (this player)
  (let ((card (car (tr-!draw-cards 1))))
    (setq tr-interstitial-action
          `(selection
            :title ,(format "Buy Card %s?"
                            (tr-line-string card))
            :items [(options ((yes . "Buy (3$)")
                              (no . "Don't buy")))]
            :validation (lambda (_) '(info ""))
            :on-confirm (lambda (selection)
                          (when (eql selection yes)
                            (tr-add-card-to-players-hand ,player ,card))
                          (tr-submit-response ,player nil))))))

(defun tr-!run-effect (effect &optional params card)
  (unless tr-active-player
    (error "No active player's turn."))
  (seq-do
   (lambda (action)
     (when action
       (let* ((tr-active-card card)
              (effect-id (car action))
              (effect-params (cdr action))
              (effect-definition (tr-card-effect-by-id effect-id))
              (param-required-p (or (terraform-card-extra-action effect-definition)
                                    (terraform-card-immediate-action effect-definition)))
              (action-func (terraform-card-effect-action effect-definition)))
         (unless action-func
           (error "no effect defined for %s" action))
         (if param-required-p
             (funcall (apply action-func effect-params) (pop params) )
           (apply action-func effect-params)))))
   effect))

(defun tr-!run-action (action params card)
  (unless tr-active-player
    (error "No active player's turn."))
  (unless (tr-card-p card)
    (error "invalid card: %s" card))
  (pcase-let ((`(-> ,cost ,effect) action))
    (if (vectorp effect)
        (seq-do
         (lambda (effect)
           (tr-!run-effect (vector cost effect) params card))
         effect)
      (tr-!run-effect (vector cost effect) params card))))

(defun tr--event-applicable-p (event owner handle-sym handle-player)
  ;; effect on card
  ;; -> effect that occurred
  (pcase handle-sym
    (`(spend ,amt)
     (when (eql owner handle-player)
      (pcase event
        (`(project-played ,card)
         (>= (tr-card-cost card) amt))
        (`(standard-project-played ,card)
         (>= (tr-card-cost card) amt)))))
    ('any-ocean
     (pcase event
       (`(ocean-placed) t)))
    ('ocean
     (when (eql owner handle-player)
      (pcase event
        (`(ocean-placed) t))))
    ('city
     (pcase event
       (`(city-placed) t)))
    ('any-city
     (pcase event
       (`(city-placed) t)))
    (`(and ,ands)
     (seq-every-p
      (lambda (and-item)
        (tr--event-applicable-p event owner handle-sym))
      ands))
    (`(tags ,tags)
     (when (eql owner handle-player)
       (pcase event
         (`(project-played ,card)
          (seq-find
           (lambda (tag)
             (seq-find (lambda (other-tag) (eql tag other-tag)) (tr-card-tags card)))
           tags)))))
    ('event
     (when (eql owner handle-player)
       (pcase event
         (`(project-played ,card)
          (eql (tr-card-type card) event)))))))

(defun tr-!trigger-continuous-effects (event owner)
  "Trigger EVENT, caused by player OWNER."
  (let* ((players (tr-game-state-players tr-game-state)))
    (dolist (player players)
      (let ((played-projects (tr-player-played player)))
        (dolist (project played-projects)
          (let* ((continuous-effect (tr-card-continuous-effect project)))
            (pcase continuous-effect
              (`(on ,handle-sym ,action)
               (when (tr--event-applicable-p event owner handle-sym player)
                 (tr-!run-effect action nil project))))))
        ;; abstract corps and projects
        (let* ((continuous-effect (tr-corporation-continuous-effect
                                   (tr-player-corp-card player))))
          (pcase continuous-effect
            (`(on ,handle-sym ,action)
             (when (tr--event-applicable-p event owner handle-sym player)
               (tr-!run-effect action nil nil)))))))))

(defun tr-!run-project (project-id params)
  "Run the effects of a given card."
  ;; Note that the first parameter when running a project is always
  ;; the cards buy amount (regardless of whether or not it was
  ;; building or space).
  (let* ((effects (terraform-player-next-turn-effects terraform-active-player))
         (discount-amt (or (cadr (seq-find (lambda (effect) (eql (car effect) 'discount)) effects))
                            0))
         (sell-amounts (pop params))
         (card (tr-card-by-id project-id))
         (cost (max (- (tr-card-cost card) discount-amt) 0))
         (final-cost (max (- cost (alist-get :total sell-amounts)) 0))
         (effect (tr-card-effect card)))
    (tr-!increment-user-resource 'money (- final-cost))
    (dolist (item sell-amounts)
      (pcase item
        (`(,resource . ,amt)
         (when (not (eql resource :total))
           (tr-!increment-user-resource resource (- amt))))))
    (tr-!run-effect effect params)
    (tr-!play-in-front project-id)
    (tr-!trigger-continuous-effects `(project-played ,card) tr-active-player))
  (setf (terraform-player-next-turn-effects terraform-active-player) nil))

;; Turn order ---

(defun tr-all-players-passed-p ()
  (>= (length (tr-game-state-passed-players tr-game-state))
      (length (tr-game-state-players tr-game-state))))

(defun tr-player-generation (player)
  (let ((rating (tr-player-rating player))
        (money-prod (tr-player-money-production player))
        (steel-prod (tr-player-steel-production player))
        (titanium-prod (tr-player-titanium-production player))
        (plant-prod (tr-player-plant-production player))
        (energy-prod (tr-player-energy-production player))
        (energy (tr-player-energy player))
        (heat-prod (tr-player-heat-production player)))
    (cl-incf (tr-player-money player) (+ money-prod rating))
    (cl-incf (tr-player-steel player) steel-prod)
    (cl-incf (tr-player-titanium player) titanium-prod)
    (cl-incf (tr-player-plant player) plant-prod)
    (setf (tr-player-energy player) energy-prod)
    (cl-incf (tr-player-heat player) (+ heat-prod energy))))

(defun tr-!next-generation ()
  (setq tr-action-no 0)
  (cl-incf (tr-game-state-generation tr-game-state))
  (setf (tr-game-state-passed-players tr-game-state) '())
  (dolist (player (tr-game-state-players tr-game-state))
    (dolist (card (tr-player-played player))
      (setf (tr-card-used card) nil))
    (tr-player-generation player))
  ;; TODO: Rotate players
  (let* ((cards (tr-!draw-cards 4)))
    (tr->request tr-active-player
                 `(select-research-cards ,cards)
                 #'tr-!research-projects-selected)))

(defun tr-!player-pass ()
  (let* ((active-player-id (tr-player-id tr-active-player))
         (next-player (tr-get-player-after tr-active-player)))
    (setf (tr-game-state-passed-players tr-game-state)
          (cons active-player-id
                (tr-game-state-passed-players tr-game-state)))
    (if (tr-all-players-passed-p)
        (tr-!next-generation)
      (setq tr-active-player next-player) ;; TODO: set player's state to passed
      (setq tr-action-no 0)
      (tr->request next-player (tr-actions-for next-player)
                   #'tr-action-performed)))
  (throw 'ignore-action-step nil))

(defun tr-!player-skip ()
  (let* ((active-player-id (tr-player-id tr-active-player))
         (next-player (tr-get-player-after tr-active-player)))
    (setq tr-active-player next-player)
    (setq tr-action-no 0)
    (tr->request next-player (tr-actions-for next-player)
                 #'tr-action-performed))
  (throw 'ignore-action-step nil))

(defun tr-?all-players-ready () ;; TODO
  "Return non-nil if all players are ready to start the game."
  t)

(defun tr-?first-player ()
  "Return the player who should go first."
  (car (tr-game-state-players tr-game-state)))

;; Action generation

(defun tr-get-requirement-count (item) ;; TODO: Better name
  (pcase item
    ('money (tr-player-money tr-active-player))
    ('money-production (tr-player-money-production tr-active-player))
    ('steel (tr-player-steel tr-active-player))
    ('steel-production (tr-player-steel-production tr-active-player))
    ('titanium (tr-player-titanium tr-active-player))
    ('titanium-production (tr-player-titanium-production tr-active-player))
    ('plant-production (tr-player-plant-production tr-active-player))
    ('plant (tr-player-plant tr-active-player))
    ('energy-production (tr-player-energy-production tr-active-player))
    ('energy (tr-player-energy tr-active-player))
    ('heat-production (tr-player-heat-production tr-active-player))
    ('heat (tr-player-heat tr-active-player))
    ('ocean (tr-game-state-param-ocean tr-game-state))
    ('oxygen (tr-game-state-param-oxygen tr-game-state))
    ('tempurature (tr-ct-to-tempurature (tr-game-state-param-tempurature tr-game-state)))
    ('science-tag (tr-count-player-tag tr-active-player 'science))
    ('microbe-tag (tr-count-player-tag tr-active-player 'microbe))
    ('plant-tag (tr-count-player-tag tr-active-player 'plant))
    ('greenery (tr-game-board-count-greeneries))))

(defun tr-get-tile-count (top-sym)
  (let ((ct 0))
   (dolist (coord (tr--board-coordinates))
     (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
            (tile (gethash coord board))
            (top (plist-get tile :top))
            (player (plist-get tile :player)))
       (when (and (eql player (tr-player-id tr-active-player))
                  (eql top-sym top))
         (cl-incf ct))))
   ct))

(defun tr-requirements-satisfied-p (requirements)
  (terraform-eval-card-condition requirements))

(defun tr-playable-projects-for (player)
  (let ((money (tr-player-money player))
        (hand (tr-player-hand player)))
    (let ((tr-active-player player))
      (seq-map
       (lambda (card)
         (tr-card-number card))
       (seq-filter
        (lambda (card)
          (let ((cost (tr-card-cost card))
                (requirements (tr-card-requirements card))
                (effects (tr-card-effect card)))
            (and (>= money cost)
                 (tr-requirements-satisfied-p requirements)
                 (seq-every-p (lambda (effect)
                                (let* ((effect-id (car effect))
                                       (effect-params (cdr effect))
                                       (effect-data (tr-card-effect-by-id effect-id))
                                       (requirement (tr-card-effect-requirement effect-data)))
                                  (or (not requirement)
                                      (apply requirement effect-params))))
                              effects))))
        hand)))))

(defun tr-playable-project-actions-for (player)
  (let ((tr-active-player player)
        (hand (tr-player-played player)))
    (seq-map
     (lambda (project)
       (tr-card-number project))
     (seq-filter
      (lambda (project)
        (and (tr-card-action project)
             (not (tr-card-used project))))
      hand))))

(defun tr-standard-projects-for (player)
  "Return list of all standard projects a player can take."
  (let ((hand (tr-player-hand player))
        (money (tr-player-money player))
        ;; TODO Centralize discount
        (discount (apply #'+ (seq-map #'cadr (tr-played-continuous-effects 'standard-project-discount)))))
    (setq money (+ money discount))
    (seq-filter
     #'identity
     ;; TODO: Centralize these variables
     (list (when (>= money 11) 'power-plant)
           (when (>= money 14) 'asteroid)
           (when (>= money 18) 'aquifer)
           (when (>= money 23) 'greenery)
           (when (>= money 25) 'city)
           (when (and hand (> (length hand) 0)) 'sell-patents)))))

(defun tr-extra-actions-for (player)
  (let* ((actions (if (= tr-action-no 0) '(pass) '(skip)))
         (plant-count (tr-player-plant player))
         (heat-count (tr-player-heat player))
         (greenery-cost (tr-get-player-greenery-cost)))
    (when (>= plant-count greenery-cost)
      (push 'plant-to-greenery actions))
    (when (and (>= heat-count 8) (< (tr-game-state-param-tempurature tr-game-state) 19))
      (push 'heat-to-tempurature actions))
    actions))

(defun tr-actions-for (player)
  "Return description of all actions the player can take."
  `(action ((projects . ,(tr-playable-projects-for player))
            (project-actions . ,(tr-playable-project-actions-for player))
            (standard-projects . ,(tr-standard-projects-for player))
            (extra . ,(tr-extra-actions-for player)))))

(defun tr-get-player-after (player)
  ;; TODO: handle "passed" players
  (catch 'done
    (let* ((all-players (tr-game-state-players tr-game-state))
           (i 0))
      (while t
        (when (eql (tr-player-id (nth i all-players))
                   (tr-player-id player))
          (throw 'done (nth (mod (1+ i) (length all-players))
                            all-players)))))))

(defun tr-action-step ()
  (if (eql tr-action-no 0)
      (progn
        (setq tr-action-no 1))
    (let ((next-player (tr-get-player-after tr-active-player)))
      (setq tr-active-player next-player)
      (setq tr-action-no 0)))
  (tr->request tr-active-player (tr-actions-for tr-active-player)
               #'tr-action-performed))

(defun interstitial-action-performed (player &optional _action)
  "Callback for PLAYER after interstitial action performed."
  (tr-action-step)
  (tr-display-board))

;; TODO: I don't like how the actions spec is all over the place...
(defun tr-action-performed (player action)
  "Generic PLAYER action of specification ACTION"
  (unless (tr-player-p player)
    (error "Invalid type (terraform-player-p %s)" player))
  (let ((tr-interstitial-action nil))
    (catch 'ignore-action-step
      (pcase action
        (`(projects ,project-id ,params)
         (tr-!run-project project-id params))
        (`(project-action ,card ,action ,params)
         (tr-!run-action action params card)
         (setf (tr-card-used card) t))
        (`(standard-projects power-plant ,_)
         (tr-!run-effect [(dec money 11) (inc energy-production 1)]))
        ;; TODO Trigger effect on standard project
        ;; TODO incorporate with tr--standard-projects
        (`(standard-projects asteroid ,_)
         (tr-!run-effect [(dec money 14) (inc-tempurature)]))
        (`(standard-projects aquifer ,args) ;; TODO change "location" to generic PARAMS
         (tr-!run-effect [(dec money 18) (add-ocean)] args))
        (`(standard-projects greenery ,args) ;; TODO change "location" to generic PARAMS
         (tr-!run-effect [(dec money 23) (add-greenery)] args))
        (`(standard-projects city ,args) ;; TODO change "location" to generic PARAMS
         (tr-!run-effect [(dec money 25) (add-city)] args))
        (`(standard-projects sell-patents ,card-ids) ;; TODO change "cards" to generic PARAMS
         (tr-!discard-cards card-ids)
         (tr-!increment-user-resource 'money (length card-ids)))
        (`(extra pass)
         (tr-!player-pass))
        (`(extra skip)
         (tr-!player-skip))
        (`(extra plant-to-greenery ,args)
         (let* ((greenery-cost (tr-get-player-greenery-cost)))
           (tr-!run-effect `[(dec plant ,greenery-cost) (add-greenery)] args)))
        (`(extra heat-to-tempurature)
         (tr-!run-effect [(dec heat 8) (inc-tempurature)]))
        (_ (error "Invalid action response %s" action)))
      ;; hacky way to implement standard project discount
      (when (equal (car action) 'standard-projects)
        (let ((standard-project-discount
               (apply #'+ (seq-map #'cadr (tr-played-continuous-effects 'standard-project-discount)))))
          (tr-!run-effect `[(inc money ,standard-project-discount)])))
      (if tr-interstitial-action
          (tr->request tr-active-player tr-interstitial-action
                       #'interstitial-action-performed)
        (tr-action-step)))))

(defun tr-!research-projects-selected (player cards)
  (setf (tr-player-hand player)
        (append (tr-player-hand player)
                cards))
  (cl-decf (tr-player-money player) (* 3 (length cards)))
  ;; TODO - wait for all players
  ;; TODO - Rotate players
  (let* ((first-player (tr-?first-player)))
    (tr->request first-player
                 (tr-actions-for first-player)
                 #'tr-action-performed)))

(defun tr-!corporation-selected (player selected-corp selected-cards) ;; TODO: change name
  "Action to make initial selection of PLAYER for SELECTED-CORP and SELECTED-CARDS."
  ;; TODO Check that selection is valid
  (unless (tr-player-p player)
    (error "Invalid type (terraform-player-p %s)" player))
  (unless (tr-corporation-p selected-corp)
    (error "Invalid type (terraform-corporation-p %s)" selected-corp))
  (unless (seq-every-p #'tr-card-p selected-cards)
    (error "Invalid type (seq-every-p 'terraform-card-p %s)" selected-cards))
  (setf (tr-player-corp-card player) selected-corp)
  (let ((tr-active-player player))
    (tr-!run-effect (tr-corporation-effect selected-corp))
    (tr-!increment-user-resource 'money (- (* 3 (length selected-cards))))
    (setf (tr-player-hand tr-active-player) selected-cards)) ;; TODO beginner corp?
  ;; TODO - apply effect of corporation
  (if (tr-?all-players-ready)
      (let ((first-player (tr-?first-player)))
        (setq tr-active-player first-player)
        (setq tr-action-no 0)
        (tr->request first-player (tr-actions-for first-player)
                     #'tr-action-performed))
    (setq tr-pending-request nil)))

(defun tr->request (player action callback)
  (setq tr-pending-request (list player action callback)))

(defun tr-submit-response (_player response) ;; TODO: do we really need to pass the player here?
  "Submit RESPONSE for PLAYER."
  (unless tr-pending-request
    (error "No request to submit response for"))
  (seq-let (player _action callback) tr-pending-request
    (apply callback (cons player response))
    (tr-display-board)))

(defun tr-reset-game-state ()
  ;; TODO
  (setq tr-current-args nil)
  (setq tr-arg-callback nil)
  (setq tr-current-selection nil)
  (setq tr-selection-spec nil)
  (setq tr-active-player nil)
  (setq tr-pending-arg-request nil))

(defun tr-run ()
  "Demo command to run game."
  (unless tr-game-state
    (error "No initialized game state"))
  (tr-reset-game-state)
  (setq tr-selection-spec nil
        tr-current-selection nil)
  (setq tr-game-state (tr--new-game-state 1))
  (dolist (player (tr-game-state-players tr-game-state))
    (let ((corps (tr-!draw-corporations))
          (cards (tr-!draw-cards 10)))
      (tr->request player
                   `(select-starting-cards ,corps ,cards)
                   #'tr-!corporation-selected))))

(defun tr-run-with-selected-card ()
  "Demo command to run game."
  (unless tr-game-state
    (error "No initialized game state"))
  (tr-reset-game-state)
  (setq tr-selection-spec nil
        tr-current-selection nil)
  (setq tr-game-state (tr--new-game-state 1))
  (dolist (player (tr-game-state-players tr-game-state))
    (let* ((card-names (seq-map
                        (lambda (id)
                          (plist-get (gethash id tr-card-directory) :name ))
                        (hash-table-keys tr-card-directory)))
           (selected-card-name (completing-read "Card:" card-names nil t))
           (selected-id (seq-find (lambda (id)
                                    (equal (plist-get (gethash id tr-card-directory) :name) selected-card-name))
                                  (hash-table-keys tr-card-directory)))
           (corps (tr-!draw-corporations))
           (cards (cons (apply #'terraform-card-create (gethash selected-id tr-card-directory)) (tr-!draw-cards 10))))
      (tr->request player
                   `(select-starting-cards ,corps ,cards)
                   #'tr-!corporation-selected))))


;;;  Game Mode

(defun tr-dwim ()
  (interactive)
  (let* ((coords (get-text-property (point) 'tr-tile)))
    (tr--process-arg-selection coords)))

(defconst tr-mode-map
  (let ((map (make-keymap)))
    (prog1 map
      (define-key map (kbd "RET") #'tr-dwim))))

(define-derived-mode tr-mode fundamental-mode "tr-game"
  "Major mode for interacting with tr game-board."
  (setq buffer-read-only t)
  (setq-local truncate-lines 0)
  (toggle-truncate-lines 1))

;;; TODO LIST
;; Card to string
;; Basic selection process to updating the state.

;;; DEMO SETUP

(progn (tr-run-with-selected-card)
       (tr-display-board))

;; (let* ((p1 (car (terraform-game-state-players terraform-game-state)))
;;        (req (cadr terraform-pending-request))
;;        (corps (cadr req))
;;        (selected-corps (car corps))
;;        (cards (caddr req))
;;        (selected-cards (seq-take cards 3)))
;;   (terraform-submit-response p1 (list selected-corps selected-cards)))

;; (cadr terraform-pending-request)

;; (tr-player-hand (car tr-pending-request))

(provide 'terraform)

;;; terraform.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("tr-" . "terraform-"))
;; End:
