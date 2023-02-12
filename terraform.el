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
  played)


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
    (:bonus (plant steel) :name pavonis) (:bonus (plant))
    (:bonus (plant)) (:bonus (plant)) (:bonus (plant plant))
    (:bonus (plant)) (:bonus (plant)) (:bonus (plant plant) :type ocean)

    ;; equator
    (:bonus (plant plant) :name arsia) (:bonus (plant plant))
    (:bonus (plant plant) :name noctus) (:bonus (plant plant) :type ocean)
    (:bonus (plant plant) :type ocean) (:bonus (plant plant) :type ocean)
    (:bonus (plant plant)) (:bonus (plant plant)) (:bonus (plant plant))

    ;; sixth rown
    (:bonus (plant)) (:bonus (plant plant)) (:bonus (plant))
    (:bonus (plant)) (:bonus (plant 1)) (:bonus (plant) :type ocean)
    (:bonus (plant) :type ocean) (:bonus (plant) :type ocean :top ocean)

    ;; seventh row
    (:top special :special-type lava-flows :player player4) () () () () (:bonus (plant)) ()

    ;; eighth row
    (:bonus (steel steel) :top city :player player2) () (:bonus (card)) (:bonus (card)) () (:bonus (titanium))

    ;; bottom row
    (:bonus (steel) :top greenery :player player1) (:bonus (steel steel)) () () (:bonus (steel steel) :type ocean))
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
  effect)

(defconst tr--sample-card-1
  ;; TODO - Requirements for cards
  ;; TODO - Display requirements for cards
  ;; TODO - Global parameter requirement O2
  ;; TODO - Figure out card tags
  (tr-card-create
   :number 1
   :name "Colonizer Training Camp"
   :cost 8
   :tags '(jovian building)
   :type 'automated
   :victory-points 2
   :type 'automated))
(defconst tr--sample-card-2
  ;; TODO - Global parameter requirement production
  (tr-card-create
   :number 2
   :name "Asteroid Mining Consortium"
   :cost 13
   :tags '(jovian)
   :victory-points 1
   :requirements '(> titanium-production 0)
   :type 'automated
   :effect [(dec-other titanium-production 1) (inc titanium-production 1)]))
(defconst tr--sample-card-3
  (tr-card-create
   :number 3
   :name "Deep Well Heating"
   :cost 13
   :type 'automated
   :tags '(power building)
   :effect [(inc-tempurature) (inc energy-production 1)]))
(defconst tr--sample-card-4
  (tr-card-create
   :number 4
   :name "Cloud Seeding"
   :cost 11
   :type 'automated
   :requirements '(>= ocean 3)
   :effect [(dec money-production 1) (inc heat-production 1) (inc plant-production 2)]))
(defconst tr--sample-card-5
  (tr-card-create
   :number 5
   :name "Search For Life"
   :cost 13
   :type 'active
   :tags '(science)
   :victory-points '(* resource 3)
   :requirements '(<= oxygen 6)
   :type 'automated
   :action '[(-> (dec money 1)
                 (action "MICR.*:SCI"
                         (lambda ())))]))
(defconst tr--sample-card-6
  (tr-card-create
   :number 6
   :name "Inventors' Guild"
   :cost 9
   :type 'active
   :tags '(science)
   :action '[(-> nil
                 (action "Look at top card, buy or discard"
                         (lambda ())))]))
(defconst tr--sample-card-7
  (tr-card-create
   :number 7
   :name "Martian Rails"
   :cost 13
   :type 'active
   :tags '(building)
   :action '[(-> (dec energy 1)
                 (inc-per money every-city))]))
(defconst tr--sample-card-8
  (tr-card-create
   :number 8
   :name "Capital"
   :cost 26
   :type 'automated
   :tags '(city building)
   :tile 'capital
   :requirements '(>= ocean 4)
   :effect [(dec energy-production 2) (inc money-production 5)]
   :victory-points
   (lambda ()
     (let ((tile (tr-get-tile-by-name 'capital)))
       (when tile
         (length (seq-filter
                  (lambda (tile) (eql (plist-get tile :top) 'ocean))
                  (tr-get-adjacent-tiles))))))))
(defconst tr--sample-card-9
  (tr-card-create
   :number 9
   :name "Asteroid"
   :cost 14
   :type 'event
   :tags '(space event)
   :effect [(inc-tempurature) (inc titanium 5) (dec-other plant 3)]))
(defconst tr--sample-card-10
  (tr-card-create
   :number 10
   :name "Comet"
   :cost 21
   :type 'event
   :tags '(space event)
   :effect [(inc-tempurature) (add-ocean) (dec-other plant 3)]))
(defconst tr--sample-card-11
  (tr-card-create
   :number 11
   :name "Big Asteroid"
   :cost 27
   :type 'event
   :tags '(space event)
   :effect [(inc-tempurature 2) (inc titanium 4) (dec-other plant 4)]))
(defconst tr--sample-card-12
  (tr-card-create
   :number 12
   :name "Water Import From Europa"
   :cost 25
   :type 'active
   :tags '(space jovian)
   :action '[(-> (buy titanium 12)
                 (add-ocean))]
   :victory-points (lambda () (error "not implemented"))))
(defconst tr--sample-card-13
  (tr-card-create
   :number 13
   :name "Space Elevator"
   :type 'active
   :action '[(-> (dec steel 1)
                 (inc money 5))]
   :effect [(inc titanium-production 1)]
   :victory-points 2))
(defconst tr--sample-card-14
  (tr-card-create
   :number 14
   :name "Development Center"
   :type 'active
   :action '[(-> (dec energy 1)
                 (draw-project 1))]))
(defconst tr--sample-card-15
  (tr-card-create
   :number 15
   :name "Equatorial Magnetizer"
   :type 'active
   :action '[(-> (dec energy-production 1)
                 (draw-project 1))]))

(defconst tr-all-cards
  (list tr--sample-card-1
        tr--sample-card-2
        tr--sample-card-3
        tr--sample-card-4
        tr--sample-card-5
        tr--sample-card-6
        tr--sample-card-7
        tr--sample-card-8
        tr--sample-card-9
        tr--sample-card-10
        tr--sample-card-11
        tr--sample-card-12))

(defun tr-card-by-id (id)
  "Return the card of the given ID."
  (seq-find (lambda (card)
              (= (tr-card-number card) id))
            tr-all-cards))

(cl-defstruct (tr-corporation
               (:constructor tr-corporation-create)
               (:copier nil))
  number
  name
  tags
  effect
  active-effect
  action
  first-move)

(defconst tr--sample-corp-1
  (tr-corporation-create
   :number 1
   :name "Credicor"
   :tags nil
   :effect [(inc money 57)]
   :active-effect "(on (spend 20) (inc money 4))"))
(defconst tr--sample-corp-2
  (tr-corporation-create
   :number 2
   :name "Ecoline"
   :tags '(plant)
   :effect [(inc plant-production 2) (inc money 36) (inc plant 3)]
   :active-effect "(set greenery-cost 7)"))
(defconst tr--sample-corp-2
  (tr-corporation-create
   :number 3
   :name "Helion"
   :tags '(space)
   :effect [(inc heat-production 3) (inc money 42)]
   :active-effect "(set heat-as-money t)"))
(defconst tr--sample-corp-3
  (tr-corporation-create
   :number 3
   :tags '(space)
   :effect [(inc heat-production 3) (inc money 42)]
   :active-effect "(set heat-as-money t)"))
(defconst tr--sample-corp-4
  (tr-corporation-create
   :number 4
   :name "Mining Guild"
   :tags '(building building)
   :effect [(inc money 30) (inc steel 5) (inc steel-production 1)]
   :active-effect "(on gain-board-steel-or-titanium (inc steel-production 1))"))
(defconst tr--sample-corp-5
  (tr-corporation-create
   :number 5
   :name "Interplanetary Cinematics"
   :tags '(building)
   :effect [(inc money 30) (inc steel 20)]
   :active-effect "(on (play-tag event) (inc money 2))"))

(defun tr-sample-deck ()
  (list tr--sample-card-1 tr--sample-card-2 tr--sample-card-3 tr--sample-card-4 tr--sample-card-5 tr--sample-card-6 tr--sample-card-7
        tr--sample-card-8 tr--sample-card-9 tr--sample-card-10 tr--sample-card-11 tr--sample-card-12))

(defun tr--sample-corporation-deck ()
  (list tr--sample-corp-1 tr--sample-corp-2 tr--sample-corp-3 tr--sample-corp-4 tr--sample-corp-5))

;; (tr--sample-corporation-deck)



;;; Generic Functions

(defun tr-resource-type-to-string (type)
  (pcase type
    ('money-production (tr--char->prod tr--money-char))
    ('money tr--money-char)
    ('plant-production (tr--char->prod tr--plant-char))
    ('plant tr--plant-char)
    ('steel-production (tr--char->prod tr--steel-char))
    ('steel tr--steel-char)
    ('titanium-production (tr--char->prod tr--titanium-char))
    ('titanium tr--titanium-char)
    ('energy-production (tr--char->prod tr--energy-char))
    ('energy tr--energy-char)
    ('heat-production (tr--char->prod tr--heat-char))
    ('heat tr--heat-char)
    (_ "?")))

(defun tr-effect-to-string (clause)
  "Convert a card effect data description to a string."
  (pcase clause
    ('() "")
    (`(inc ,resource ,amt)
     (format "+%d%s" amt (tr-resource-type-to-string resource)))
    (`(dec ,resource ,amt)
     (format "-%d%s" amt (tr-resource-type-to-string resource)))
    (`(dec-other ,resource ,amt)
     (format "-%d%s" amt (tr--char->decrease-any (tr-resource-type-to-string resource))))
    (`(inc-tempurature)
     "℃")
    (`(add-ocean)
     "+H₂O")
    (`(-> ,cost ,action)
     (format "%s%s%s"
             (tr-effect-to-string cost)
             tr--action-arrow
             (tr-effect-to-string action)))
    (`(action ,disp ,_)
     disp)
    (_ (format "%s" clause))))

(defun tr-effects-to-string (effect)
  "Convert a card effect data description to a string."
  (string-join
   (seq-map
    (lambda (clause)
      (tr-effect-to-string clause))
    effect)
   "; "))

(cl-defgeneric tr-line-string (item)
  (:documentation "Display item on a single line."))

(cl-defmethod tr-line-string ((item tr-card))
  (let* ((card-type (tr-card-type item))
         (card-face (pcase card-type
                      ('active 'tr-active-face)
                      ('automated 'tr-automated-face)
                      ('event 'tr-event-face)))
         (card-name (propertize (tr-card-name item) 'font-lock-face card-face)))
    (format "$%2d %s %s %s"
            (tr-card-cost item)
            card-name
            (tr-effects-to-string (tr-card-effect item))
            (tr-effects-to-string (tr-card-action item)))))

(cl-defmethod tr-line-string ((item tr-corporation))
  (format "%s %s" (tr-corporation-name item) (tr-effects-to-string (tr-corporation-effect item))))


;;; Game State
;; The game state is stored in the tr-game-state struct which contains
;; all of the information about the current state of the game.  The
;; global parameters are on a scale of 0 to max, including the
;; tempurature.

(defvar tr-game-state nil)

(defconst tr--game-state-max-tempurature 14)
(defconst tr--game-state-max-oxygen 19)
(defconst tr--game-state-max-ocean 9)

(defun tr-oxygen-to-ct (oxygen)
  (/ (+ oxygen 30) 2))

(defun tr-ct-to-oxygen (oxygen)
  (- (* oxygen 2) 30))

(cl-defstruct
    (tr-game-state
     (:constructor tr-game-state-create)
     (:copier nil))
  gameboard
  players
  deck
  corporation-deck
  param-ocean
  param-tempurature
  param-oxygen
  generation
  passed-players
  state)

(defun tr--new-game-state (player-ct)
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
   :deck (tr-sample-deck)
   :state 'start
   :corporation-deck (tr--sample-corporation-deck)))

(defun tr--game-state-player-ct ()
  (length (tr-game-state-players tr-game-state)))

;;
(setq tr-game-state (tr--new-game-state 1))
;; (tr-game-state-deck (tr--new-game-state 1))


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

(defconst tr--tree-char "♠")
(defconst tr--city-char "#")

(defconst tr--card-char "▮")
(defconst tr--money-char (propertize "$" 'font-lock-face 'tr-money-face))
(defconst tr--steel-char (propertize "⚒" 'font-lock-face 'tr-steel-face))
(defconst tr--titanium-char (propertize "*" 'font-lock-face 'tr-titanium-face))
(defconst tr--plant-char (propertize "☘" 'font-lock-face 'tr-plant-face))
(defconst tr--energy-char (propertize "↯" 'font-lock-face 'tr-energy-face))
(defconst tr--heat-char (propertize "≋" 'font-lock-face 'tr-heat-face))

(defconst tr--action-arrow (propertize "→" 'font-lock-face '(:foreground "red" :weight bold) ))

(defun tr--char->decrease-any (char)
  "Add properties to CHAR to make it indicate production."
  (propertize char 'font-lock-face
              (list
               '(:foreground "red")
               (get-text-property 0 'font-lock-face char))))

(defun tr--char->prod (char)
  "Add properties to CHAR to make it indicate production."
  (propertize char 'font-lock-face
              (list
               '(:box (:line-width (3 . 3)
                                   :color "brown"))
               (get-text-property 0 'font-lock-face char))))

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
                     :bottom-line " ### "))
    ))

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
                          (neuclear-zone . "NUCLR")
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
               (concat "   " (tr--board-player-char player) "   ")
               (concat "   " (tr--board-player-char player) "   ")
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
                          (noctus . "Noctus")))
         (bonus (plist-get tile :bonus))
         (name (plist-get tile :name))
         (display-name (alist-get name display-names)))
    (nth line-no
         (list (propertize "     " 'font-lock-face 'tr-ocean-face)
               (concat (propertize " " 'font-lock-face 'tr-ocean-face)
                       (tr--board-format-bonus bonus)
                       (propertize " " 'font-lock-face 'tr-ocean-face))
               (format "%7s" display-name)
               (concat " _ _ ")))))

(defun tr--board-line (pt line-no)
  (let* ((line-length (if (memq line-no '(0 3)) 5 7))
         (content (if (= line-no 3)
                      (seq-let (q r s) pt
                        (let ((bottom-pt (list q (1+ r) (1- s))))
                          (if (or (tr--in-board-p pt)
                                  (tr--in-board-p bottom-pt))
                              " _ _ "
                            "     ")))
                    (make-string line-length ?\s))))
    (if (not (tr--in-board-p pt))
        content
      (let* ((at-tile (tr--gameboard-tile-at pt))
             (type (plist-get at-tile :type))
             (top (plist-get at-tile :top))
             (name (plist-get at-tile :name)))
        (cond
         ((eql top 'ocean)
          (tr--board-line-top-ocean at-tile line-no))
         ((eql top 'greenery)
          (tr--board-line-top-greenery at-tile line-no))
         ((eql top 'city)
          (tr--board-line-top-city at-tile line-no))
         ((eql top 'special)
          (tr--board-line-top-special at-tile line-no))
         ((eql type 'ocean)
          (tr--board-line-ocean at-tile line-no))
         (name
          (tr--board-line-name at-tile line-no))
         (t content))))))

(defun tr--display-board ()
  (let ((parts '((" "
                  (lambda (a b c atl abl)
                    (insert (tr--format-edge atl a "/") (tr--board-line a 0) (tr--format-edge a b "\\") (tr--board-line b 2))))
                 (""
                  (lambda (a b c atl abl)
                    (insert (tr--format-edge atl a "/") (tr--board-line a 1) (tr--format-edge a b "\\") (tr--board-line b 3))))
                 (""
                  (lambda (a b c atl abl)
                    (insert (tr--format-edge abl a "\\") (tr--board-line a 2) (tr--format-edge a c "/") (tr--board-line c 0))))
                 (" "
                  (lambda (a b c atl abl)
                    (insert (tr--format-edge abl a "\\") (tr--board-line a 3) (tr--format-edge a c "/") (tr--board-line c 1)))))))
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
                      (tr-line-string item) "\n")))
           ))))))

(cl-defun tr-selection (&key title items validation on-confirm)
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
    (insert "\n" msg "\n")
    (when (memq type passing-types)
      (insert (buttonize "[confirm selection]"
                         (lambda (_)
                           (apply on-confirm tr-current-selection))))))
  (when (not tr-selection-spec)
    (setq tr-selection-spec
          (list :items items
                :validation validation
                :on-confirm on-confirm))
    (setq tr-current-selection (make-list (length items) nil))))



;;; Display
;; This page deals with composing the entire page.

(defun tr--display-parameters ()
  "Display game parameters."
  (insert (format "Generation: %d   Ocean:%d/9   Temp:%s   O2:%s\n"
                  (tr-game-state-generation tr-game-state)
                  (tr-game-state-param-ocean tr-game-state)
                  (tr-game-state-param-tempurature tr-game-state)
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
                      (or "???"))))
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

(defun tr--display-project-selection (project-ids)
  (insert "Select a Project:\n")
  (dolist (proj-id project-ids)
    (let* ((card (tr-card-by-id proj-id)))
      (insert "   " (button-buttonize "[ ]"
                                      (lambda (proj-id)
                                        (tr-submit-response (car tr-pending-request)
                                                            (list (list 'projects proj-id nil))))
                                      proj-id)
              " "
              (tr-line-string card)
              "\n"))))

(defun tr--display-standard-project-selection (standard-project-ids)
  (insert "Select a Standard Project:\n")
  (let ((project-costs '((power-plant . 11)
                         (asteroid . 14)
                         (aquifer . 18)
                         (greenery . 23)
                         (city . 25)
                         (sell-patents . 0))))
    (dolist (proj-id standard-project-ids)
      (insert "   " (button-buttonize (format "[%2d] %s"
                                              (alist-get proj-id project-costs)
                                              proj-id)
                                      (lambda (proj-id)
                                        (tr-submit-response (car tr-pending-request)
                                                            (list (list 'standard-projects proj-id))))
                                      proj-id)
              "\n"))))

(defun tr--display-extra-selection (extras)
  (insert "Other Actions:\n")
  (dolist (extra extras)
    (pcase extra
      ('pass (insert "   "
                     (button-buttonize "Pass"
                                       (lambda (_)
                                         (tr-submit-response (car tr-pending-request)
                                                             '((extra pass))))
                                       nil))))))

(defun tr--display-standard-action-selection (actions)
  "Display the standard action selection input."
  (insert (format "Action %d/2\n" (1+ tr-action-no)))
  (dolist (action actions)
    (insert "\n")
    (let* ((action-symbol (car action))
           (action-params (cdr action)))
      (pcase action-symbol
        ('projects (tr--display-project-selection action-params))
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
                          (msg (format "%d project(s) selected. Cost: $%d. Remaining:"
                                       proj-count
                                       (* proj-count 3))))
                     (list 'info msg)))))
   :on-confirm (lambda (corp projects)
                 (tr-submit-response
                  tr-active-player
                  (list corp projects)))))

;; (cadr terraform-pending-request)
(defun tr--display-action-selection ()
  "Display the section asking user to make input."
  (let* ((request (cadr terraform-pending-request)))
    (pcase request
      (`(action ,actions)
       (tr--display-standard-action-selection actions))
      (`(select-starting-cards ,corps ,cards)
       (tr--select-starting-cards-selection corps cards)))))

(defconst tr-main-buffer "*terraforming*") ;; TODO: support multiple instances of the game

(defun tr-display-board ()
  "Display the board according to the current request."
  (with-current-buffer (get-buffer-create tr-main-buffer)
    (erase-buffer)
    (tr--display-board)
    (tr--display-parameters)
    (tr--display-player-panel)
    (tr--display-action-selection)))




;;; Game Flow

;; States:
;; start

(defvar tr-pending-request nil)
(defvar tr-active-player nil)
(defvar tr-action-no nil)

(defun tr-!draw-corporations ()
  ""
  (let* ((corp-deck (tr-game-state-corporation-deck tr-game-state))
         (top-two (seq-take corp-deck 2)))
    (setf (tr-game-state-corporation-deck tr-game-state) (seq-drop corp-deck 2))
    top-two))

(defun tr-!draw-cards (n)
  ""
  (let* ((deck (tr-game-state-deck tr-game-state))
         (top (seq-take deck n)))
    (setf (tr-game-state-corporation-deck tr-game-state) (seq-drop deck n))
    top))

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
     (cl-incf (tr-player-money tr-active-player) amount))
    ('money-production
     (cl-incf (tr-player-money-production tr-active-player) amount))
    ('steel
     (cl-incf (tr-player-steel tr-active-player) amount))
    ('steel-production
     (cl-incf (tr-player-steel-production tr-active-player) amount))
    ('titanium
     (cl-incf (tr-player-titanium tr-active-player) amount))
    ('titanium-production
     (cl-incf (tr-player-titanium-production tr-active-player) amount))
    ('plant
     (cl-incf (tr-player-plant tr-active-player) amount))
    ('plant-production
     (cl-incf (tr-player-plant-production tr-active-player) amount))
    ('energy
     (cl-incf (tr-player-energy tr-active-player) amount))
    ('energy-production
     (cl-incf (tr-player-energy-production tr-active-player) amount))
    ('heat
     (cl-incf (tr-player-heat tr-active-player) amount))
    ('heat-production
     (cl-incf (tr-player-heat-production tr-active-player) amount))))

(defun tr-!increase-tempurature (amt)
  (cl-incf (tr-game-state-param-tempurature tr-game-state) amt)
  (cl-incf (tr-player-rating tr-active-player) amt))

(defun tr-!increase-oxygen (amt)
  (cl-incf (tr-game-state-param-oxygen tr-game-state) amt)
  (cl-incf (tr-player-rating tr-active-player) amt))

(defun tr-!place-ocean (location)
  ;; TODO: validate location is ok
  (cl-incf (tr-game-state-param-ocean tr-game-state))
  (cl-incf (tr-player-rating tr-active-player))
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash location board)))
    (puthash location (plist-put tile :top 'ocean) board)))

(defun tr-!place-ocean (location)
  ;; TODO: validate location is ok
  (cl-incf (tr-game-state-param-oxygen tr-game-state))
  (cl-incf (tr-player-rating tr-active-player))
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash location board)))
    (puthash location (plist-put (plist-put tile :top 'greenery) :player (tr-player-id tr-active-player))
             board)))

(defun tr-!place-city (location)
  ;; TODO: validate location is ok
  (let* ((board (tr-gameboard-board (tr-game-state-gameboard tr-game-state)))
         (tile (gethash location board)))
    (puthash location (plist-put (plist-put tile :top 'city) :player (tr-player-id tr-active-player))
             board)))

(defun tr-!run-effect (effect &optional params)
  (unless tr-active-player
    (error "No active player's turn."))
  (seq-do
   (lambda (action)
     (pcase action
       (`(inc-param tempurature ,amount)
        (tr-!increase-tempurature amount))
       (`(inc ,resource ,amount)
        (tr-!increment-user-resource resource amount))
       (`(dec ,resource ,amount)
        (tr-!increment-user-resource resource (- amount)))
       (`(add-ocean)
        (let ((location (pop params)))
          (tr-!place-ocean location)))
       (`(add-greenery)
        (let ((location (pop params)))
          (tr-!place-greenery location)))
       (`(add-city)
        (let ((location (pop params)))
          (tr-!place-city location)))))
   effect))

(defun tr-!run-project (project-id params)
  "Run the effects of a given card."
  (let* ((card (tr-card-by-id project-id))
         (cost (tr-card-cost card))
         (effect (tr-card-effect card)))
    (tr-!increment-user-resource 'money (- cost))
    (tr-!run-effect effect params)
    (tr-!play-in-front project-id)))

;; Turn order ---

(defun tr-all-players-passed-p ()
  (>= (length (tr-game-state-passed-players tr-game-state))
      (length (tr-game-state-players tr-game-state))))

(defun tr-!next-generation ()
  (cl-incf (tr-game-state-generation tr-game-state))
  (setf (tr-game-state-passed-players tr-game-state) '())
  ;; TODO: Generation and card selection
  ;; TODO: Rotate players
  (tr->request tr-active-player (tr-actions-for tr-active-player)
               #'tr-action-performed))

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

(defun tr-?all-players-ready () ;; TODO
  "Return non-nil if all players are ready to start the game."
  t)

(defun tr-?first-player ()
  "Return the player who should go first."
  (car (tr-game-state-players tr-game-state)))

;; Action generation

(defun tr-get-requirement-count (item)
  (pcase item
    ('money (tr-player-money tr-active-player))
    ('money-production (tr-player-money-production tr-active-player))
    ('steel (tr-player-steel tr-active-player))
    ('steel-production (tr-player-steel-production tr-active-player))
    ('titanium (tr-player-titanium tr-active-player))
    ('titanium-production (tr-player-titanium-production tr-active-player))
    ('plant-production (tr-player-plant-production tr-active-player))
    ('plant (tr-player-plant-production tr-active-player))
    ('energy-production (tr-player-energy-production tr-active-player))
    ('energy (tr-player-energy-production tr-active-player))
    ('heat-production (tr-player-heat-production tr-active-player))
    ('heat (tr-player-heat-production tr-active-player))
    ('ocean (tr-game-state-param-ocean tr-game-state))
    ('oxygen (tr-ct-to-oxygen (tr-game-state-param-oxygen tr-game-state)))))

(defun tr-requirements-satisfied-p (requirements)
  (pcase requirements
    (`(> ,resource ,amt)  ;; TODO: add pred to check that resource is symbol
     (> (tr-get-requirement-count resource) amt))
    (`(>= ,resource ,amt)
     (>= (tr-get-requirement-count resource) amt))
    (`(< ,resource ,amt)
     (< (tr-get-requirement-count resource) amt))
    (`(<= ,resource ,amt)
     (<= (tr-get-requirement-count resource) amt))
    (_ t)))

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
                (requirements (tr-card-requirements card)))
            (and (>= money cost)
                 (tr-requirements-satisfied-p requirements))))
        hand)))))

(defun tr-standard-projects-for (player)
  "Return list of all standard projects a player can take."
  (let ((hand (tr-player-hand player))
        (money (tr-player-money player)))
    (seq-filter
     #'identity
     (list (when (>= money 11) 'power-plant)
           (when (>= money 14) 'asteroid)
           (when (>= money 18) 'aquifer)
           (when (>= money 23) 'greenery)
           (when (>= money 25) 'city)
           (when (and hand (> (length hand) 0)) 'sell-patents)))))

(defun tr-actions-for (player)
  "Return description of all actions the player can take."
  `(action ((projects . ,(tr-playable-projects-for player))
            (standard-projects . ,(tr-standard-projects-for player))
            (extra . (pass)))))

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

;; TODO: I don't like how the actions spec is all over the place...
(defun tr-action-performed (player action)
  "Generic PLAYER action of specification ACTION"
  (unless (tr-player-p player)
    (error "Invalid type (terraform-player-p %s)" player))
  (catch 'ignore-action-step
    (pcase action
      (`(projects ,project-id ,params)
       (tr-!run-project project-id params))
      (`(standard-projects power-plant)
       (tr-!run-effect [(dec money 11) (inc energy-production 1)]))
      (`(standard-projects asteroid)
       (tr-!run-effect [(dec money 14) (inc-param tempurature 1)]))
      (`(standard-projects aquifer ,location) ;; TODO change "location" to generic PARAMS
       (tr-!run-effect [(dec money 18) (add-ocean)] (list location)))
      (`(standard-projects greenery ,location) ;; TODO change "location" to generic PARAMS
       (tr-!run-effect [(dec money 23) (add-greenery)] (list location)))
      (`(standard-projects city ,location) ;; TODO change "location" to generic PARAMS
       (tr-!run-effect [(dec money 25) (add-city)] (list location)))
      (`(standard-projects sell-patents ,card-ids) ;; TODO change "cards" to generic PARAMS
       (tr-!discard-cards card-ids)
       (tr-!increment-user-resource 'money (length card-ids)))
      (`(extra pass)
       (tr-!player-pass))
      (_ (error "Invalid action response %s" action)))
    (tr-action-step)))

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
  (error "not implemented"))

(defun tr-run ()
  "Demo command to run game."
  (unless tr-game-state
    (error "No initialized game state"))
  (dolist (player (tr-game-state-players tr-game-state))
    (let ((corps (tr-!draw-corporations))
          (cards (tr-!draw-cards 10)))
      (tr->request player
                   `(select-starting-cards ,corps ,cards)
                   #'tr-!corporation-selected))))

;;; TODO LIST
;; Card to string
;; Basic selection process to updating the state.

;;; DEMO SETUP

(terraform-run)

(cadr terraform-pending-request)

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
