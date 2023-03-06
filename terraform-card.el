;;; terraform-card.el --- Card Definitions for Terraforming Mars -*- lexical-binding: t -*-

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

;; This package contains card definitions for the game.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CARD EFFECTS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar terraform-active-card)
(defvar terraform-active-user)
(declare-function terraform-effect-to-string "terraform.el")
(declare-function terraform-resource-type-to-string "terraform.el")
(declare-function terraform-card-resource-count "terraform.el")
(declare-function terraform-!increment-user-resource "terraform.el")
(declare-function terraform--char->decrease-any "terraform.el")
(declare-function terraform--count-thing "terraform.el")
(declare-function terraform-!increase-tempurature "terraform.el")
(declare-function terraform-!place-ocean "terraform.el")
(declare-function terraform-!place-greenery "terraform.el")
(declare-function terraform-!place-city "terraform.el")
(defvar terraform--titanium-char)
(defvar terraform--steel-char)
(defvar terraform--money-char)
(defvar terraform--action-arrow)

(defconst terraform-card-effect-registry nil)

(defun terraform-resource-type-to-string (type)
  (pcase type
    ('money-production (terraform--char->prod terraform--money-char))
    ('money terraform--money-char)
    ('plant-production (terraform--char->prod terraform--plant-char))
    ('plant terraform--plant-char)
    ('steel-production (terraform--char->prod terraform--steel-char))
    ('steel terraform--steel-char)
    ('titanium-production (terraform--char->prod terraform--titanium-char))
    ('titanium terraform--titanium-char)
    ('energy-production (terraform--char->prod terraform--energy-char))
    ('energy terraform--energy-char)
    ('heat-production (terraform--char->prod terraform--heat-char))
    ('heat terraform--heat-char)
    ('card terraform--card-char)
    ('every-city (concat (terraform--char->decrease-any "[") terraform--city-tag
                         (terraform--char->decrease-any "]")))
    ('microbe "🦠")
    ('animal "🐾")
    ('any-animal (concat
                  (terraform--char->decrease-any "[")
                  "🐾"
                  (terraform--char->decrease-any "]")))
    ('rating "TR")
    (_ "?")))

(defun terraform-card-effect-by-id (id)
  (seq-find
   (lambda (effect)
     (eql (car effect) id))
   terraform-card-effect-registry))

(defun terraform-card-effect-lighter (effect)
  "Return the lighter element of registered EFFECT."
  (nth 3 effect))
(defun terraform-card-extra-action (effect)
  "Return the extra-action element of registered EFFECT."
  (nth 2 effect))
(defun terraform-card-effect-action (effect)
  "Return the action element of registered EFFECT."
  (nth 4 effect))
(defun terraform-card-effect-requirement (effect)
  "Return the requirement of registered EFFECT."
  (nth 5 effect))
(defun terraform-card-immediate-action (effect)
  "Return the requirement of registered EFFECT."
  (nth 6 effect))

(defun terraform-card-register-effect (effect-name param-list extra-action lighter-fn effect-fn requirement immediate-action)
  (add-to-list 'terraform-card-effect-registry (list effect-name param-list extra-action lighter-fn effect-fn requirement immediate-action)
               t #'equal))

(defmacro terraform-card-def-effect (name params &rest body)
  (declare (indent 2))
  (let* ((keyw)
         (lighter)
         (extra-action)
         (immediate-action)
         (requirement))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:lighter (setq lighter (purecopy (pop body))))
        (:extra-action (setq extra-action (pop body)))
        (:immediate-action (setq immediate-action (purecopy (pop body))))
        (:requirement (setq requirement (purecopy (pop body))))))
    `(terraform-card-register-effect
      ',name
      ',params
      ,(when extra-action
           `(lambda ,params
             ,extra-action))
      (lambda ,params ,lighter)
      (lambda ,params
        ,@body)
      ,(when requirement
         `(lambda ,params ,requirement))
      ,(when immediate-action
         `(lambda ,params
            ,immediate-action)))))

;; TODO Add documentation string for card-def-effect

(terraform-card-def-effect inc (resource amt)
  :lighter (format "+%d%s" amt (terraform-resource-type-to-string resource))
  (terraform-!increment-user-resource resource amt))

(terraform-card-def-effect add (resource amt)
  :lighter (format "+%d%s" amt (terraform-resource-type-to-string resource))
  (setf (terraform-card-resource-count terraform-active-card)
        (+ (or (terraform-card-resource-count terraform-active-card) 0) amt)))

(terraform-card-def-effect inc-per (resource by)
  :lighter (format "+%s/%s"
             (terraform-resource-type-to-string resource)
             (terraform-resource-type-to-string by))
  (let* ((amt (terraform--count-thing by)))
    (terraform-!increment-user-resource resource amt)))

(terraform-card-def-effect buy (resource amt)
  :lighter (let* ((purchase-symbol (pcase resource
                                     ('titanium terraform--titanium-char)
                                     ('steel terraform--steel-char))))
             (format "-%d%s(%s)" amt terraform--money-char purchase-symbol))
  :immediate-action (let* ((player-resources (terraform-get-requirement-count resource))
                           (sell-amount (terraform-get-player-resource-sell-amount terraform-active-player resource))
                           (max-buy-ct (min (ceiling (/ (float amt) sell-amount)) player-resources))
                           (selected (read-number (format "Amount of %s to sell (0-%d):"
                                                          (symbol-name resource)
                                                          max-buy-ct))))
                      (when (or (not (integerp selected))
                                (< selected 0)
                                (< max-buy-ct selected))
                        (user-error "invalid selection %s %s %s %s" selected (not (integerp selected))
                                (< amt 0)
                                (< max-buy-ct amt)))
                      selected)
  (lambda (sell-amt)
    (let* ((sell-total (* (terraform-get-player-resource-sell-amount terraform-active-player resource)
                          sell-amt))
           (new-price (- amt sell-total)))
      (terraform-!increment-user-resource 'money (- new-price))
      (terraform-!increment-user-resource resource (- sell-amt)))))

(terraform-card-def-effect dec (resource amt)
  :lighter (format "-%d%s" amt (terraform-resource-type-to-string resource))
  :requirement (if (eql resource 'money-production)
                   (>= (- (terraform-get-requirement-count resource) amt) -5) ;; TODO: parameterize this number
                 (>= (- (terraform-get-requirement-count resource) amt) 0))
  (terraform-!increment-user-resource resource (- amt)))

(terraform-card-def-effect dec-other (resource amt)
  :lighter (format "-%d%s" amt (terraform--char->decrease-any (terraform-resource-type-to-string resource)))
  :requirement (or (not (terraform--production-resource-p resource))
                   (> (length (terraform--get-other-options resource amt)) 0))
  :extra-action (let* ((title-string (format "Select user to decrease %d%s"
                                             amt
                                             (terraform-resource-type-to-string resource)))
                       (options (seq-map
                                 (lambda (option)
                                   (pcase option
                                     (`(,player ,card ,amt)
                                      (format "From %s(%s) on card %s remove %d"
                                              (terraform-corporation-name (terraform-player-corp-card player))
                                              (terraform-player-id player)
                                              (terraform-card-name card)
                                              amt))
                                     (`(,player ,amt)
                                      (format "From %s(%s) remove %d"
                                              (terraform-corporation-name (terraform-player-corp-card player))
                                              (terraform-player-id player)
                                              amt)))
                                   (cons option (format "{%s}" option)))
                                 (terraform--get-other-options resource amt)))
                       (options (if (not (terraform--production-resource-p resource))
                                    (cons (cons 'skip "Skip")
                                          options)
                                  options)))
                  `(selection
                    :title ,title-string
                    :items [(selection :title "Choose One"
                                       :items ,options
                                       :type one)]
                    :validation (lambda (_) '(info ""))
                    :on-confirm (lambda (selection)  ;; TODO - probaly can do w/o player arg
                                  (tr--process-arg-selection selection))))
  (lambda (selection)
    (pcase selection
      (`(skip . ,_))
      (`((,player ,card ,amt) . ,_)
       (cl-decf (terraform-card-resource-count card) amt))
      (`((,player ,amt) . ,_)
       (message "decreasing player by %d" amt)
       (terraform-!increment-user-resource resource (- amt)))
      (_ (error "option not recognized %s" selection)))))

(terraform-card-def-effect inc-tempurature ()
  :lighter "+℃"
  (when (< (terraform-game-state-param-tempurature terraform-game-state) 19)
    (terraform-!increment-user-resource 'rating 1)
    (terraform-!increase-tempurature 1)))

(terraform-card-def-effect inc-oxygen ()
  :lighter "+℃"
  (when (< (terraform-game-state-param-oxygen terraform-game-state) 14)
    (terraform-!increment-user-resource 'rating 1)
    (terraform-!increase-tempurature 1)))

(terraform-card-def-effect add-ocean ()
  :lighter "+🌊"
  :extra-action 'empty-ocean
  (lambda (param)
    (let ((location param))
      (terraform-!increment-user-resource 'rating 1)
      (terraform-!place-ocean location))))

(terraform-card-def-effect add-greenery ()
  :lighter "+🌳"
  :extra-action 'standard-greenery-placement
  (lambda (param)
    (let ((location param))
      (terraform-!place-greenery location))))

(terraform-card-def-effect add-city ()
  :lighter "+🏙️"
  :extra-action 'standard-city-placement ;; TODO - rename :extra-action to extra-input
  (lambda (param)
    (let ((location param))
      (terraform-!place-city location))))

(terraform-card-def-effect add-noctis-city ()
  :lighter "+🏙️*"
  (let ((location (terraform--find-named-tile 'noctus)))
    (terraform-!place-city location)))

(terraform-card-def-effect add-non-adjacent-city ()
  :lighter "+🏙️*")

(terraform-card-def-effect add-phobos-space-haven-city ()
  :lighter "+🏙️*"
  nil)

(terraform-card-def-effect action (disp lambda)
  :lighter disp
  (funcall action-lambda terraform-active-card terraform-active-player))

(terraform-card-def-effect -> (cost action) ;; TODO not really an effect, document this
  :lighter (format "%s%s%s"
                   (terraform-effect-to-string cost)
                   terraform--action-arrow
                   (terraform-effect-to-string action)))

(terraform-card-def-effect draw-cards-keep-some (ct buy)
  :lighter (if (= 1 ct)
               "Look at top card, buy or discard"
             (format "Look at top %d cards, buy or discard" ct))
  (let ((projects (terraform-!draw-cards ct)))
    (setq terraform-interstitial-action
          `(selection
            :title "Optionally Purchase Projects"
            :items [(selection :title "Projects"
                               :items ,projects
                               :type multiple)]
            :validation (lambda (projects)
                          (let ((cost (* 3 (length projects))))
                            (cond
                             ((> cost (terraform-player-money ,terraform-active-player))
                              '(error "Not enough money"))
                             (t
                              (let* ((proj-count (length projects))
                                     (msg (format "%d project(s) selected. Cost: $%d" proj-count (* proj-count 3))))
                                (list 'info msg))))))
            :on-confirm (lambda (projects)
                          (terraform-add-projects-to-players-hand ,terraform-active-player projects)
                          (terraform-submit-response
                           terraform-active-player
                           nil))))))

(terraform-card-def-effect add-special (id)
  :lighter (format "+%s" (symbol-name id))
  :extra-action 'standard-greenery-placement
  (lambda (param)
    (let ((location param))
      (terraform-!place-special location id))))

(terraform-card-def-effect add-modifier (descr flag)
  :lighter descr)

(terraform-card-def-effect or (&rest clauses)
  :lighter (string-join
            (seq-map
             #'terraform-effect-to-string
             clauses)
            "|"))

;; TODO: This is handled elsewhere and is not needed...
(terraform-card-def-effect on (event handler)
  :lighter
  (progn
    (message ">>> %s" handler)
    (format "%s:%s" event (string-join (seq-map #'terraform-effect-to-string handler) "; ")))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CARDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar terraform-card-directory (make-hash-table :test 'equal))
(defvar terraform-card-corporation-directory (make-hash-table :test 'equal))

(defconst terraform-card-tags
  '(building space science power earth jovian venus plant microbe animal city wild))
(defconst terraform-card-types
  '(automated event active))

(cl-defun terraform-corporation-def (name &key number tags effect continuous-effect)
  (declare (indent defun))
  (unless (stringp name)
    (error "Invalid corporation: name wrong type stringp: %s" name))
  (unless number
    (error "Invalid corporation: number must exist"))
  (when (and tags (not (seq-every-p (lambda (tag) (member tag terraform-card-tags)) tags)))
    (error "Invalid corporation: tags must be valid list of tags: %s" tags))
  (puthash number
           `(:number ,number
             :name ,name
             :tags ,tags
             :effect ,effect
             :continuous-effect ,continuous-effect)
           terraform-card-corporation-directory))

(terraform-corporation-def "Credicor"
 :number 1
 :tags nil
 :effect [(inc money 57)]
 :continuous-effect '(on (spend 20) [(inc money 4)]))
(terraform-corporation-def "Ecoline"
 :number 2
 :tags '(plant)
 :effect [(inc plant-production 2) (inc money 36) (inc plant 3)]
 :continuous-effect '(add-modifier "7☘→🌳" (set-greenery-cost 7)))
(terraform-corporation-def "Helion"
 :number 3
 :tags '(space)
 :effect [(inc heat-production 3) (inc money 42)]
 :continuous-effect '(add-modifier "x≋=x$" heat-as-money))
(terraform-corporation-def  "Mining Guild"
 :number 4
 :tags '(building building)
 :effect [(inc money 30) (inc steel 5) (inc steel-production 1)]
 :continuous-effect '(add-modifier "⚒*/**:[⚒]" tile-placement-gain-steel-prod))

(cl-defun terraform-card-def (name &key number cost tags type victory-points requirements effect action
                                   continuous-effect)
  (declare (indent defun))
  (when (not number)
    (error "Invalid card: number must be defined: %s" number))
  (unless type
    (error "Invalid card : type must exist"))
  (unless (stringp name)
    (error "Invalid card: name wrong type stringp: %s" name))
  (unless (integerp cost)
    (error "Invalid card: cost wrong type integerp: %s" cost))
  (unless (seq-every-p (lambda (tag) (member tag terraform-card-tags)) tags)
    (error "Invalid card: invalid tags: %s" tags))
  (unless (or (functionp victory-points) (integerp victory-points) (listp victory-points) (not victory-points))
    (error "Invalid card: victory-points not function or integer: %s" victory-points))
  ;; TODO: validate effect, action, and continuous-effect.
  (puthash number
           `(:name ,name
             :number ,number
             :cost ,cost
             :tags ,tags
             :type ,type
             :victory-points ,victory-points
             :requirements ,requirements
             :effect ,effect
             :action ,action
             :continuous-effect ,continuous-effect
             :used nil)
           terraform-card-directory))

(terraform-card-def "Colonizer Training Camp"
  :number 1
  :cost 8
  :tags '(jovian building)
  :victory-points 2
  :type 'automated)

(terraform-card-def "Asteroid Mining Consortium"
  :number 2
  :cost 13
  :tags '(jovian)
  :victory-points 1
  :requirements '(> titanium-production 0)
  :type 'automated
  :effect [(dec-other titanium-production 1) (inc titanium-production 1)])

(terraform-card-def "Deep Well Heating"
  :number 3
  :cost 13
  :type 'automated
  :tags '(power building)
  :effect [(inc-tempurature) (inc energy-production 1)])

(terraform-card-def "Cloud Seeding"
  :number 4
  :cost 11
  :type 'automated
  :requirements '(>= ocean 3)
  :effect [(dec money-production 1) (inc heat-production 1) (inc plant-production 2)])

(terraform-card-def "Search For Life"
  :number 5
  :cost 13
  :type 'active
  :tags '(science)
  ;; TODO :victory-points '(* resource 3)
  :requirements '(<= oxygen 6)
  :type 'active
  :action '[(-> (dec money 1)
                (action "🦠*:SCI"
                        (lambda (this player)
                          (let ((card (car (terraform-!draw-cards 1))))
                            (when (member 'microbe (terraform-card-tags card))
                              (cl-incf (terraform-card-resource-count this)))))))])

(terraform-card-def "Inventors' Guild"
  :number 6
  :cost 9
  :type 'active
  :tags '(science)
  :action '[(-> nil
                (draw-cards-keep-some 1 t))])

(terraform-card-def "Martian Rails"
  :number 7
  :cost 13
  :type 'active
  :tags '(building)
  :action '[(-> (dec energy 1)
                (inc-per money every-city))])

(terraform-card-def "Capital"
  :number 8
  :cost 26
  :type 'automated
  :tags '(city building)
  :requirements '(>= ocean 4)
  :effect [(dec energy-production 2) (inc money-production 5)]
  :victory-points
  (lambda (this)
    (let ((tile (terraform-get-tile-by-name 'capital)))
      (when tile
        (length (seq-filter
                 (lambda (tile) (eql (plist-get tile :top) 'ocean))
                 (terraform-get-adjacent-tiles)))))))

(terraform-card-def "Asteroid"
  :number 9
  :cost 14
  :type 'event
  :tags '(space)
  :effect [(inc-tempurature) (inc titanium 5) (dec-other plant 3)])


(terraform-card-def "Comet"
  :number 10
  :cost 21
  :type 'event
  :tags '(space)
  :effect [(inc-tempurature) (add-ocean) (dec-other plant 3)])

(terraform-card-def "Big Asteroid"
  :number 11
  :cost 27
  :type 'event
  :tags '(space)
  :effect [(inc-tempurature) (inc-tempurature) (inc titanium 4) (dec-other plant 4)])


(terraform-card-def "Water Import From Europa"
  :number 12
  :cost 25
  :type 'active
  :tags '(space jovian)
  :action '[(-> (buy titanium 12)
                (add-ocean))]
  :victory-points (lambda () (error "not implemented")))
(terraform-card-def "Space Elevator"
  :number 13
  :cost  27
  :type 'active
  :tags '(space building)
  :action '[(-> (dec steel 1)
                (inc money 5))]
  :effect [(inc titanium-production 1)]
  :victory-points 2)
(terraform-card-def "Development Center"
  :number 14
  :cost 11
  :tags '(science building)
  :type 'active
  :action '[(-> (dec energy 1)
                (inc card 1))])
(terraform-card-def "Equatorial Magnetizer"
  :number 15
  :tags '(building)
  :type 'active
  :cost 11
  :action '[(-> (dec energy-production 1)
                (inc rating 1))])
(terraform-card-def "Domed Crater"
  :number 16
  :tags '(city building)
  :type 'automated
  :cost 24
  :requirements `(<= oxygen 7)
  :effect [(dec energy-production 1)
           (inc money-production 3)
           (add-city)
           (inc plant 3)])
(terraform-card-def "Noctis City"
  :number 17
  :cost 18
  :type 'automated
  :tags '(city building)
  :effect [(dec energy-production 1)
           (inc money-production 3)
           (add-noctis-city)])

(terraform-card-def "Methane From Titan"
  :number 18
  :cost 28
  :requirements '(>= oxygen 2)
  :type 'automated
  :tags '(jovian space)
  :effect [(inc heat-production 2)
           (inc plant-production 2)]
  :victory-points 2)

(terraform-card-def "Imported Hydrogen"
  :number 19
  :cost 16
  :requirements nil
  :type 'event
  :tags '(earth space)
  :effect [(or ((inc plant 3) (add microbe 3) (add animal 2)))
           (add-ocean)])

(terraform-card-def "Research Outpost"
  :number 20
  :cost 18
  :requirements nil
  :type 'active
  :tags '(science city building)
  :effect [(add-non-adjacent-city)]
  :continuous-effect '(add-modifier ":-1$" (reduce-project-cost 1)))

(terraform-card-def "Phobos Space Haven"
  :number 21
  :cost 25
  :requirements nil
  :type 'automated
  :tags '(city space)
  :effect [(inc titanium-production 1)
           (add-phobos-space-haven-city)]
  :victory-points 3)

(terraform-card-def "Black Polar Dust"
  :number 22
  :cost 15
  :requirements  nil
  :type 'automated
  :tags '()
  :effect [(dec money-production 2) (inc heat-production 3) (add-ocean)])

(terraform-card-def "Arctic Algae"
  :number 23
  :cost 12
  :type 'active
  :requirements '(<= tempurature -12)
  :tags '(plant)
  :effect [(inc plant 1)]
  :continuous-effect '(on any-ocean [(inc plant 2)]))

(terraform-card-def "Predators"
  :number 24
  :cost 14
  :requirements  '(>= oxygen 11)
  :type 'active
  :tags '(animal)
  :action [(-> (dec-other any-animal 1)
               (add animal 1))]
  :victory-points
  (lambda (this) (terraform-card-resource-count this)))

(terraform-card-def "Space Station"
  :number 25
  :cost 10
  :requirements nil
  :type 'active
  :tags '(space)
  :continuous-effect `(card-discount space 2)
  :victory-points 1)

(terraform-card-def "EOS Chasma National Park"
  :number 26
  :cost 16
  :requirements '(>= tempurature -12)
  :type 'automated
  :tags '(plant building)
  :effect [(add animal 1)
           (inc plant 3)
           (inc money-production 2)]
  :victory-points 1)

(terraform-card-def "Interstellar Colony Ship"
  :number 27
  :cost 24
  :requirements '(>= science-tag 5)
  :type 'event
  :tags '(earth space)
  :victory-points 4)

(terraform-card-def "Security Fleet"
  :number 28
  :cost 12
  :type 'active
  :tags '(space)
  :action [(-> (dec titanium 1)
               (add fighter 1))]
  :victory-points '(per-resource 1 1))

(terraform-card-def "Optimal Aerobraking"
  :number 31
  :cost 7
  :type 'active
  :tags '(space)
  :continuous-effect '(on (and ((tags (space)) event))
                          [(inc money 3)
                           (inc heat 3)]))

(terraform-card-def "Regolith Eaters"
  :number 33
  :cost 13
  :type 'active
  :tags '(science microbe)
  :action '[(-> nil (add microbe 1))
            (-> (dec microbe 2) (inc-oxygen))])

(terraform-card-def "GHG Producing Bacteria"
  :number 34
  :cost 8
  :type 'active
  :requirements '(>= oxygen 4)
  :tags '(science microbe)
  :action '[(-> nil (add microbe 1))
            (-> (dec microbe 2) (inc-tempurature))])

(terraform-card-def "Ants"
  :number 35
  :cost 9
  :type 'active
  :requirements '(>= oxygen 4)
  :tags '(microbe)
  :action '[(-> (dec-other microbe 1) (add microbe 1))])

(terraform-card-def "Release of Inert Gasses"
  :number 36
  :cost 14
  :type 'event
  :effect [(inc rating 2)])

(terraform-card-def "Nitrogen-Rich Asteroid"
  :number 37
  :cost 31
  :type 'event
  :requirements nil
  :tags '(space)
  :effect [(inc plant-production 1)
           (when (>= (tags plant) 3) ;; TODO: 
             (inc plant-production 3))
           (inc rating 2)
           (inc-tempurature)])

(terraform-card-def "Rover Construction"
  :number 38
  :cost 8
  :type 'active
  :tags '(building)
  :continuous-effect '(on any-city [(inc money 2)]))

(terraform-card-def "Deimos Down"
  :number 39
  :cost 31
  :type 'event
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc-tempurature) (inc-tempurature) (inc-tempurature)
           (inc steel 4) (dec-other plant 8)]
  :victory-points nil)

(terraform-card-def "Asteroid Mining"
  :number 40
  :cost 30
  :type 'automated
  :requirements nil
  :tags '(jovian space)
  :action nil
  :effect [(inc titanium-production 2)]
  :victory-points 2)

(terraform-card-def "Food Factory"
  :number 41
  :cost 12
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec plant-production 1) (inc money-production 4)]
  :victory-points 1)

(terraform-card-def "Archaebacteria"
  :number 42
  :cost 6
  :type 'automated
  :requirements '(<= tempurature -18)
  :tags '(microbe)
  :action nil
  :effect [(inc plant-production 1)]
  :victory-points nil)

(terraform-card-def "Carbonate Processing"
  :number 43
  :cost 6
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 1) (inc heat-production 3)]
  :victory-points nil)

(terraform-card-def "Natural Preserve"
  :number 44
  :cost 9
  :type 'automated
  :requirements '(<= oxygen 4)
  :tags '(science building)
  :action nil
  :effect [(inc money-production 1) (add-special natural-preserve)]
  :victory-points 1)

(terraform-card-def "Nuclear Power"
  :number 45
  :cost 10
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(dec money-production 2) (inc energy-production 3)]
  :victory-points nil)

(terraform-card-def "Lightning Harvest"
  :number 46
  :cost 8
  :type 'automated
  :requirements '(>= (tags (science)) 3) ;; TODO: implement tags requirements
  :tags '(power)
  :action nil
  :effect [(inc energy-production 1) (inc money-production 1)]
  :victory-points 1)

(terraform-card-def "Algae"
  :number 47
  :cost 10
  :type 'automated
  :requirements '(>= ocean 5)
  :tags '(plant)
  :action nil
  :effect [(inc plant 1) (inc plant-production 2)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Adapted Lichen"
  :number 48
  :cost 9
  :type 'automated
  :requirements nil
  :tags '(plant)
  :action nil
  :effect [(inc plant-production 1)]
  :continuous-effect nil
  :victory-points 9)

(terraform-card-def "Tardigrades"
  :number 49
  :cost 4
  :type 'active
  :tags '(microbe)
  :action '[(-> nil (add microbe 1))]
  :victory-points '(per-resource 1 4))

(terraform-card-def "Virus"
  :number 50
  :cost 1
  :type 'event
  :requirements nil
  :tags '(microbe)
  :action nil
  :effect [(or ((dec-other animal 2) (dec-other plant 5)))]) ;; TODO implement or

(terraform-card-def "Miranda Resort"
  :number 51
  :cost 12
  :type 'automated
  :requirements nil
  :tags '(jovian space)
  :action nil
  :effect [(inc-per money-production (tags earth))] ;; TODO implement inc-per with tags
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Fish"
  :number 52
  :cost 9
  :type 'active
  :requirements '(>= tempurature 2)
  :tags '(animal)
  :action '[(-> nil (add animal 1))]
  :effect [(dec-other plant-production 1)]
  :victory-points '(per-resource 1 1))

(terraform-card-def "Lake Marineris"
  :number 53
  :cost 18
  :type 'automated
  :requirements '(>= tempurature 0)
  :tags '()
  :action nil
  :effect [(add-ocean) (add-ocean)]
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Small Animals"
  :number 54
  :cost 6
  :type 'active
  :requirements '(>= oxygen 6)
  :tags '(animal)
  :action '[(-> nil (add animal 1))]
  :effect [(dec-other plant-production 1)]
  :victory-points '(per-resource 1 2))

(terraform-card-def "Kelp Farming"
  :number 55
  :cost 17
  :type 'automated
  :requirements '(>= ocean 6)
  :tags '(plant)
  :action nil
  :effect [(inc money-production 2) (inc plant-production 3) (inc plant 2)]
  :victory-points 1)

(terraform-card-def "Mine"
  :number 56
  :cost 4
  :type 'automated
  :requirements 
  :tags '(building)
  :effect [(inc steel-production 1)])

(terraform-card-def "Vesta Shipyard"
  :number 57
  :cost 15
  :type 'automated
  :requirements nil
  :tags '(jovian space)
  :action nil
  :effect [(inc titanium-production 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Beam From a Thorium Asteroid"
  :number 58
  :cost 32
  :type 'automated
  :requirements '(>= (tags (jovian)) 1)
  :tags '(jovian space power)
  :effect [(inc heat-production 3) (inc energy-production 3)]
  :victory-points 1)

(terraform-card-def "Mangrove"
  :number 59
  :cost 12
  :type 'automated
  :requirements '(>= tempurature 4)
  :tags '(plant)
  :effect [(add-greenery)]
  :victory-points 1)

(terraform-card-def "Trees"
  :number 60
  :cost 13
  :type 'automated
  :requirements '(>= tempurature -4)
  :tags '(plant)
  :action nil
  :effect [(inc plant-production 3) (inc plant 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Great Escarpment Consortium"
  :number 61
  :cost 6
  :type 'automated
  :requirements '(>= steel-production 1)
  :tags '()
  :effect [(dec-other steel-production 1) (inc steel-production 1)])

(terraform-card-def "Mineral Deposit"
  :number 62
  :cost 5
  :type 'event
  :tags '()
  :effect [(inc steel 5)])

(terraform-card-def "Mining Expedition"
  :number 63
  :cost 12
  :type 'automated
  :requirements nil
  :tags '()
  :action nil
  :effect [(inc-oxygen) (inc steel 2) (dec-other plant 2)])

(terraform-card-def "Mining Area"
  :number 64
  :cost 4
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(add-mining mining-area)] ;; TODO needs special logic
  )

(terraform-card-def "Building Industries"
  :number 65
  :cost 6
  :type 'automated
  :tags '(building)
  :effect [(dec energy-production 1) (inc steel-production 2)])

(terraform-card-def "Land Claim"
  :number 66
  :cost 1
  :type 'event
  :tags '()
  :effect [(add-land-claim)] ;; TODO needs special logic
  )

(terraform-card-def "Mining Rights"
  :number 67
  :cost 9
  :type 'automated
  :tags '(building)
  :effect [(add-mining mining-rights)])

(terraform-card-def "Sponsors"
  :number 68
  :cost 6
  :type 'automated
  :tags '(earth)
  :effect [(inc money-production 2)])

(terraform-card-def "Electro Catapult"
  :number 69
  :cost 17
  :type 'active
  :requirements '(<= oxygen 8)
  :tags '(building)
  :action '[(-> (dec plant 1) (inc money 7))
            (-> (dec steel 1) (inc money 7))]
  :effect [(dec energy-production 1)]
  :victory-points 1)

(terraform-card-def "Earth Catapult"
  :number 70
  :cost 23
  :type 'active
  :tags '(earth)
  :continuous-effect '(card-discount nil 2)
  :victory-points 2)

(terraform-card-def "Advanced Alloys"
  :number 71
  :cost 9
  :type 'active
  :tags '(science)
  :continuous-effect '(resource-enrich steel+titanium 1))

(terraform-card-def "Birds"
  :number 72
  :cost 10
  :type 'active
  :requirements '(>= oxygen 13)
  :tags '(animal)
  :action [(-> nil (add animal 1))]
  :effect [(dec-other plant-production 2)]
  :victory-points '(per-resource 1 1))

(terraform-card-def "Mars University"
  :number 73
  :cost 8
  :type 'active
  :requirements nil
  :tags '(science building)
  :action nil
  :effect nil
  :continuous-effect '(on (tags (science)) [(swap-card)])
  :victory-points 1)

(terraform-card-def "Viral Enhancers"
  :number 74
  :cost 9
  :type 'active
  :tags '(science microbe)
  :continuous-effect '(on (tags (plant microbe animal)) [(or ((inc plant 1) (add _ 1)))]) ;; TODO requires tricky logic...
  )

(terraform-card-def "Towing a Comet"
  :number 75
  :cost 23
  :type 'event
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc-oxygen) (add-ocean) (inc plant 2)])

(terraform-card-def "Space Mirrors"
  :number 76
  :cost 3
  :type 'active
  :requirements nil
  :tags '(power space)
  :action [(-> (dec money 7) (inc power 1))])

(terraform-card-def "Solar Wind Power"
  :number 77
  :cost 11
  :type 'automated
  :tags '(science space power)
  :effect [(inc energy-production 1) (inc titanium 2)])

(terraform-card-def "Ice Asteroid"
  :number 78
  :cost 23
  :type 'event
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(add-ocean) (add-ocean)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Quantum Extractor"
  :number 79
  :cost 13
  :type 'active
  :requirements '(>= (tags (science)) 4)
  :tags '()
  :action 
  :effect 
  :continuous-effect 
  :victory-points )

(terraform-card-def "Giant Ice Asteroid"
  :number 80
  :cost 36
  :type 'event
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc-tempurature) (inc-tempurature) (add-ocean) (add-ocean) (dec-other plant 6)] ;; TODO unable to select same ocean twice
  )

(terraform-card-def "Ganymede Colony"
  :number 81
  :cost 20
  :type 'automated
  :requirements nil
  :tags '(jovian)
  :action nil
  :effect [(add-city-noplace)] ;; TODO add this effect
  :victory-points '(per-tag jovian 1 1)) ;; TODO implement this

(terraform-card-def "Callisto Penal Mines"
  :number 82
  :cost 24
  :type 'automated
  :tags '(jovian space)
  :effect [(inc money-production 3)]
  :victory-points 2)

(terraform-card-def "Giant Space Mirror"
  :number 83
  :cost 17
  :type 'automated
  :requirements nil
  :tags '(power space)
  :action nil
  :effect [(inc energy-production 3)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Trans-Neptune Probe"
  :number 84
  :cost 6
  :type 'automated
  :requirements nil
  :tags '(science space)
  :action nil
  :victory-points 1)

(terraform-card-def "Commercial District"
  :number 85
  :cost 16
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(add-special commercial-district)]
  :continuous-effect nil
  :victory-points '(per-city 1 1))

((terraform-card-def "Robotic Workforce"
   :number 86
   :cost 9
   :type 'automated
   :requirements nil
   :tags '(science)
   :action nil
   :effect [(duplicate-automated (tags building))])) ;; TODO Implement this

(terraform-card-def "Grass"
  :number 87
  :cost 11
  :type 'automated
  :requirements '(>= tempurature 16)
  :tags '(plant)
  :effect [(inc plant-production 1) (inc plant 3)])

(terraform-card-def "Heather"
  :number 88
  :cost 6
  :type 'automated
  :requirements '(>= tempurature -14)
  :tags '()
  :action nil
  :effect [(inc plant-production 1) (inc plant 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Peroxide Power"
  :number 89
  :cost 7
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(dec money-production 1) (inc energy-production 2)])

(terraform-card-def "Research"
  :number 90
  :cost 11
  :type 'automated
  :requirements nil
  :tags '(science science) ;; TODO Does this work for other counts?
  :action nil
  :effect [(inc card 2)]
  :victory-points 1)

(terraform-card-def "Gene Repair"
  :number 91
  :cost 12
  :type 'automated
  :requirements '(>= (tags (science)) 3)
  :tags '(science)
  :action nil
  :effect (inc money-production 2)
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "IO Mining Industries"
  :number 92
  :cost 41
  :type 'automated
  :requirements nil
  :tags '(jovian space)
  :action nil
  :effect [(inc titanium-production 2) (inc money-production 2)]
  :continuous-effect nil
  :victory-points '(per-tag jovian 1 1))

(terraform-card-def "Bushes"
  :number 93
  :cost 10
  :type 'automated
  :requirements '(>= tempurature -10)
  :tags '(plant)
  :action nil
  :effect [(inc plant-production 2) (inc plant 2)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Mass Converter"
  :number 94
  :cost 8
  :type 'automated
  :requirements '(>= (tags (science)) 5)
  :tags '(science power)
  :action nil
  :effect [(inc energy-production 6)]
  :continuous-effect '(card-discount space 2))

(terraform-card-def "Physics Complex"
  :number 95
  :cost 12
  :type 'automated
  :requirements nil
  :tags '(science power)
  :action [(-> (dec energy 6) (add science 1))]
  :effect nil
  :continuous-effect nil
  :victory-points '(per-resource 2 1))

(terraform-card-def "Greenhouse"
  :number 96
  :cost 6
  :type 'automated
  :requirements nil
  :tags '(plant building)
  :action nil
  :effect [(inc-per plant every-city)])

(terraform-card-def "Nuclear Zone"
  :number 97
  :cost 10
  :type 'automated
  :tags '(earth)
  :effect [(add-special nuclear-zone) (inc-tempurature) (inc-tempurature)]
  :victory-points -2)

(terraform-card-def "Tropical Resort"
  :number 98
  :cost 13
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec heat-production 2) (inc money-production 3)]
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Toll Station"
  :number 99
  :cost 12
  :type 'automated
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc-per money (opponents-tag space))] ;; TODO Implement this
  )

(terraform-card-def "Fueled Generators"
  :number 100
  :cost 1
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(dec money-production 1) (inc energy-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Ironworks"
  :number 101
  :cost 11
  :type 'automated
  :requirements nil
  :tags '()
  :action [(-> (dec energy 4) [(inc steel 1) (inc-oxygen)])] ;; TODO do vectors work here?
  :effect nil
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Power Grid"
  :number 102
  :cost 18
  :type 'automated
  :tags '(power)
  :effect [(inc-per energy-production (tags power))] ;; TODO make sure that this works
  )

(terraform-card-def "Steelworks"
  :number 103
  :cost 15
  :type 'automated
  :tags '(building)
  :action [(-> (dec energy 4) [(inc steel 2) (inc-oxygen)])])

(terraform-card-def "Ore Processor"
  :number 104
  :cost 13
  :type 'automated
  :requirements nil
  :tags '(building)
  :action [(-> (dec energy 4) [(inc titanium 1) (inc-oxygen)])])

(terraform-card-def "Earth Office"
  :number 105
  :cost 1
  :type 'active
  :tags '(earth)
  :continuous-effect '(add-modifier "🌎:-3$" (card-discount earth 3)))

(terraform-card-def "Acquired Company"
  :number 106
  :cost 10
  :type 'automated
  :requirements nil
  :tags '(earth)
  :action nil
  :effect [(inc money-production 3)])

(terraform-card-def "Media Archives"
  :number 107
  :cost 8
  :type 'automated
  :requirements nil
  :tags '(earth)
  :action nil
  :effect [(inc-per money every-event)])

(terraform-card-def "Open City"
  :number 108
  :cost 23
  :type 'automated
  :requirements '(>= oxygen 12)
  :tags '(city building)
  :action nil
  :effect [(dec energy-production 1) (inc money-production 4) (inc plant 2) (add-city)]
  :victory-points 1)

(terraform-card-def "Media Group"
  :number 109
  :cost 6
  :type 'active
  :tags '(earth)
  :continuous-effect '(on event [(inc money 3)]))

(terraform-card-def "Business Network"
  :number 110
  :cost 4
  :type 'automated
  :requirements nil
  :tags '(earth)
  :action [(-> nil (draw-cards-keep-some 1 t))]
  :effect [(dec money-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Business Contacts"
  :number 111
  :cost 7
  :type 'event
  :tags '(earth)
  :effect [(draw-cards-keep-some 4 2)])

(terraform-card-def "Bribed Committee"
  :number 112
  :cost 7
  :type 'event
  :requirements nil
  :tags '(earth)
  :action nil
  :effect [(inc rating 2)]
  :victory-points -2)

(terraform-card-def "Solar Power"
  :number 113
  :cost 11
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(inc energy-production 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Breathing Filters"
  :number 114
  :cost 11
  :type 'automated
  :requirements '(>= oxygen 7)
  :tags '(science)
  :action nil
  :effect nil
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Artificial Photosynthesis"
  :number 115
  :cost 12
  :type 'automated
  :requirements nil
  :tags '(science)
  :action nil
  :effect [(or ((inc plant 1) (inc energy-production 2)))]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Artificial Lake"
  :number 116
  :cost 15
  :type 'automated
  :requirements '(>= tempurature -6)
  :tags '(building)
  :effect [(add-ocean-on-land)] ;; TODO add this effect
  :victory-points 1)

(terraform-card-def "Geothermal Power"
  :number 117
  :cost 11
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(inc energy-production 2)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Farming"
  :number 118
  :cost 16
  :type 'automated
  :requirements '(>= tempurature 4)
  :tags '(plant)
  :action nil
  :effect [(inc :money-production 2) (inc plant-production 2) (inc plant 2)]
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Dust Seals"
  :number 119
  :cost 16
  :type 'automated
  :requirements '(<= ocean 3)
  :tags '()
  :action nil
  :effect nil
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Urbanized Area"
  :number 120
  :cost 10
  :type 'automated
  :requirements nil
  :tags '(city building)
  :action nil
  :effect [(dec energy-production 1) (inc money-production 2) (add-city-urbanized-area)] ;; TODO add this effect
  )

(terraform-card-def "Sabotage"
  :number 121
  :cost 1
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(or ((dec-other titanium 3)
                (dec-other steel 4)
                (dec-other money 7)))])

(terraform-card-def "Moss"
  :number 122
  :cost 4
  :type 'automated
  :requirements '(>= ocean 3)
  :tags '(plant)
  :action nil
  :effect [(dec plant 1) (inc plant-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Industrial Center"
  :number 123
  :cost 4
  :type 'automated
  :requirements nil
  :tags '(building)
  :action [(-> (dec money 7) (inc steel-production 1))]
  :effect [(add-special industrial-center)])

(terraform-card-def "Hired Raiders"
  :number 124
  :cost 1
  :type 'event
  :tags '()
  :effect [(or ((steal steel 2) ;; TODO add this effect
                (steal money 3)))])

(terraform-card-def "Hackers"
  :number 125
  :cost 3
  :type 'automated
  :tags '()
  :effect [(dec energy-production 1) (dec-other money-production 2) (inc money-production 2)]
  :victory-points -1)

(terraform-card-def "GHG Factories"
  :number 126
  :cost 11
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 1) (inc heat-production 4)])

(terraform-card-def "Subterranean Reservoir"
  :number 127
  :cost 11
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(add-ocean)])

(terraform-card-def "Ecological Zone"
  :number 128
  :cost 12
  :type 'active
  :requirements '(>= own-forest 1)
  :tags '(animal plant)
  :action nil
  :effect [(add-special ecological-zone)]
  :continuous-effect '(on (tags (plant animal)) [(add animal 1)])
  :victory-points '(per-resource 1 2))

(terraform-card-def "Zeppelins"
  :number 129
  :cost 13
  :type 'automated
  :requirements '(>= oxygen 5)
  :tags '()
  :action nil
  :effect [(inc-per money every-city)]
  :victory-points 1)

;; here

(terraform-card-def "Worms"
  :number 130
  :cost 8
  :type 'automated
  :requirements '(>= oxygen 4)
  :tags '(microbe)
  :action nil
  :effect [(inc-per plant (tags microbe))]
  :continuous-effect nil
  :victory-points nil)


(terraform-card-def "Decomposers"
  :number 131
  :cost 5
  :type 'active
  :tags '(microbe)
  :requirements '(>= oxygen 3)
  :continuous-effect '(on (tags (plant animal microbe)) [(add microbe 1)]))

(terraform-card-def "Fusion Power"
  :number 132
  :cost 14
  :type 'automated
  :requirements '(>= (tags power) 2)
  :tags '(science building power)
  :action nil
  :effect [(inc energy-production 3)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Symbiotic Fungus"
  :number 133
  :cost 4
  :type 'active
  :requirements '(>= tempurature -14)
  :tags '(microbe)
  :action [(-> nil (add-other microbe 1))] ;; TODO add this effect
  )

(terraform-card-def "Extreme-Cold Fungus"
  :number 134
  :cost 13
  :type 'active
  :requirements '(<= tempurature -10)
  :tags '(microbe)
  :action [(-> nil (inc plant 1))
           (-> nil (add-other microbe 2))])

(terraform-card-def "Advanced Ecosystems"
  :number 135
  :cost 11
  :type 'automated
  :requirements '(and (>= (tags (plant)) 1)
                      (>= (tags (microbe)) 1)
                      (>= (tags (animal)) 1))
  :tags '(plant microbe animal)
  :victory-points 3)

(terraform-card-def "Great Dam"
  :number 136
  :cost 12
  :type 'automated
  :requirements 
  :tags '(power building)
  :action nil
  :effect [(inc energy-production 2)]
  :victory-points 1)

(terraform-card-def "Cartel"
  :number 137
  :cost 8
  :type 'automated
  :requirements nil
  :tags '(earth)
  :action nil
  :effect [(inc-per money (tags earth))]
  :victory-points 1)

(terraform-card-def "Strip Mine"
  :number 138
  :cost 25
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 2) (inc steel-production 2) (inc titanium-production 1)
           (inc-oxygen) (inc-oxygen)])

(terraform-card-def "Wave Power"
  :number 139
  :cost 8
  :type 'automated
  :requirements '(>= ocean 8)
  :tags '(power)
  :action nil
  :effect [(inc energy-production 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Lava Flows"
  :number 140
  :cost 18
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(inc-tempurature) (inc-tempurature) (add-special lava-flows)])

(terraform-card-def "Power Plant"
  :number 141
  :cost 4
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(inc energy-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Mohole Area"
  :number 142
  :cost 20
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(inc heat-production 4) (add-special mohole-area)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Large Convoy"
  :number 143
  :cost 36
  :type 'event
  :requirements nil
  :tags '(space earth)
  :action nil
  :effect [(add-ocean) (inc card 2) (or ((inc plant 5) (add-other animal 4)))]
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Titanium Mine"
  :number 144
  :cost 7
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(inc titanium-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Tectonic Stress Power"
  :number 145
  :cost 18
  :type 'automated
  :requirements '(>= (tags science) 2)
  :tags '(power building)
  :action nil
  :effect [(inc energy-production 3)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Nitrophilic Moss"
  :number 146
  :cost 8
  :type 'automated
  :requirements '(>= ocean 3)
  :tags '(plant)
  :action nil
  :effect [(inc plant-production 2) (dec plant 2)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Herbivores"
  :number 147
  :cost 12
  :type 'automated
  :requirements '(>= oxygen 8)
  :tags '(animal)
  :action nil
  :effect [(add animal 1) (dec plant-production 1)]
  :continuous-effect '(on greenery [(add animal 1)])
  :victory-points '(per-resource 1 2))

(terraform-card-def "Insects"
  :number 148
  :cost 9
  :type 'automated
  :requirements '(>= oxygen 6)
  :tags '(microbe)
  :action nil
  :effect [(inc-per plant (tags plant))]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "CEO's Favorite Project"
  :number 149
  :cost 1
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(add-other-with-resource _ 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Anti-Gravity Technology"
  :number 150
  :cost 14
  :type 'active
  :requirements '(>= (tags science) 7)
  :tags '(science)
  :continuous-effect '(card-discount nil 2)
  :victory-points 3)

(terraform-card-def "Investment Loan"
  :number 151
  :cost 3
  :type 'event
  :requirements nil
  :tags '(earth)
  :action nil
  :effect [(dec money-production 1) (inc money 10)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Insulation"
  :number 152
  :cost 2
  :type 'automated
  :requirements nil
  :tags '()
  :action nil
  :effect [(convert-resource heat money)] ;; TODO Implement this
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Adaptation Technology"
  :number 153
  :cost 12
  :type 'active
  :requirements nil
  :tags '(science)
  :action nil
  :effect nil
  :continuous-effect '(global-requirement-discount 2)
  :victory-points 1)

(terraform-card-def "Caretaker Contract"
  :number 154
  :cost 3
  :type 'active
  :requirements '(>= tempurature 0) 
  :tags '()
  :action [(-> (dec heat 8) (inc rating 1))])

(terraform-card-def "Designed Microorganisms"
  :number 155
  :cost 16
  :type 'automated
  :requirements '(<= tempurature 14)
  :tags '(science microbe)
  :action nil
  :effect [(inc plant-production 2)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Standard Technology"
  :number 156
  :cost 6
  :type 'active
  :requirements nil
  :tags '(science)
  :action nil
  :effect nil
  :continuous-effect '(standard-project-discount 3)
  :victory-points nil)

(terraform-card-def "Nitrite Reducing Bacteria"
  :number 157
  :cost 11
  :type 'active
  :requirements nil
  :tags '(microbe)
  :action [(-> nil (add microbe))
           (-> (dec microbe 3) (inc rating 1))]
  :effect [(inc microbe 3)])

(terraform-card-def "Industrial Microbes"
  :number 158
  :cost 12
  :type 'automated
  :requirements nil
  :tags '(microbe building)
  :action nil
  :effect [(inc energy-production 1) (inc steel-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Lichen"
  :number 159
  :cost 7
  :type 'automated
  :requirements '(>= tempurature -24)
  :tags '(plant)
  :action nil
  :effect [(inc plant-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Power Supply Consortium"
  :number 160
  :cost 5
  :type 'automated
  :requirements '(>= (tags (power)) 2)
  :tags '(power)
  :action nil
  :effect [(dec-other energy-production 1) (inc energy-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Convoy From Europa"
  :number 161
  :cost 15
  :type 'automated
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(add-ocean) (inc card 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Imported GHG"
  :number 162
  :cost 7
  :type 'automated
  :requirements nil
  :tags '(space earth)
  :action nil
  :effect [(inc heat 3) (inc heat-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Imported Nitrogen"
  :number 163
  :cost 23
  :type 'automated
  :tags '(space earth)
  :effect [(inc rating 1) (inc plant 4) (add-other microbe 3) (add-other animal 2)])

(terraform-card-def "Micro-Mills"
  :number 164
  :cost 3
  :type 'automated
  :requirements nil
  :tags '()
  :action nil
  :effect [(inc heat-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Magnetic Field Generators"
  :number 165
  :cost 20
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 4) (inc plant-production 2) (inc rating 3)])

(terraform-card-def "Shuttles"
  :number 166
  :cost 10
  :type 'active
  :requirements '(>= oxygen 5)
  :tags '(space)
  :action nil
  :effect [(dec energy-production 1) (inc money-production 2)]
  :continuous-effect '(card-discount space 2)
  :victory-points 1)

(terraform-card-def "Import of Advanced GHG"
  :number 167
  :cost 9
  :type 'event
  :requirements nil
  :tags '(earth space)
  :action nil
  :effect [(inc heat-production 2)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Windmills"
  :number 168
  :cost 6
  :type 'automated
  :requirements '(>= oxygen 7)
  :tags '(power building)
  :action nil
  :effect [(inc energy-production 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Tundra Farming"
  :number 169
  :cost 16
  :type 'automated
  :requirements '(>= tempurature -6)
  :tags '(plant)
  :action nil
  :effect [(inc plant-production 1) (inc money-production 2) (inc plant 1)]
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Aerobraked Ammonia Asteroid"
  :number 170
  :cost 26
  :type 'automated
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc heat-production 3) (inc plant-production 1) (add-other microbe 2)])

(terraform-card-def "Magnetic Field Dome"
  :number 171
  :cost 5
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 2) (inc plant-production 1) (inc rating 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Pets"
  :number 172
  :cost 10
  :type 'active
  :requirements nil
  :tags '(earth animal)
  :action nil
  :effect [(add animal 1)]
  :continuous-effect (on any-city [(add animal 1)])
  :victory-points '(per-resource 1 2))

(terraform-card-def "Protected Habitats"
  :number 173
  :cost 5
  :type 'active
  :requirements nil
  :tags '()
  :action nil
  :effect nil
  :continuous-effect `(add-modifier
                       (format "Oppenents may not remove your %s %s %s"
                               terraform--plant-char terraform--animal-tag terraform--microbe-tag)
                       protected-habitats))

(terraform-card-def "Protected Valley"
  :number 174
  :cost 23
  :type 'automated
  :requirements 
  :tags '(plant building)
  :action nil
  :effect [(inc money-production 2) (add-greenery)])

(terraform-card-def "Satellites"
  :number 175
  :cost 10
  :type 'automated
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc-per money (tags (space)))])

(terraform-card-def "Noctis Farming"
  :number 176
  :cost 10
  :type 'automated
  :requirements '(>= tempurature -20)
  :tags '(plant building)
  :action nil
  :effect [(inc money-production 1) (inc plant 2)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Water Splitting Plant"
  :number 177
  :cost 12
  :type 'active
  :requirements '(>= ocean 2)
  :tags '(building)
  :action [(-> (dec energy 3) [(inc-oxygen)])])

(terraform-card-def "Heat Trappers"
  :number 178
  :cost 6
  :type 'automated
  :requirements nil
  :tags '(power building)
  :action nil
  :effect [(dec-other heat-production 2) (inc energy-production 1)]
  :victory-points -1 )

(terraform-card-def "Soil Factory"
  :number 179
  :cost 9
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 1) (inc plant-production 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Fuel Factory"
  :number 180
  :cost 6
  :type 'automated
  :requirements nil
  :tags '(building)
  :action nil
  :effect [(dec energy-production 1) (inc titanium-production 1) (inc money-production 1)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Ice Cap Melting"
  :number 181
  :cost 5
  :type 'event
  :requirements '(>= tempurature 2)
  :tags '()
  :action nil
  :effect [(add-ocean)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Corporate Stronghold"
  :number 182
  :cost 11
  :type 'automated
  :requirements nil
  :tags '(city building)
  :action nil
  :effect [(dec energy-production 1) (inc money-production 3) (add-city)]
  :continuous-effect nil
  :victory-points -2)

(terraform-card-def "Biomass Combustors"
  :number 183
  :cost 4
  :type 'automated
  :requirements '(>= oxygen 6)
  :tags '(power building)
  :action nil
  :effect [(dec-other plant-production 1) (inc energy-production 2)]
  :continuous-effect nil
  :victory-points -1)

(terraform-card-def "Livestock"
  :number 184
  :cost 13
  :type 'active
  :requirements '(>= oxygen 9)
  :tags '(animal)
  :action [(-> nil (add animal 1))]
  :effect [(dec plant-production 1) (inc money-production 2)]
  :continuous-effect nil
  :victory-points '(per-resource 1 1))

(terraform-card-def "Olympus Conference"
  :number 185
  :cost 10
  :type 'active
  :requirements nil
  :tags '(science earth building)
  :action nil
  :effect nil
  :continuous-effect '(on (tags science) (or (add science 1) [(remove science 1) (inc card 1)])) ;; implement this
  :victory-points 1)

(terraform-card-def "RAD-Suits"
  :number 186
  :cost 6
  :type 'automated
  :requirements '(>= city 2)
  :tags '()
  :action nil
  :effect [(inc money-production 1)]
  :continuous-effect nil
  :victory-points 1)

(terraform-card-def "Aquifer Pumping"
  :number 187
  :cost 18
  :type 'active
  :requirements nil
  :tags '(building)
  :action [(-> (buy steel 8) [(add-ocean)])])

(terraform-card-def "Flooding"
  :number 188
  :cost 7
  :type 'event
  :requirements nil 
  :tags '()
  :action nil
  :effect [(flooding)] ;; TODO Implement this
  :victory-points -1)

(terraform-card-def "Energy Savings"
  :number 189
  :cost 15
  :type 'automated
  :tags '()
  :effect [(inc-per energy-production every-city)])

(terraform-card-def "Local Heat Trapping"
  :number 190
  :cost 1
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(dec heat 5) (or ((inc plant 4) (inc animal 2)))])

(terraform-card-def "Permafrost Extraction"
  :number 191
  :cost 8
  :type 'event
  :requirements '(>= tempurature -8)
  :tags '()
  :action nil
  :effect [(add-ocean)])

(terraform-card-def "Invention Contest"
  :number 192
  :cost 2
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(draw-cards-keep-some 3 1)])

(terraform-card-def "Plantation"
  :number 193
  :cost 15
  :type 'automated
  :requirements '(>= (tags (science)) 2)
  :tags '(plant)
  :action nil
  :effect [(add-greenery)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Power Infrastructure"
  :number 194
  :cost 4
  :type 'active
  :requirements nil
  :tags '(power building)
  :action [(-> nil [(convert-resource energy money)])])

(terraform-card-def "Indentured Workers"
  :number 195
  :cost 0
  :type 'event
  :requirements nil
  :tags '()
  :action nil
  :effect [(give-discount 8)] ;; TODO implement this
  :victory-points -1)

(terraform-card-def "Lagrange Observatory"
  :number 196
  :cost 9
  :type 'automated
  :requirements 
  :tags '(science space)
  :action nil
  :effect [(inc card 1)]
  :victory-points 1)

(terraform-card-def "Terraforming Ganymede"
  :number 197
  :cost 33
  :type 'automated
  :requirements nil
  :tags '(jovian space)
  :action nil
  :effect [(inc-per rating (tags jovian))]
  :continuous-effect nil
  :victory-points 2)

(terraform-card-def "Immigration Shuttles"
  :number 198
  :cost 31
  :type 'automated
  :requirements nil
  :tags '(earth space)
  :effect [(inc money-production 5)]
  :victory-points '(per-every-city 1 3))

(terraform-card-def "Restricted Area"
  :number 199
  :cost 11
  :type 'active
  :requirements nil
  :tags '(science)
  :action [(-> (dec money 2) [(inc card 1)])]
  :effect [(add-special restricted-area)]
  :continuous-effect nil
  :victory-points nil)

(terraform-card-def "Immigrant City"
  :number 200
  :cost 13
  :type 'active
  :requirements nil
  :tags '(city building)
  :action nil
  :effect [(dec energy-production 1) (dec money-production 2) (add-city)]
  :continuous-effect (on any-city (inc money-production 1)))

(terraform-card-def "Energy Tapping"
  :number 201
  :cost 3
  :type 'automated
  :requirements nil
  :tags '(power)
  :action nil
  :effect [(dec-other energy-production 1) (inc energy-production 1)]
  :continuous-effect nil
  :victory-points -1)

(terraform-card-def "Underground Detonations"
  :number 202
  :cost 6
  :type 'active
  :tags '(building)
  :action [(-> (dec money 10) (inc heat-production 2))])

(terraform-card-def "Soletta"
  :number 203
  :cost 35
  :type 'automated
  :tags '(space)
  :effect [(inc heat-production 7)])

(terraform-card-def "Technology Demonstration"
  :number 204
  :cost 5
  :type 'event
  :requirements nil
  :tags '(science space)
  :action nil
  :effect [(inc card 2)])

(terraform-card-def "RAD-Chem Factory"
  :number 205
  :cost 8
  :type 'automated
  :requirements nil
  :tags '()
  :action [(dec energy-production 1) (inc rating 2)])

(terraform-card-def "Special Design"
  :number 206
  :cost 4
  :type 'event
  :tags '(science)
  :effect [(give-param-discount 2)])

(terraform-card-def "Medical Lab"
  :number 207
  :cost 13
  :type 'automated
  :requirements nil
  :tags '(science building)
  :action nil
  :effect [(inc-per money-production (tags/ (building) 2))]
  :victory-points 1)

(terraform-card-def "AI Central"
  :number 208
  :cost 21
  :type 'active
  :requirements '(>= (tags science) 3)
  :tags '(science building)
  :action [(-> nil [(inc card 2)])]
  :effect [(dec energy-production 1)]
  :victory-points 1)

(terraform-card-def "Small Asteroid"
  :number 209
  :cost 10
  :type 'event
  :requirements nil
  :tags '(space)
  :action nil
  :effect [(inc-tempurature) (dec-other plant 2)])

;; END OF STANDARD CARDS

(defun terraform-card-generate-deck ()
  (let* ((ids (terraform-randomize (hash-table-keys terraform-card-directory))))
    (seq-map
     (lambda (id)
       (apply #'terraform-card-create (gethash id terraform-card-directory)))
     ids)))

(defun terraform-card-generate-corporation-deck ()
  (let* ((ids (terraform-randomize (hash-table-keys terraform-card-corporation-directory))))
    (seq-map
     (lambda (id)
       (apply #'terraform-corporation-create (gethash id terraform-card-corporation-directory)))
     ids)))

(provide 'terraform-card)

;;; terraform-card.el ends here
