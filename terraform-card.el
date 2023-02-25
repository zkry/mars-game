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

(defconst terraform-card-effect-registry nil)

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


(defun terraform-card-register-effect (effect-name param-list lighter-fn effect-fn)
  (add-to-list 'terraform-card-effect-registry (list effect-name param-list lighter-fn effect-fn) t #'equal))

(defmacro terraform-card-def-effect (name params &rest body)
  (declare (indent 2))
  (let* ((keyw)
         (lighter))
    (while (keywordp (setq keyw (car body)))
      (setq body (cdr body))
      (pcase keyw
        (:lighter (setq lighter (purecopy (pop body))))))
    `(terraform-card-register-effect
      ',name
      ',params
      (lambda ,params ,lighter)
      (lambda ,params ,@body))))

(terraform-card-def-effect inc (resource amt)
  :lighter (format "+%d%s" amt (terraform-resource-type-to-string resource))
  (terraform-!increment-user-resource resource amt))

(terraform-card-def-effect add (resource amt)
  :lighter (format "+%d%s" amt (terraform-resource-type-to-string resource))
  (cl-incf (terraform-card-resource-count terraform-active-card)))

(terraform-card-def-effect inc-per (resource by)
  :lighter (format "+%s/%s"
             (terraform-resource-type-to-string resource)
             (terraform-resource-type-to-string by))
  (let* ((amt (terraform--count-thing thing)))
    (terraform-!increment-user-resource resource amt)))

(terraform-card-def-effect buy (resource amt)
  :lighter (let* ((purchase-symbol (pcase resource
                                     ('titanium terraform--titanium-char)
                                     ('steel terraform--steel-char))))
             (format "-%d%s(%s)" amt terraform--money-char purchase-symbol))
  (let ((resource-amt (pop params))
        (resource-sell-amt
         (terraform-get-player-resource-sell-amount terraform-active-player resource))
        (new-price (max (- amount (* resource-amt resource-sell-amt)) 0)))
    (terraform-!increment-user-resource 'money (- new-price))
    (terraform-!increment-user-resource resource (- amount))))

(terraform-card-def-effect dec (resource amt)
  :lighter (format "-%d%s" amt (terraform-resource-type-to-string resource))
  (terraform-!increment-user-resource resource (- amount)))

(terraform-card-def-effect dec-other (resource amt)
  :lighter (format "-%d%s" amt (terraform--char->decrease-any (terraform-resource-type-to-string resource)))
  ())

(terraform-card-def-effect inc-tempurature ()
  :lighter "+℃"
  (terraform-!increase-tempurature 1))

(terraform-card-def-effect add-ocean ()
  :lighter "+🌊"
  (let ((location (pop params)))
    (terraform-!place-ocean location)))

(terraform-card-def-effect add-greenery ()
  :lighter "+🌳"
  (let ((location (pop params)))
    (terraform-!place-greenery location)))

(terraform-card-def-effect add-city ()
  :lighter "+🏙️"
  (let ((location (pop params)))
    (terraform-!place-city location)))

(terraform-card-def-effect add-noctis-city ()
  :lighter "+🏙️*")

(terraform-card-def-effect add-non-adjacent-city ()
  :lighter "+🏙️*")

(terraform-card-def-effect action (disp lambda)
  :lighter disp
  (funcall action-lambda terraform-active-card terraform-active-player))

(terraform-card-def-effect -> (cost action)
  :lighter (format "%s%s%s"
                   (terraform-effect-to-string cost)
                   terraform--action-arrow
                   (terraform-effect-to-string action))
  )

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

(terraform-card-def-effect add-modifier (descr flag)
  :lighter descr)

(terraform-card-def-effect or (&rest clauses)
  :lighter (string-join
            (seq-map
             #'terraform-effect-to-string
             clauses)
            "|"))

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
 :continuous-effect '(on (spend 20) (inc money 4)))
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
  (unless (or (functionp victory-points) (integerp victory-points) (not victory-points))
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
             :continuous-effect ,continuous-effect)
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
  :effect [(or (inc plant 3) (add microbe 3) (add animal 2))
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
  :type 'automated
  :requirements '(<= tempurature -12) ;; TODO : right unit?
  :tags '(plant)
  :effect [(inc plant 1)]
  :continuous-effect '(on ocean-placed (inc plant 2)))

(terraform-card-def "Predators"
  :number 24
  :cost 14
  :requirements  '(>= oxygen 11)
  :type 'active
  :tags '(animal)
  :action [(-> (remove any-animal 1)
               (add-token 1))]
  :victory-points
  (lambda (this) (terraform-card-resource-count this)))

(terraform-card-def "Space Station"
  :number 25
  :cost 10
  :requirements nil
  :type 'automated
  :tags '(space)
  :continuous-effect '(add-modifier
                       ":space::-2$"
                       (reduce-cost-for-tag space 2))
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

(defun terraform-card-generate-deck ()
  ;; TODO: Shuffle deck
  (let* ((ids (hash-table-keys terraform-card-directory)))
    (seq-map
     (lambda (id)
       (apply #'terraform-card-create (gethash id terraform-card-directory)))
     ids)))

(terraform-card-generate-deck)

(defun terraform-card-generate-corporation-deck ()
  (let* ((ids (hash-table-keys terraform-card-corporation-directory)))
    (seq-map
     (lambda (id)
       (apply #'terraform-corporation-create (gethash id terraform-card-corporation-directory)))
     ids)))

(provide 'terraform-card)

;;; terraform-card.el ends here
