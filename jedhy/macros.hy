"Some general purpose imports and code."

;; * Imports

(import functools
        tlz
        hy [unmangle
            mangle :as unfixed-mangle])
(require hyrule [fn+ ->>])

;; * Hy Overwrites

;; We have different requirements in the empty string case
(defn mangle [s]
  (if (!= s "") (unfixed-mangle s) (str)))

;; * Tag Macros

(defreader $
  "Partially apply a form eg. (#$(map inc) [1 2 3])."
  (let [form (.parse-one-form &reader)]
      `(do
         (import functools [partial])
         (functools.partial ~@form))))

(defreader f
  "Flipped #$."
  (let [form (.parse-one-form &reader)]
    `(do
       (import tlz [flip])
       (tlz.flip ~@form))))

;; * Misc

(defn _allkeys [d * [parents #()]]
  "In-order tuples of keys of nested, variable-length dict."
  (if (isinstance d #(list tuple))
      []
      (tuple (->> d
                  (tlz.keymap (fn [k] (+ parents #(k))))
                  (.items)
                  (map (fn+ [[k v]]
                         (if (isinstance v dict)
                             (_allkeys v :parents k)
                             [k])))
                  (tlz.concat)))))

(defn allkeys [d]
  (->> d _allkeys (map tlz.last) tuple))
