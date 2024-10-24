"Implements Namespace-dependent methods and structures."

;; * Imports

(require jedhy.macros * :readers *)
(import jedhy.macros *)
(import builtins
        
        hy
        hy.reader
        hy.core.result-macros [_hy_macros :as _compile-table]

        hyrule [distinct butlast]
        itertools [chain]
        tlz [last])
(require hyrule [->>])

;; * Fixes

;; See this issue for below #1467: https://github.com/hylang/hy/issues/1467
;;(hy.eval `(import hy.macros))
;;(hy.eval `(require [hy.extra.anaphoric [*]]))
;; Overwrite Hy's mangling
(import jedhy.macros [mangle])

;; * Namespace

(defclass Namespace [object]
  (defn __init__ [self [globals- None] [locals- None] [macros- None]]
    ;; Components
    (setv self.globals       (or globals- (globals)))
    (setv self.locals        (or locals- (locals)))
    (setv self.macros        (tlz.keymap unmangle (or macros- _hy_macros)))
    (setv self.compile-table (self._collect-compile-table))

    ;; Collected
    (setv self.names (self._collect-names)))
  
  (defn [staticmethod] _to-names [key]
    "Function for conerting keys (strs, functions, modules...) to names."
    (unmangle (if (isinstance key str)
                key
                key.__name__)))

  (defn _collect-compile-table [self]
    "Collect compile table as dict."
    (->> _compile-table
         (tlz.keymap self._to-names)))

  (defn _collect-names [self]
    "Collect all names from all places."
    (->>
      (chain (allkeys self.globals)
             (allkeys self.locals)
             (.keys self.macros)
             (.keys self.compile-table))
      (map self._to-names)
      distinct
      tuple))

  (defn eval [self mangled-symbol]
    "Evaluate `mangled-symbol' within the Namespace."
    ;; Short circuit a common case (completing without "." present at all)
    (when (not mangled-symbol)
      (return None))

    (let [hy-tree (hy.reader.read mangled-symbol)]
      (try
        (hy.eval hy-tree :locals self.globals)
        (except [e NameError]
          (try
            (hy.eval hy-tree :locals self.locals)
            (except []
              None)))))))

;; * Candidate

(defclass Candidate []
  (defn __init__ [self symbol [namespace None]]
    (setv self.symbol    (unmangle symbol))
    (setv self.mangled   (mangle symbol))
    (setv self.namespace (or namespace (Namespace))))

  (defn __str__ [self]
    self.symbol)

  (defn __repr__ [self]
    (.format "Candidate<(symbol={}>)" self.symbol))

  (defn __eq__ [self other]
    (when (isinstance other Candidate)
      (= self.symbol other.symbol)))

  (defn __bool__ [self]
    (bool self.symbol))

  (defn compiler? [self]
    "Is candidate a compile table construct and return it."
    (try
      (get self.namespace.compile-table self.symbol)
      (except [e KeyError]
        None)))

  (defn macro? [self]
    "Is candidate a macro and return it."
    (try
      (get self.namespace.macros self.symbol)
      (except [e KeyError]
        None)))

  (defn evaled? [self]
    "Is candidate evaluatable and return it."
    (try
      (.eval self.namespace self.symbol)
      (except [e Exception]
        None)))

  (defn get-obj [self]
    "Get object for underlying candidate."
    ;; Compiler *must* come after .evaled to catch objects that are
    ;; both shadowed and in the compile table as shadowed (eg. `+`)
    (or (self.macro?)
        (self.evaled?)
        (self.compiler?)))

  (defn attributes [self]
    "Return attributes for obj if they exist."
    (let [obj (self.evaled?)]
      (when obj
        (->> obj dir (map unmangle) tuple))))

  (defn [staticmethod] _translate-class [klass]
    "Return annotation given a name of a class."
    (cond (in klass ["function" "builtin_function_or_method"]) "def"
          (= klass "type")                                     "class"
          (= klass "module")                                   "module"
          True                                                 "instance"))

  (defn annotate [self]
    "Return annotation for a candidate."
    (let [obj (self.evaled?)
          obj? (not (is obj None))  ; Obj could be instance of bool
          
          ;; Shadowed takes first priority but compile table takes last priority
          annotation (cond
                       obj?              (self._translate-class obj.__class__.__name__)
                       (.compiler? self) "compiler"
                       (.macro? self)    "macro")]
      
      (.format "<{} {}>" annotation self))))

;; * Prefix

(defclass Prefix []
  "A completion candidate."
  
  (defn __init__ [self prefix [namespace None]]
    (setv self.prefix prefix)
    (setv self.namespace (or namespace (Namespace)))
    
    (setv self.candidate (self._prefix->candidate prefix self.namespace))
    (setv self.attr-prefix (self._prefix->attr-prefix prefix))

    (setv self.completions (tuple)))

  (defn __repr__ [self]
    (.format "Prefix<(prefix={})>" self.prefix))
  
  (defn [staticmethod] _prefix->candidate [prefix namespace]
    (->> (.split prefix ".")
         butlast
         (.join ".")
         (Candidate :namespace namespace)))

  (defn [staticmethod] _prefix->attr-prefix [prefix]
    "Get prefix as str of everything after last dot if a dot is there."
    (->> (.split prefix ".")
         last
         unmangle))

  (defn [property] has-attr? [self]
    "Does prefix reference an attr?"
    (in "." self.prefix))

  (defn [property] obj? [self]
    "Is the prefix's candidate an object?"
    (bool (.get-obj self.candidate)))

  (defn complete-candidate [self completion]
    "Given a potential string `completion`, attach to candidate."
    (if self.candidate
        (+ (str self.candidate) "." completion)
        completion))

  (defn complete [self [cached-prefix None]]
    "Get candidates for a given Prefix."
    ;; Short circuit the case: "1+nonsense.real-attr" eg. "foo.__prin"
    (when (and self.has-attr?  ; The and ordering here matters for speed
               (not self.obj?))
      (setv self.completions (tuple))
      (return self.completions))

    ;; Complete on relevant top-level names or candidate-dependent names
    (if (and cached-prefix
             (= self.candidate cached-prefix.candidate))
        (setv self.completions cached-prefix.completions)
        (setv self.completions (or (.attributes self.candidate)
                                   self.namespace.names)))

    (->> self.completions
       (filter #f(str.startswith self.attr-prefix))
       (map self.complete-candidate)
       tuple)))
