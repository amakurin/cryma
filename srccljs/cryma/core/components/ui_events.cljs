(ns cryma.core.components.ui-events)

(defmulti wrap-event (fn [_ qn] (keyword (name qn))))

(def input-keys
  {8   :key/backspace
   9   :key/tab
   13  :key/enter
   27  :key/esc
   33  :key/page-up
   34  :key/page-down
   35  :key/end
   36  :key/home
   37  :key/left-arrow
   38  :key/up-arrow
   39  :key/right-arrow
   40  :key/down-arrow
   44  :key/comma
   45  :key/insert
   46  :key/delete
   48  :key/-0
   49  :key/-1
   50  :key/-2
   51  :key/-3
   52  :key/-4
   53  :key/-5
   54  :key/-6
   55  :key/-7
   56  :key/-8
   57  :key/-9
   65  :key/a
   66  :key/b
   67  :key/c
   68  :key/d
   69  :key/e
   70  :key/f
   71  :key/g
   72  :key/h
   73  :key/i
   74  :key/j
   75  :key/k
   76  :key/l
   77  :key/m
   78  :key/n
   79  :key/o
   80  :key/p
   81  :key/q
   82  :key/r
   83  :key/s
   84  :key/t
   85  :key/u
   86  :key/v
   87  :key/w
   88  :key/x
   89  :key/y
   90  :key/z
   96  :key/n0
   97  :key/n1
   98  :key/n2
   99  :key/n3
   100 :key/n4
   101 :key/n5
   102 :key/n6
   103 :key/n7
   104 :key/n8
   105 :key/n9
   106 :key/multiply
   107 :key/add
   109 :key/subtract
   110 :key/decimal-point
   111 :key/divide
   186 :key/semi-colon
   187 :key/equal-sign
   188 :key/comma
   189 :key/dash
   190 :key/period
   191 :key/forward-slash
   192 :key/grave-accent
   219 :key/open-bracket
   220 :key/back-slash
   221 :key/close-braket
   222 :key/single-quote})

(defn key-event [e qn]
  (let [kc (.-keyCode e)
        cc (.-charCode e)]
    {:qn        qn
     :kk        (input-keys (if (= 0 kc) cc kc))
     :key       (.-key e)
     :key-code  kc
     :char-code cc
     :alt?      (.-altKey e)
     :ctrl?     (.-ctrlKey e)
     :shift?    (.-shiftKey e)
     :meta?     (.-metaKey e)
     :repeat    (.-repeat e)
     :event     e}))

(defmethod wrap-event :on-key-down
  [e qn]
  (key-event e qn))

(defmethod wrap-event :on-key-press
  [e qn]
  (key-event e qn))

(defmethod wrap-event :on-key-up
  [e qn]
  (key-event e qn))

(defn stop-propagation [e]
  (let [e (if (map? e) (:event e) e)]
    (.stopPropagation e)))

(defn prevent-default [e]
  (let [e (if (map? e) (:event e) e)]
    (.preventDefault e)))

(defn cancel-event [e]
  (stop-propagation e)
  (prevent-default e))