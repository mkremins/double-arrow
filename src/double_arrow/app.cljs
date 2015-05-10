(ns double-arrow.app
  (:require [cljs.core.async :as async]
            [om.core :as om]
            [om-tools.core :refer-macros [defcomponent defcomponentmethod]]
            [om-tools.dom :as dom])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;;; miscellaneous helpers

(defn remove-item [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx) (count v)))))

(defn move-item [v idx1 idx2]
  (if (= idx1 idx2)
    v
    (let [item (nth v idx1)
          [before after] (split-at idx2 (remove-item v idx1))]
      (vec (concat before [item] after)))))

(defn with-indices [k coll]
  (map-indexed #(assoc %2 k %1) coll))

(defn NaN? [x]
  (and (number? x) (not= x x)))

(defn value [ev]
  (.. ev -target -value))

(defn input-element? [elem]
  (#{"INPUT" "SELECT" "TEXTAREA"} (.-tagName elem)))

;;; app state management

(defn parse-input
  "Given the top-level app state, parses the data in the `input` map and
  returns the rows with which the initial column should be populated."
  [{:keys [input]}]
  (case (:type input)
    :range
      (let [{:keys [start end step inclusive?]} input]
        (when-not (some NaN? [start end step])
          (range start (cond-> end inclusive? (+ step)) step)))
    ()))

(def lookup-fn
  {"even?" even? "odd?" odd?
   "dec" dec "inc" inc "str" str
   "identity" identity})

(defmulti populate
  "Given a `column` and the `rows` passed in from the previous column, should
  return a 2-tuple `[rows-for-column rows-to-pass-on]`. `rows-for-column` will
  be displayed under this `column` in the view; `rows-to-pass-on` will be
  passed in as the `rows` argument when `populate` is called on the next column
  down the line."
  (fn [column rows] (:type column)))

(defmethod populate :drop [column rows]
  (let [[dropped taken] (split-at (:n column) rows)
        dropped-rows (map #(-> {:value % :accept? false}) dropped)
        taken-rows (map #(-> {:value % :accept? true}) taken)]
    [(concat dropped-rows taken-rows) taken]))

(defmethod populate :drop-last [column rows]
  (let [[taken dropped] (split-at (- (count rows) (:n column)) rows)
        taken-rows (map #(-> {:value % :accept? true}) taken)
        dropped-rows (map #(-> {:value % :accept? false}) dropped)]
    [(concat taken-rows dropped-rows) taken]))

(defmethod populate :filter [column rows]
  (let [predicate (lookup-fn (:predicate column))]
    [(map #(-> {:value % :accept? (predicate %)}) rows)
     (filter predicate rows)]))

(defmethod populate :map [column rows]
  (let [new-rows (map (lookup-fn (:function column)) rows)]
    [new-rows new-rows]))

(defmethod populate :reverse [column rows]
  (let [new-rows (reverse rows)]
    [new-rows new-rows]))

(defmethod populate :take [column rows]
  (let [[taken dropped] (split-at (:n column) rows)
        taken-rows (map #(-> {:value % :accept? true}) taken)
        dropped-rows (map #(-> {:value % :accept? false}) dropped)]
    [(concat taken-rows dropped-rows) taken]))

(defmethod populate :take-last [column rows]
  (let [[dropped taken] (split-at (- (count rows) (:n column)) rows)
        dropped-rows (map #(-> {:value % :accept? false}) dropped)
        taken-rows (map #(-> {:value % :accept? true}) taken)]
    [(concat dropped-rows taken-rows) taken]))

(defn populate-columns
  "Given the top-level app state `data`, zeroes out the rows in each column and
  repopulates the columns by feeding rows from one to the next. Should be used
  whenever a change occurs to bring the rows in every column up to date."
  [data]
  (let [init-rows (parse-input data)]
    (loop [rows init-rows
           new-columns []
           old-columns (map #(dissoc % :error :rows) (:columns data))]
      (if-let [column (first old-columns)]
        (try
          (let [[rows-for-column rows-to-pass-on] (populate column rows)]
            (recur rows-to-pass-on
                   (conj new-columns (assoc column :rows (vec rows-for-column)))
                   (rest old-columns)))
          (catch js/Error err
            (recur nil
                   `[~@new-columns
                     ~(assoc column :error (.-message err))
                     ~@(rest old-columns)]
                   nil)))
        (assoc data :columns new-columns :init-rows init-rows)))))

(defonce app-state
  (atom (-> {:input {:type :range :start 0 :end 10 :step 1 :inclusive? false}
             :columns []}
            populate-columns)))

(def column-defaults
  {:drop {:n 0}
   :drop-last {:n 0}
   :filter {:predicate "identity"}
   :map {:function "identity"}
   :take {:n 10}
   :take-last {:n 10}})

(defn add-column
  "Given the top-level app state `data` and a valid column `type`, adds a new
  column of this `type` to the end of the pipeline."
  ([type] (partial add-column type))
  ([type data]
    (let [type (keyword type)
          new-column (assoc (column-defaults type) :type type)]
      (update data :columns conj new-column))))

(defn move-column-by
  "Given the top-level app state `data` and an `offset` in pixels by which the
  column at `column-idx` was moved, adjusts the order of the columns."
  ([offset column-idx] (partial move-column-by offset column-idx))
  ([offset column-idx data]
    (let [column-width 200
          adjust (if (neg? offset) js/Math.ceil js/Math.floor)
          target-idx (-> (+ column-idx (adjust (/ offset column-width)))
                         (max 0)
                         (min (dec (count (:columns data)))))]
      (update data :columns move-item column-idx target-idx))))

(defn delete-selected-columns [data]
  (update data :columns #(vec (remove :selected? %))))

;;; Om components

(defcomponent number-input [data owner {:keys [path]}]
  (render [_]
    (dom/input {:on-change #(om/update! data path (js/parseFloat (value %)))
                :type "number" :value (path data)})))

(defcomponent input [data owner]
  (render [_]
    (dom/div {:class "input"}
      (dom/span "The input is ")
      (dom/select {:on-change #(om/update! data :type (keyword (value %)))
                   :value (name (:type data))}
        ;(dom/option {:value "text"} "text")
        ;(dom/option {:value "file"} "a file")
        (dom/option {:value "range"} "a range"))
      (case (:type data)
        :range
          (dom/span
            " from "
            (om/build number-input data {:opts {:path :start}})
            " to "
            (om/build number-input data {:opts {:path :end}})
            " "
            (dom/span {:class "toggle"
                       :on-click #(om/transact! data :inclusive? not)}
              (if (:inclusive? data) "inclusive" "exclusive"))
            " with step size "
            (om/build number-input data {:opts {:path :step}})
            ".")))))

(defcomponent simple-row [data owner]
  (render [_]
    (dom/div {:class "row"} (pr-str data))))

(defcomponent maybe-row [data owner]
  (render [_]
    (let [{:keys [accept? value]} data]
      (dom/div {:class (str "row " (if accept? "accept" "reject"))}
        (pr-str value)))))

(defmulti column-header-contents (fn [data _] (:type data)))

(defn filtering-column? [{:keys [type]}]
  (#{:drop :drop-last :filter :take :take-last} type))

(defcomponent column [data owner]
  (init-state [_] {})
  (render-state [_ state]
    (dom/div {:class (cond-> (str "column " (name (:type data)))
                             (:init state) (str " dragging")
                             (:selected? data) (str " selected"))
              :style {:transform (str "translateX(" (:offset state 0) "px)")}}
      (dom/div {:class "column-header"
                :on-mouse-down
                #(when-not (input-element? (.-target %))
                   (om/set-state! owner :init (.-clientX %)))
                :on-mouse-move
                #(when-let [init (om/get-state owner :init)]
                   (om/set-state! owner :offset (- (.-clientX %) init)))
                :on-mouse-up
                #(when-let [init (om/get-state owner :init)]
                   (if (= init (.-clientX %))
                     (om/transact! data :selected? not)
                     (async/put! (om/get-shared owner :drag-ch)
                                 {:offset (- (.-clientX %) init) :id (:id data)}))
                   (om/set-state! owner {}))}
        (om/build column-header-contents data))
      (if-let [error (:error data)]
        (dom/p {:class "error"} error)
        (om/build-all (if (filtering-column? data) maybe-row simple-row)
                      (:rows data))))))

(doseq [[type label] {:drop "Drop the first "
                      :drop-last "Drop the last "
                      :take "Limit to the first "
                      :take-last "Limit to the last "}]
  (defcomponentmethod column-header-contents type [data _]
    (render [_]
      (dom/span label (om/build number-input data {:opts {:path :n}})))))

(defcomponentmethod column-header-contents :filter [data owner]
  (render [_]
    (dom/span
      "Filter by "
      (dom/select {:on-change #(om/update! data :predicate (value %))
                   :value (:predicate data)}
        (for [predicate ["identity" "even?" "odd?"]]
          (dom/option {:value predicate} predicate))))))

(defcomponentmethod column-header-contents :map [data owner]
  (render [_]
    (dom/span
      "Apply "
      (dom/select {:on-change #(om/update! data :function (value %))
                   :value (:function data)}
        (for [function ["identity" "dec" "inc" "str"]]
          (dom/option {:value function} function)))
      " to each")))

(defcomponentmethod column-header-contents :reverse [_ _]
  (render [_] (dom/span "Reverse order")))

(defcomponent init-column [data owner]
  (render [_]
    (dom/div {:class "column init"}
      (dom/div {:class "column-header"} "Initial data")
      (om/build-all simple-row data))))

(defcomponent column-builder [data owner]
  (render [_]
    (dom/div {:class "column column-builder"}
      (dom/div {:class "column-builder-header"}
        (dom/select {:on-change #(om/transact! data (add-column (value %)))
                     :value nil}
          (dom/option {:value nil} "Add another column...")
          (dom/option {:value "drop"} "Drop the first 𝑛 items")
          (dom/option {:value "drop-last"} "Drop the last 𝑛 items")
          (dom/option {:value "filter"} "Filter items")
          (dom/option {:value "take"} "Limit to the first 𝑛 items")
          (dom/option {:value "take-last"} "Limit to the last 𝑛 items")
          (dom/option {:value "reverse"} "Reverse order")
          (dom/option {:value "map"} "Transform each item")
          ;(dom/option {:value "sort"} "Sort items")
          )))))

(defcomponent app [data owner]
  (will-mount [_]
    (go-loop []
      (when-let [{:keys [id offset]} (async/<! (om/get-shared owner :drag-ch))]
        (om/transact! data [] (move-column-by offset id))
        (recur))))
  (render [_]
    (dom/div {:class "app"}
      (dom/div {:class "header"}
        (om/build input (:input data)))
      (dom/div {:class "main"}
        (dom/div {:class "columns"}
          (om/build init-column (:init-rows data))
          (om/build-all column (with-indices :id (:columns data)))
          (om/build column-builder data))))))

(defn handle-keydown! [ev]
  (when (and (= (.-keyCode ev) 8) ; backspace
             (not (input-element? (.-target ev))))
    (.preventDefault ev)
    (om/transact! (om/root-cursor app-state) [] delete-selected-columns)))

(defn -main []
  (enable-console-print!)
  (om/root app app-state
    {:shared {:drag-ch (async/chan)}
     :target (js/document.getElementById "app")
     :tx-listen (fn [{:keys [tag]} data]
                  (when-not (= tag :populate)
                    (om/transact! data [] populate-columns :populate)))})
  (js/document.addEventListener "keydown" handle-keydown!))

(-main)
