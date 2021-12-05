(defn make-cluster-repo
  [{:keys [name url]}]
  {:apiVersion "catalog.cattle.io/v1"
   :metadata   {:name name}
   :spec       {:url url}})
