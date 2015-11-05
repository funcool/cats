(ns cats.labs.crdt.util
  #?(:clj (:import java.net.InetAddress)))

(defn hostname
  "Determines the hostname in the most portable manner."
  []
  #?(:clj (.getHostName (InetAddress/getLocalHost))
     :cljs (if (= *target* "nodejs")
             (let [os (js/require "os")]
               (.hostname os))
             (.-hostname js/location))))
