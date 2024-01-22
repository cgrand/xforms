(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.shell :as sh]))

(def lib 'net.cgrand/xforms)
(def version "0.19.6"  #_(format "0.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def scm {:connection "scm:git:git://github.com/cgrand/xforms.git"
          :developerConnection "scm:git:git://github.com/cgrand/xforms.git"
          :url "https://github.com/cgrand/xforms"})
(def extra-pom-data
  [[:licenses
    [:license
     [:name "Eclipse Public License 1.0"]
     [:url "https://opensource.org/license/epl-1-0/"]
     [:distribution "repo"]]
    [:license
     [:name "Eclipse Public License 2.0"]
     [:url "https://opensource.org/license/epl-2-0/"]
     [:distribution "repo"]]]])

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]
                :scm (assoc scm :tag (str "v" version))
                :pom-data extra-pom-data})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn clojars [_]
  (sh/sh
    "mvn" "deploy:deploy-file" (str "-Dfile=" jar-file)
;target/classes/META-INF/maven/net.cgrand/xforms/pom.xml
    (format "-DpomFile=%s/META-INF/maven/%s/%s/pom.xml"
      class-dir (namespace lib) (name lib))
    "-DrepositoryId=clojars" "-Durl=https://clojars.org/repo/"))
