(ns cryma.core.localizer
  (:require [cryma.core.api :as api]
            [cryma.core.l10n :as l10n]
            [clojure.string :as clostr])
  (:import (clojure.lang IFn)))

(def readers-key :localizer/vocabulary-readers)
(def locales-key :localizer/locales)
(def default-locale-key :localizer/default-locale)
(def locale-request-path-key :localizer/locale-request-path)

(def locale-infos
  {"af"    {:name "Afrikaans", :native-name "Afrikaans"}
   "ak"    {:name "Akan", :native-name "Akan"}
   "sq"    {:name "Albanian", :native-name "Shqip"}
   "arq"   {:name "Algerian Arabic", :native-name "الدارجة الجزايرية"}
   "am"    {:name "Amharic", :native-name "አማርኛ"}
   "ar"    {:name "Arabic", :native-name "العربية"}
   "hy"    {:name "Armenian", :native-name "Հայերեն"}
   "rup"   {:name "Aromanian", :native-name "Armãneashce"}
   "frp"   {:name "Arpitan", :native-name "Arpitan"}
   "as"    {:name "Assamese", :native-name "অসমীয়া"}
   "az"    {:name "Azerbaijani", :native-name "Azərbaycan dili"}
   "az-tr" {:name "Azerbaijani (Turkey)", :native-name "Azərbaycan Türkcəsi"}
   "bcc"   {:name "Balochi Southern", :native-name "بلوچی مکرانی"}
   "ba"    {:name "Bashkir", :native-name "башҡорт теле"}
   "eu"    {:name "Basque", :native-name "Euskara"}
   "bel"   {:name "Belarusian", :native-name "Беларуская мова"}
   "bn"    {:name "Bengali", :native-name "বাংলা"}
   "bs"    {:name "Bosnian", :native-name "Bosanski"}
   "br"    {:name "Breton", :native-name "Brezhoneg"}
   "bg"    {:name "Bulgarian", :native-name "Български"}
   "ca"    {:name "Catalan", :native-name "Català"}
   "bal"   {:name "Catalan (Balear)", :native-name "Català (Balear)"}
   "ceb"   {:name "Cebuano", :native-name "Cebuano"}
   "zh-cn" {:name "Chinese (China)", :native-name "简体中文"}
   "zh-hk" {:name "Chinese (Hong Kong)", :native-name "香港中文版"}
   "zh-tw" {:name "Chinese (Taiwan)", :native-name "繁體中文"}
   "co"    {:name "Corsican", :native-name "Corsu"}
   "hr"    {:name "Croatian", :native-name "Hrvatski"}
   "cs"    {:name "Czech", :native-name "Čeština‎"}
   "da"    {:name "Danish", :native-name "Dansk"}
   "nl"    {:name "Dutch", :native-name "Nederlands"}
   "nl-be" {:name "Dutch (Belgium)", :native-name "Nederlands (België)"}
   "dzo"   {:name "Dzongkha", :native-name "རྫོང་ཁ"}
   "en"    {:name "English", :native-name "English"}
   "en-au" {:name "English (Australia)", :native-name "English (Australia)"}
   "en-ca" {:name "English (Canada)", :native-name "English (Canada)"}
   "en-nz" {:name "English (New Zealand)", :native-name "English (New Zealand)"}
   "en-za" {:name "English (South Africa)", :native-name "English (South Africa)"}
   "en-gb" {:name "English (UK)", :native-name "English (UK)"}
   "eo"    {:name "Esperanto", :native-name "Esperanto"}
   "et"    {:name "Estonian", :native-name "Eesti"}
   "fo"    {:name "Faroese", :native-name "Føroyskt"}
   "fi"    {:name "Finnish", :native-name "Suomi"}
   "fr-be" {:name "French (Belgium)", :native-name "Français de Belgique"}
   "fr-ca" {:name "French (Canada)", :native-name "Français du Canada"}
   "fr"    {:name "French (France)", :native-name "Français"}
   "fy"    {:name "Frisian", :native-name "Frysk"}
   "fur"   {:name "Friulian", :native-name "Friulian"}
   "fuc"   {:name "Fulah", :native-name "Pulaar"}
   "gl"    {:name "Galician", :native-name "Galego"}
   "ka"    {:name "Georgian", :native-name "ქართული"}
   "de"    {:name "German", :native-name "Deutsch"}
   "de-ch" {:name "German (Switzerland)", :native-name "Deutsch (Schweiz)"}
   "el"    {:name "Greek", :native-name "Ελληνικά"}
   "kal"   {:name "Greenlandic", :native-name "Kalaallisut"}
   "gn"    {:name "Guaraní", :native-name "Avañe'ẽ"}
   "gu"    {:name "Gujarati", :native-name "ગુજરાતી"}
   "haw"   {:name "Hawaiian", :native-name "Ōlelo Hawaiʻi"}
   "haz"   {:name "Hazaragi", :native-name "هزاره گی"}
   "he"    {:name "Hebrew", :native-name "עִבְרִית"}
   "hi"    {:name "Hindi", :native-name "हिन्दी"}
   "hu"    {:name "Hungarian", :native-name "Magyar"}
   "is"    {:name "Icelandic", :native-name "Íslenska"}
   "ido"   {:name "Ido", :native-name "Ido"}
   "id"    {:name "Indonesian", :native-name "Bahasa Indonesia"}
   "ga"    {:name "Irish", :native-name "Gaelige"}
   "it"    {:name "Italian", :native-name "Italiano"}
   "ja"    {:name "Japanese", :native-name "日本語"}
   "jv"    {:name "Javanese", :native-name "Basa Jawa"}
   "kab"   {:name "Kabyle", :native-name "Taqbaylit"}
   "kn"    {:name "Kannada", :native-name "ಕನ್ನಡ"}
   "kk"    {:name "Kazakh", :native-name "Қазақ тілі"}
   "km"    {:name "Khmer", :native-name "ភាសាខ្មែរ"}
   "kin"   {:name "Kinyarwanda", :native-name "Ikinyarwanda"}
   "ky"    {:name "Kirghiz", :native-name "кыргыз тили"}
   "ko"    {:name "Korean", :native-name "한국어"}
   "ckb"   {:name "Kurdish (Sorani)", :native-name "كوردی‎"}
   "lo"    {:name "Lao", :native-name "ພາສາລາວ"}
   "lv"    {:name "Latvian", :native-name "Latviešu valoda"}
   "li"    {:name "Limburgish", :native-name "Limburgs"}
   "lin"   {:name "Lingala", :native-name "Ngala"}
   "lt"    {:name "Lithuanian", :native-name "Lietuvių kalba"}
   "lb"    {:name "Luxembourgish", :native-name "Lëtzebuergesch"}
   "mk"    {:name "Macedonian", :native-name "Македонски јазик"}
   "mg"    {:name "Malagasy", :native-name "Malagasy"}
   "ms"    {:name "Malay", :native-name "Bahasa Melayu"}
   "ml"    {:name "Malayalam", :native-name "മലയാളം"}
   "mri"   {:name "Maori", :native-name "Te Reo Māori"}
   "mr"    {:name "Marathi", :native-name "मराठी"}
   "xmf"   {:name "Mingrelian", :native-name "მარგალური ნინა"}
   "mn"    {:name "Mongolian", :native-name "Монгол"}
   "me"    {:name "Montenegrin", :native-name "Crnogorski jezik"}
   "ary"   {:name "Moroccan Arabic", :native-name "العربية المغربية"}
   "mya"   {:name "Myanmar (Burmese)", :native-name "ဗမာစာ"}
   "ne"    {:name "Nepali", :native-name "नेपाली"}
   "nb"    {:name "Norwegian (Bokmål)", :native-name "Norsk bokmål"}
   "nn"    {:name "Norwegian (Nynorsk)", :native-name "Norsk nynorsk"}
   "oci"   {:name "Occitan", :native-name "Occitan"}
   "ory"   {:name "Oriya", :native-name "ଓଡ଼ିଆ"}
   "os"    {:name "Ossetic", :native-name "Ирон"}
   "ps"    {:name "Pashto", :native-name "پښتو"}
   "fa"    {:name "Persian", :native-name "فارسی"}
   "fa-af" {:name "Persian (Afghanistan)", :native-name "(فارسی (افغانستان"}
   "pl"    {:name "Polish", :native-name "Polski"}
   "pt-br" {:name "Portuguese (Brazil)", :native-name "Português do Brasil"}
   "pt"    {:name "Portuguese (Portugal)", :native-name "Português"}
   "pa"    {:name "Punjabi", :native-name "ਪੰਜਾਬੀ"}
   "rhg"   {:name "Rohingya", :native-name "Ruáinga"}
   "ro"    {:name "Romanian", :native-name "Română"}
   "roh"   {:name "Romansh Vallader", :native-name "Rumantsch Vallader"}
   "ru"    {:name "Russian", :native-name "Русский"}
   "rue"   {:name "Rusyn", :native-name "Русиньскый"}
   "sah"   {:name "Sakha", :native-name "Сахалыы"}
   "sa-in" {:name "Sanskrit", :native-name "भारतम्"}
   "srd"   {:name "Sardinian", :native-name "Sardu"}
   "gd"    {:name "Scottish Gaelic", :native-name "Gàidhlig"}
   "sr"    {:name "Serbian", :native-name "Српски језик"}
   "szl"   {:name "Silesian", :native-name "Ślōnskŏ gŏdka"}
   "snd"   {:name "Sindhi", :native-name "سنڌي"}
   "si"    {:name "Sinhala", :native-name "සිංහල"}
   "sk"    {:name "Slovak", :native-name "Slovenčina"}
   "sl"    {:name "Slovenian", :native-name "Slovenščina"}
   "so"    {:name "Somali", :native-name "Afsoomaali"}
   "azb"   {:name "South Azerbaijani", :native-name "گؤنئی آذربایجان"}
   "es-ar" {:name "Spanish (Argentina)", :native-name "Español de Argentina"}
   "es-cl" {:name "Spanish (Chile)", :native-name "Español de Chile"}
   "es-co" {:name "Spanish (Colombia)", :native-name "Español de Colombia"}
   "es-gt" {:name "Spanish (Guatemala)", :native-name "Español de Guatemala"}
   "es-mx" {:name "Spanish (Mexico)", :native-name "Español de México"}
   "es-pe" {:name "Spanish (Peru)", :native-name "Español de Perú"}
   "es-pr" {:name "Spanish (Puerto Rico)", :native-name "Español de Puerto Rico"}
   "es"    {:name "Spanish (Spain)", :native-name "Español"}
   "es-ve" {:name "Spanish (Venezuela)", :native-name "Español de Venezuela"}
   "su"    {:name "Sundanese", :native-name "Basa Sunda"}
   "sw"    {:name "Swahili", :native-name "Kiswahili"}
   "sv"    {:name "Swedish", :native-name "Svenska"}
   "gsw"   {:name "Swiss German", :native-name "Schwyzerdütsch"}
   "tl"    {:name "Tagalog", :native-name "Tagalog"}
   "tah"   {:name "Tahitian", :native-name "Reo Tahiti"}
   "tg"    {:name "Tajik", :native-name "Тоҷикӣ"}
   "ta"    {:name "Tamil", :native-name "தமிழ்"}
   "ta-lk" {:name "Tamil (Sri Lanka)", :native-name "தமிழ்"}
   "tt"    {:name "Tatar", :native-name "Татар теле"}
   "te"    {:name "Telugu", :native-name "తెలుగు"}
   "th"    {:name "Thai", :native-name "ไทย"}
   "bo"    {:name "Tibetan", :native-name "བོད་སྐད"}
   "tir"   {:name "Tigrinya", :native-name "ትግርኛ"}
   "tr"    {:name "Turkish", :native-name "Türkçe"}
   "tuk"   {:name "Turkmen", :native-name "Türkmençe"}
   "twd"   {:name "Tweants", :native-name "Twents"}
   "ug"    {:name "Uighur", :native-name "Uyƣurqə"}
   "uk"    {:name "Ukrainian", :native-name "Українська"}
   "ur"    {:name "Urdu", :native-name "اردو"}
   "uz"    {:name "Uzbek", :native-name "O‘zbekcha"}
   "vi"    {:name "Vietnamese", :native-name "Tiếng Việt"}
   "wa"    {:name "Walloon", :native-name "Walon"}
   "cy"    {:name "Welsh", :native-name "Cymraeg"}
   "yor"   {:name "Yoruba", :native-name "Yorùbá"}})

(defn build-locales [vocabulary-readers]
  (->>
    vocabulary-readers
    (mapv (fn [reader] (api/read-resource reader)))
    (group-by :locale-id)
    (map (fn [[locale-id vocs]]
           (let [locale-id (clostr/lower-case locale-id)]
             [locale-id
              (-> (get locale-infos locale-id)
                  (assoc :locale-id locale-id)
                  (assoc :voc
                         (->> vocs
                              (map :voc)
                              (apply merge))))])))
    (into {})))

(defn get-locale-id [localizer request]
  (when-let [value
             (get-in request
                     (api/get-conf localizer locale-request-path-key)
                     (api/get-conf localizer default-locale-key))]
    (clostr/lower-case value)))

(defn base-locale-id [locale-id]
  (first (clostr/split locale-id #"-")))

(defn locale-or-base [localizer locale-id]
  (let [locales (locales-key localizer)
        locale (get locales locale-id)]
    (if (seq (:voc locale))
      locale
      (let [base-id (base-locale-id locale-id)
            base (get locales base-id)]
        (if (seq (:voc base))
          base
          (assoc (get locale-infos locale-id
                      (get locale-infos base-id))
            :locale-id locale-id
            :voc {}))))))

(defrecord Localizer []
  api/IClientStateProvider
  (provide-client-state [localizer ring-request]
    (let [locale-id (get-locale-id localizer ring-request)]
      {:current-locale (locale-or-base localizer locale-id)}))
  IFn
  (invoke [localizer request q]
    (localizer request q 1))
  (invoke [localizer request q n]
    (let [locale-id (get-locale-id localizer request)
          locale (locale-or-base localizer locale-id)]
      (l10n/localize locale q n)))
  api/IModule
  api/IComponent
  (start [localizer]
    (->
      localizer
      (assoc locales-key (build-locales (readers-key localizer)))
      (dissoc readers-key)))
  (stop [localizator]
    localizator))

(defn serve-get-supported-locales [localizer request]
  (api/respond-success
    (:core localizer)
    request
    (->>
      (locales-key localizer)
      (mapv (fn [[_ locale]]
              (dissoc locale :voc))))))

(defn serve-get-locale [localizer request]
  (let [locale-id (:locale-id request)]
    (api/respond-success
      (:core localizer)
      request
      (get-in localizer [locales-key locale-id]
              (assoc (get locale-infos locale-id)
                :locale-id locale-id
                :voc {})))))

(defn new-localizer [vocabulary-readers & [m]]
  (map->Localizer
    (merge
      {api/id-key              :localizer
       readers-key             vocabulary-readers
       api/configuration-key   {default-locale-key      "en"
                                locale-request-path-key [:params :locale]}
       api/request-servers-key [{:msg-filter :localizer/get-supported-locales
                                 :handler    serve-get-supported-locales}
                                {:msg-filter :localizer/get-locale
                                 :handler    serve-get-locale}]}
      m)))
