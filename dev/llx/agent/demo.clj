(ns llx.agent.demo
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [llx.agent :as agent]
   [llx.agent.fx.inference :as inference]
   [llx.agent.loop :as loop]
   [llx.ai :as ai]
   [promesa.core :as p]
   [promesa.exec.csp :as sp]))

(set! *warn-on-reflection* true)

(def tap-log-file
  "tmp/llx-agent-demo.tap.edn")

(defn- append-line!
  [path value]
  (io/make-parents path)
  (spit path (str (pr-str value) "\n") :append true))

(defn make-file-tap-handler
  "Returns a tap handler that appends each tapped value as one EDN line."
  [path]
  (fn [value]
    (append-line! path value)))

(defn- start-event-forwarder!
  [runtime]
  (let [events> (agent/subscribe runtime)
        worker  (future
                  (loop []
                    (when-some [event (p/await (sp/take events>))]
                      (tap> event)
                      (recur))))]
    {:events> events>
     :worker  worker}))

(defn- stop-event-forwarder!
  [runtime {:keys [events> worker]}]
  (agent/unsubscribe runtime events>)
  (deref worker 1000 nil)
  nil)

(defn- step-and-collect!
  [state_ input]
  (let [[state' effects] (loop/step @state_ input)]
    (reset! state_ state')
    effects))

(defn- execute-fx-for-demo
  [env effect]
  (case (:llx.agent.fx/type effect)
    :emit-event
    (do
      (sp/offer (:events-mx> env) (:event effect))
      nil)

    :call-llm
    (inference/fx-call-llm env effect)

    :execute-tool
    (let [ch (sp/chan)]
      (sp/close ch)
      ch)

    :reject
    nil

    nil))

(defn- process-effects!
  [env effects]
  (reduce
   (fn [channels effect]
     (let [result (execute-fx-for-demo env effect)]
       (if (sp/chan? result)
         (conj channels result)
         channels)))
   []
   effects))

(defn- run-command!
  [env input]
  (let [state_  (:state_ env)
        effects (step-and-collect! state_ input)
        new-chs (process-effects! env effects)]
    (p/loop [active (set new-chs)]
      (when (seq active)
        (p/let [[val port] (sp/alts (vec active))]
          (if (nil? val)
            (p/recur (disj active port))
            (let [effects (step-and-collect! state_ val)
                  new-chs (process-effects! env effects)]
              (p/recur (into active new-chs)))))))))

(comment
  ;; Evaluate in REPL:
  ;; (require '[llx.agent.demo :as demo] :reload)
  ;; (demo/run-demo!)
  ;;

  (def  tap-handler (make-file-tap-handler tap-log-file))
  (io/make-parents tap-log-file)
  (spit tap-log-file "")
  (add-tap tap-handler)

  ;; To inspect taps inspect:
  ;; (slurp demo/tap-log-file)

  ;; create agent
  (def opts {:tool-defs      {}
             :system-prompt  "You are concise, but follow instructions carefully"
             :model          (ai/get-model :openai "gpt-5.2-codex")
             :thinking-level :high
             :tools          []})

  (def agent (agent/create-agent opts))

  (def forwarder (start-event-forwarder! agent))

  (p/await (agent/prompt agent [{:role      :user
                                 :content   "Give me a two-word codename for this conversation"
                                 :timestamp (System/currentTimeMillis)}]))

  (p/await (agent/prompt agent [{:role      :user
                                 :content   "Now explain that codename in one short sentence."
                                 :timestamp (System/currentTimeMillis)}]))

  (p/await (agent/prompt agent [{:role      :user
                                 :content   "Now repeat to me the first word of the codename in one message, then do some thinking,  then in a separate message, after thinking, send me the second part of the name."
                                 :timestamp (System/currentTimeMillis)}]))

  (p/await (agent/prompt agent [{:role      :user
                                 :content   "You didn't send the 2nd part, why not?"
                                 :timestamp (System/currentTimeMillis)}]))

  ;; after this:
  ;; messages are
  (-> agent :state_ deref :messages)
  [{:role      :user,
    :content   "Give me a two-word codename for this conversation",
    :timestamp 1771182680320}
   {:role                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        :assistant,
    :content
    [{:type                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     :thinking,
      :thinking                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "**Silent Echo**",
      :signature
      "{\"id\":\"rs_0e9832aec101a9510169921a58ffbc819e809a62d172ee1807\",\"type\":\"reasoning\",\"encrypted_content\":\"gAAAAABpkhpZcNA5AEWjIW_4-NJz_yiJhiqMR3DETeXDFu2fch8gyHgcf2iqCTOmBlfKEbySZSQW82Sz8yoRMaf_W9AIvaICZ8oRiOpXXLarVz61DruuXy9AeNiHgy9oOttszo4n4zCFGz9bmSmoRkkzIBrhisf7MNxYGHPRpFMiv2Wz2kt8QFCg-Y9--i8wt4C4ei6R9PAdBrCseyDPDdp3ZawTFOARqcBUK7oz82wT-YS5OAH8_8TXmckf84tmOB2CdDcnQQ9k1IyTsPuYTMALLCtx_8nwEMv6qTxD7zzHXh1z1n2gjv0qsFIbsabP4A4OW1UBUSKn_BITYvXIjHZwTAU8Wb3Ox5ZbI1_FUyJ2b9l4_BUqMI9e5AmFQlshsMCzvdHF72v4sQnob9CIjx99HWoTXcTOtPWWkk7hg8v1owg6W4737VBGttxx0rnPejb9zgQ__O_8OQZh9rWZW0Zyp--GGWcBfIBAN8_5SKOUhR4ygvgCjYFjSkjdTwO-xuHkHtaZkJqv4cxK5rjSfNE4WSJwvU9ZegnNFyoe0p5cIogUgPdl01hZeX8LYAXcskghMDCi8ffuQqEmH6rwtgupLlF3Yfd5SvpG2GAjk7tUqkzOJO9NtQ4ShbsxaDY6VHRmWzXbZrblODxDGpmzkx3cGsFYXrlKtKXMsTbTlek9edjN9XStVygz9GeloQd4bNTFZHp_Dto3AwSIzyand7V5mZQdh9H9GBkscSRuYd7L6r6afLOWh4-FGvYCN4iyKZhopZ98V3YEZlUNOv4BCR5QGurH-LRH3FbESZE_KAoWMqE3IX9_ls1cQ1L-P67pPsnx2TS43CCt697cs2Le8NGhGMu6JaCVdHCAQLtC6J4kV3yA6QnlYc_nxy2K8F0Bc_sswLsSkt6c\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"**Silent Echo**\"}]}"}
     {:type :text, :text "Silent Cipher"}],
    :api                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         :openai-responses,
    :provider                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    :openai,
    :model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "gpt-5.2-codex",
    :usage
    {:input             28,
     :output            23,
     :cache-read        0,
     :cache-write       0,
     :total-tokens      51,
     :cost
     {:input       0.000049,
      :output      0.000322,
      :cache-read  0,
      :cache-write 0,
      :total       0.000371}},
    :stop-reason                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 :stop,
    :timestamp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   1771182680611}
   {:role      :user,
    :content   "Now explain that codename in one short sentence.",
    :timestamp 1771182730718}
   {:role                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            :assistant,
    :content
    [{:type                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         :thinking,
      :thinking                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "",
      :signature
      "{\"id\":\"rs_0e9832aec101a9510169921a8b6a38819ea2ab53695840d35e\",\"type\":\"reasoning\",\"encrypted_content\":\"gAAAAABpkhqLJJQ2B18kkYCPtjrnIeqrYgA5tMcL_B8KyPgaOzvAgBRRuWeFOeL7KSkB0c6G02p6B7pht8ei080FA7VjOsP4yEJrs1jPh4qB4H4-LbC0_sn8bM8JxrJskqUrfwevt4fwkX49kBG0bdSzVYWeNQxBP5JzaHmvg_iTSwKcYjTCuMwMPSSKBSH2Q20ZyVhdGNQ5P7fRMrQPqhPZb3EOfEtgENqXlR3PZP28It4EQ71aXV7AUp9W9EIPsuTOL5OeJOOFI6-ueF-PMm4NI_ItKFTYRLFsBTwQvA3K1UbRpDnKTvbHTrQQZa_EMyGvlT9FIO1sCcmahsJOkbCY1f78dO2WYuI2VBgbZukUAtIENFw52UkrC4o8umGiOFr1Fenj56Y1j5Q9EQNKtuw3uqroTYP9MFj_OG7vK7vyhj4hCxE6zvGuRi-R7p7hgyssCDCpE-pEeldK8hLk9gjOyVUPD9YSqXb8WEqtuRCmUHCmmQlwkTuCArpJdR3CoQsQFnmRUYMQxJ3XvBgFfhaoiIQHJmEpiq8RVsL_UXB9hbtayUAWi45eU6NSmcYpMN6l_l2PdpyVhI_toWBZwrNJNiansDPvBABwVvD8u56QE5fv21UGf1gJlnuAfBgPspDB7hgzlSELsbzAI-KFLGHlIfFoSUU_M7HI8u3mL-SJHxIUJzabNDu4nhUGVjAr6d9v88melWt4sm2Gj6QICXJ8qNqC_pIQzlWX4aztJfNjfCcAzRduu1yzMOUeiyp64D1626sSoP8V2Fg-vlTFtEeEL3Idz-H9x9X9LttErq-73pwU8XZ2nEP2Wiu01-2tBK7M5erYRJNo7m7_7SsqJk-CzU7szl-nAcwI8WYR3IP9xpOmaEvrHJs=\",\"summary\":[]}"}
     {:type :text, :text "It suggests a quiet yet intricate exchange."}],
    :api                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             :openai-responses,
    :provider                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        :openai,
    :model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "gpt-5.2-codex",
    :usage
    {:input                          50,
     :output                         23,
     :cache-read                     0,
     :cache-write                    0,
     :total-tokens                   73,
     :cost
     {:input       0.0000875,
      :output      0.000322,
      :cache-read  0,
      :cache-write 0,
      :total       0.00040950000000000003}},
    :stop-reason                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     :stop,
    :timestamp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       1771182731047}
   {:role                                                                                                                                                                       :user,
    :content
    "Now repeat to me the first word of the codename in one message, then do some thinking,  then in a separate message, after thinking, send me the second part of the name.",
    :timestamp                                                                                                                                                                  1771182785334}
   {:role                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        :assistant,
    :content
    [{:type                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     :thinking,
      :thinking                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "",
      :signature
      "{\"id\":\"rs_0e9832aec101a9510169921ac2adec819e9375ca364866fd66\",\"type\":\"reasoning\",\"encrypted_content\":\"gAAAAABpkhrDKj4a3vBcJP8tuWq3VjbocD2FXjzLqMHIun5S1nDqs9gsTi2fY77dLczzMv5-TZUBZXPaukAvsd0jqvGtiv_QojlBgQBUG2w7-MXo2_h8OAAQp5grEEoZ7m37IGAvpdOUKYGeeTiyYS4-KQyJgmxZXb6uIu4UHfdXYpT5PLQUvMRorPM1pEi-8Cg2YIQiCi6vtq46SpPrIacWm2bE8IxMX8YFTSLiZ7Dqvbr6AFFDsT7zA7Jhig9MPkxEI9VU7aiDMvwB48C-KXhj4FUO6GbvKt5Z7FmjF25e4LjRftLMmxRp0f-DOptpF_tzBoUcAe3U_htNE35dl2yajORtJqK69K__hN2vjUJJXzzQ80lz_Ca0Y6_aggtZt0_QQbgquIUK_Dz5pWMBBnlYf6S04nx211iQyUUs_pPXNIchwriFOUWD5udwXlI0o6Qt_o04cn7hRpvwIld5bHu1FfvPGxDEraMyfjHNaJj9DmZpJ26sklR2pn5tEoQx5iHlASZxakzoNDkOD6NzEcEj0_kegzxrydzHH-crYkdy6PzJn-FG_jp-xvkrXp7hOM9T2ouuEndxDz2ZcNG1yQlysTofABX8tGrbRlV6a2j2nmFQaZW1y-p7U1ULC7hVRYrdVWI1GW__izyJ3rlRjWpebwzu6zHw4eaz1D24nKyMV7Sms6hVIonSBCXBUbj_SHm5Xw6Ays01KSZJ_9pV8kWob4HFR4tq75sYjomd8yDWwN-FB8VmGlOGRGrNx4cRtyklGm1azDkJNsA867WhrvSjVYPDXAoSd3qTtpGF9xvgywyk3EBYENfaKbH4T29EkT5S_s67SshJkLzUWekOf5WGzC7axBXNssypv3zFPFG8fcaPxRZVt1esfuj0BcJ8Mx-tHsi-uifGYz2zle9xz7xEpd5ggKaUCqn6Y7bCvhn2oweL8cqNjZrcPKRMAFOhGv0-0zLm-E3oU-Mn34Q8rVjB50PPCah8hvQbXVAvaxW5cpjyaF3UYwd9LRD8EqKVGZ3uAMtIpAKgGvAp6STyHF23j7pp340UKQ==\",\"summary\":[]}"}
     {:type :text, :text "Silent"}],
    :api                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         :openai-responses,
    :provider                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    :openai,
    :model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       "gpt-5.2-codex",
    :usage
    {:input              107,
     :output             44,
     :cache-read         0,
     :cache-write        0,
     :total-tokens       151,
     :cost
     {:input       0.00018725,
      :output      0.000616,
      :cache-read  0,
      :cache-write 0,
      :total       0.00080325}},
    :stop-reason                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 :stop,
    :timestamp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   1771182786195}
   {:role      :user,
    :content   "You didn't send the 2nd part, why not?",
    :timestamp 1771182820990}
   {:role                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                :assistant,
    :content
    [{:type                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             :thinking,
      :thinking                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "**Clarifying response timing**\n\nI need to send now.",
      :signature
      "{\"id\":\"rs_0e9832aec101a9510169921ae5b988819eb7dbc8faca717565\",\"type\":\"reasoning\",\"encrypted_content\":\"gAAAAABpkhrmQ3EZ3T2hmv7zZdZDxOvv8Eg4XqMT5z2GYaXaWhtwucCpI6N3ImzMRvrR4V82KjzPmTU35TIRyQCtCuBZcOZOQHsygqmApY0CGbLjA3iDtnB6JSXDTDY5iyUpF_99uJhrXOHBfIxm1XdlLWcBCLJFmYHRIYz0IkqGrRsm9hdybCQITETFWlcbSRQkgwUEEmhfV40UJevps8JJGO3mZEXwEswXSy2TMTGeDz1wNcVG_LnyfQGoMCZWelM_53h7HgHFcqQzB4w3RWU13DrDOCF2V9YKVRfkFr8ae9S3_9J0mbJMG4vnZomShHi9XPr6dJ_6CS0cw18wmRymq9dlTerxky1ja289Yr5HzvuKJkyVEIfEAYaBiHvlX3Dc_NvQu9vBSXlYer02nznUfkMTmiNRtEO__BZMJ_OCy5hkytsjzmYOLlZBVWJ5K5rjxyTUAkGUUw-whjUVfoGZE42X5qmf9zCcGUkmCglyWBfd0mmBZCrJ75dsyzJYLXfbRlRG0D0ijiC9RxhluO7Sz51VOct8HWRsMXwdesCG7e9Cgd8amu83Ed-SNNRO7Sr86BDqxZKIjM67DzcBhAnj0k5y8S3qffXlx8m0627n_vGRHBDwfFC5QOhWWnZ8T8z0u0y5D57W-dQv3Jd2kd-aiRKJQB4Q9whQkYfGa8V0nBvApt7acgpBKOZ1A8xPcwJTmLZ6Hq03xpUoKZ6sredKh2efhUQq8EYNgOV_vhhMenZd62BGw-TJol8sMmL5zK87WnVOjTo8fUSmotB45_8xpTeVDcErwN8Q8bShw0YO-2haA5k2x0tVZlG9S3NR5XyarRX18_4PsVoNrISz6sxSRKw1edk0S49O9BYS4rnD1ylFiB9ItTTsPoF8Ma9V_dvUGxp19wh6Ro3HajdCzUaK_fbf_DvHaxNNjQqMlfP3yHRc0NRwUQK1N8erS5jrZcLGuVwmfTxankRWzc_ksJ58k8bTVAdA62dWMX9TRekuh2uc32h_aB8y-d7oZOW079xA0PvpSxku\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"**Clarifying response timing**\\n\\nI need to send now.\"}]}"}
     {:type :text,
      :text "I only sent the first part; here’s the second: Cipher."}],
    :api                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 :openai-responses,
    :provider                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            :openai,
    :model                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "gpt-5.2-codex",
    :usage
    {:input                          130,
     :output                         55,
     :cache-read                     0,
     :cache-write                    0,
     :total-tokens                   185,
     :cost
     {:input       0.00022749999999999997,
      :output      0.0007700000000000001,
      :cache-read  0,
      :cache-write 0,
      :total       0.0009975000000000001}},
    :stop-reason                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         :stop,
    :timestamp                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           1771182821278}]

  (do ;; cleanup after every run
    (stop-event-forwarder! agent forwarder)
    (p/await (agent/close agent)))
  ;;
  )
