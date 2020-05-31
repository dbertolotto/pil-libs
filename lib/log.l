#(symbols 'log2 'pico)

(def '*LogErr 20)
(def '*LogInf 40)
(def '*LogDbg 80)
(def '*LogAll 100)

(def '*LogLevel *LogInf)

(de get-log-level ()
  *LogLevel)

(de prinl-logger (@Out)
  (fill
    '((Message)
      (out @Out (prinl Message)))))

(de term-logger ()
  (prinl-logger NIL))

(de file-logger (Path)
  (prinl-logger (pack "+" Path)))

(de multi-logger @LogFns
  (fill
    '((Message)
      (for F '@LogFns
        (when F ((eval F) Message))))))

(de log-level (@Level @Logger)
  (fill
    '((Message)
      (when (<= @Level (get-log-level))
        ('@Logger Message)))))

(de parse-log-pattern (Pattern)
  (make
    (for (Stream (chop Pattern) Stream)
      (let C (pop 'Stream)
        (if (= C "@")
          (link (intern (pack C (pop 'Stream))))
          (link C))))))

(de replace-log-pattern (Pattern Message)
  (let (@M Message
        @T (stamp)
        @P *PPid
        @p *Pid)
    (fill Pattern)))

(de fill-log-pattern (Pattern Message)
  (pack (replace-log-pattern (parse-log-pattern Pattern) Message)))

(de log-pattern (@Pattern @Logger)
  (fill
    '((Message)
      ('@Logger (fill-log-pattern @Pattern Message)))))

(def '*Loggers '())
(de add-logger (Logger)
  (push '*Loggers Logger))
(de del-logger (Id)
  (delete (assoc Id *Loggers) *Loggers))
(de logger (Id)
  (cadr (assoc Id *Loggers)))

(def 'RootLogger
  (list ':root
    (log-pattern "@T | @M"
      (log-level *LogInfo
        (term-logger)))))
(add-logger RootLogger)

(def 'CustomLogger
  (list ':custom
    (log-pattern "@T | @M"
      (multi-logger
        (log-level *LogInf
          (term-logger))
        (log-level *LogDbg
          (file-logger "app.log"))))))
(add-logger CustomLogger)

(de log (Message)
  ((logger ':root) Message))
#  ((logger ':root) (list (list ':message Message)))
