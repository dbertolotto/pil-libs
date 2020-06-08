# Logging library
#(symbols 'logging 'pico)

(def '*LogLevels
  (list
   '(none . 0)
   '(fatal . 10)
   '(error . 20)
   '(warn . 30)
   '(info . 40)
   '(debug . 60)
   '(trace . 80)
   '(all . 100)))

(de log-prio (Level)
  (cdr (assoc Level *LogLevels)))

(de sender-prinl (@Out)
  (fill
    '((Message)
      (out @Out
        (prinl (fill-log-pattern Message))))))

(de sender-term ()
  (sender-prinl NIL))

(de sender-file (Path)
  (sender-prinl (pack "+" Path)))

(de senders @SendFns
  (fill
    '((Message)
      (for F '@SendFns
        (when F ((eval F) Message))))))

(de level (@Level @Logger)
  (fill
    '((Message)
      (let MsgLevel (cadr (assoc 'level Message))
        (when (<= (log-prio MsgLevel)
                  (log-prio '@Level))
          ('@Logger Message))))))

(de parse-log-pattern (Pattern)
  (ifn Pattern
    '(@M)
    (make
      (for (Stream (chop Pattern) Stream)
        (let C (pop 'Stream)
          (if (= C "@")
            (link (intern (pack C (pop 'Stream))))
            (link C)))))))

(de fill-log-pattern (Message)
  (let (Pattern (cadr (assoc 'pattern Message))
        ParsedPat (parse-log-pattern Pattern)
        Msg (cadr (assoc 'message Message))
        Level (cadr (assoc 'level Message))
        Date (cadr (assoc 'date Message))
        Time (cadr (assoc 'time Message))
        Usec (cadr (assoc 'usec Message))
        Zone (cadr (assoc 'zone Message))
        Pid (cadr (assoc 'pid Message))
        PPid (cadr (assoc 'ppid Message))
        NSpace (cadr (assoc 'nspace Message))
        @M Msg
        @L (uppc Level)
        @D (dat$ Date "-")
        @T (tim$ Time T)
        @U Usec
        @Z (tim$ Zone)
        @P PPid
        @p Pid
        @N NSpace)
    (pack (fill ParsedPat))))

(de pattern (@Pattern @Logger)
  (fill
    '((Message)
      (push 'Message (list 'pattern @Pattern))
      ('@Logger Message))))

(de timeinfo (@Logger)
  (fill
    '((Message)
      (let (Date (date T)
            Time (time T)
            Usec (usec T)
            Zone (tzo))
        (push 'Message (list 'date Date))
        (push 'Message (list 'time Time))
        (push 'Message (list 'usec Usec))
        (push 'Message (list 'zone Zone))
        ('@Logger Message)))))

(de sysinfo (@Logger)
  (fill
    '((Message)
      (push 'Message (list 'pid *Pid))
      (push 'Message (list 'ppid *PPid))
      (push 'Message (list 'nspace (car (symbols))))
      ('@Logger Message))))

(def '*Loggers '())

(de remove-logger (Id)
  (delete (assoc Id *Loggers) *Loggers))

(de get-logger (Id)
  (cadr (assoc Id *Loggers)))

(de logger (Id Logger)
  (push '*Loggers (list Id Logger)))

# minimal configuration
(def '*Logger 'root)
(logger 'root
  (level 'all
    (sender-term)))

(de logging-conf (Path)
  (load Path))

(de log (Level Message)
  ((get-logger *Logger)
   (list (list 'message Message)
         (list 'level Level))))

