(symbols 'syst 'pico)

(local) '(file? dir? exist?)

(de file? (Path)
  (num? (car (info Path))))

(de dir? (Path)
  (=T (car (info Path))))

(de exist? (Path)
  (member (basename Path) (dir (dirname Path) T)))

(local) '(nfile? ndir? nexist?)

(de nfile? (Path)
  (not (file? Path)))

(de ndir? (Path)
  (not (dir? Path)))

(de nexist? (Path)
  (not (exist? Path)))
