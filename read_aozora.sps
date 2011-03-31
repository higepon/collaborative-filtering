(import (rnrs)
        (mosh)
        (srfi :98)
        (only (srfi :1) take)
        (mosh file)
        (mosh control)
        (shorten)
        (mecab)
        (tfidf))

;(include "./text.dat")

(define
  (uniq lst)
  (let loop
       ((lst lst) (ret (quote ())))
       (cond ((null? lst) ret)
             (else (if (member (car lst) ret)
                       (loop (cdr lst) ret)
                       (loop (cdr lst) (cons (car lst) ret)))))))

(define corpus_dir (string-append (get-environment-variable "HOME") "/nltk_data/corpora/aozora"))

(define (corpus-name+path*)
  (map
   (^(name)
     (cons name (string-append corpus_dir "/" name)))
   (filter (^f (file-regular? (string-append corpus_dir "/" f))) (directory-list corpus_dir))))

(define news-text "オバマ米大統領は３０日、ワシントン市内のジョージタウン大学でエネルギー安全保障について演説、「米国は電力の５分の１を原子力エネルギーから得ている。原子力には大気中の二酸化炭素を増やすことなく電力を作る重要な能力がある」と指摘し、原発推進の姿勢を堅持する考えを表明した。　福島第一原発の事故で米国内でも原発の安全に懸念が高まっていることに対しては「安全は必ず確保する。調査結果や教訓を、次世代の原発の設計、建設に役立てる」と述べた。　また、中東政情が不安定になり原油価格が高騰している問題については「今から１０年少しで、石油輸入量を３分の１削減する」とし、２０２５年までに石油の輸入量の３分の１を減らす方針を示した。")

(let* ([m (mecab-new2 "")]
       [split-proc (lambda (document)
                     (let* ([text (string->utf8 document)]
                            [len (bytevector-length text)])
                       (mecab-node-surface* (mecab-sparse-tonode2 m text len))))])

  (let loop ([name+path* (take (corpus-name+path*) 1000)]
             [stat '()])
    (cond
     [(null? stat)
      (loop (cdr name+path*) (analyze1 (caar name+path*) (file->string (cdar name+path*)) split-proc))]
     [(null? name+path*)
      (analyze1 stat 'news news-text split-proc)
      (let1 word* (uniq (split-proc news-text))
        (for-each (^w (write w) (newline)) (take (list-sort (^(x y) (> (cdr x) (cdr y))) (map (^w (cons w (tf-idf stat 'news w))) word*)) 10)))]
     [else
      (loop (cdr name+path*) (analyze1 stat (caar name+path*) (file->string (cdar name+path*)) split-proc))])))
