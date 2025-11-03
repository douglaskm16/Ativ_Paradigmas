(ns core.integrador)

(defn ler-string [msg]

  (print msg) (flush)
  (let [s (read-line)]
    (if (nil? s) "" s)))

(defn ler-double [msg]

  (print msg) (flush)
  (try
    (Double/parseDouble (read-line))
    (catch Exception _ 0.0)))

(defn cadastrar-alunos []

  (loop [alunos []]
    (let [nome (ler-string "Nome (deixe vazio para sair): ")]
      (if (or (nil? nome) (= nome ""))

        alunos
        (let [nota (ler-double "Nota: ")
              aluno {:nome nome :nota nota}]
          (recur (conj alunos aluno)))))))

(defn acrescentar-status [aluno]

  (let [nota (:nota aluno)
        status (if (>= nota 6.0) "Aprovado" "Reprovado")]
    (assoc aluno :status status)))

(defn relatorio-notas [alunos]

  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [alunos-com-status (map acrescentar-status alunos)

          aprovados (filter #(= "Aprovado" (:status %)) alunos-com-status)
          media (if (pos? (count alunos))
                  (/ (reduce + (map :nota alunos)) (count alunos))
                  0.0)]

      (println "Lista de alunos (com status):")
      (doseq [a alunos-com-status]

        (println (:nome a) "- Nota:" (:nota a) "- Status:" (:status a)))

      (println)
      (println "Alunos aprovados:")
      (doseq [a aprovados]

        (println (:nome a) "- Nota:" (:nota a)))

      (println)
      (println "Média geral da turma:" (format "%.2f" media)))))

(defn estatisticas-gerais [alunos]

  (if (empty? alunos)
    (println "Nenhum aluno cadastrado.")
    (let [total (count alunos)
          alunos-com-status (map acrescentar-status alunos)
          aprovados (count (filter #(= "Aprovado" (:status %)) alunos-com-status))
          reprovados (- total aprovados)
          notas (map :nota alunos)
          maior (apply max notas)
          menor (apply min notas)
          media (/ (reduce + notas) total)]

      (println "Total de alunos:" total)
      (println "Aprovados:" aprovados)
      (println "Reprovados:" reprovados)
      (println "Maior nota:" maior)
      (println "Menor nota:" menor)
      (println "Média geral:" (format "%.2f" media)))))

(defn buscar-aluno [alunos nome-busca]

  (let [nf (clojure.string/lower-case nome-busca)
        encontrados (filter #(= nf (clojure.string/lower-case (:nome %))) alunos)]
    (if (empty? encontrados)
      (println "Aluno não encontrado.")
      (doseq [a encontrados]
        (let [a-st (acrescentar-status a)]
          (println "Nome:" (:nome a-st) "- Nota:" (:nota a-st) "- Status:" (:status a-st)))))))

(defn mostrar-menu []

  (println)
  (println "=== MENU PRINCIPAL ===")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatorio de Notas")
  (println "3 - Estatisticas Gerais")
  (println "4 - Buscar aluno pelo nome")
  (println "0 - Sair")
  (print "Escolha uma opção: ") (flush))

(defn -main []

  (loop [alunos []]
    (mostrar-menu)
    (let [entrada (read-line)
          opc (try (Integer/parseInt (or entrada "0")) (catch Exception _ -1))]
      (cond
        (= opc 0) (do (println "Saindo...") nil)
        (= opc 1) (let [novos (cadastrar-alunos)]
                    (recur (into alunos novos)))
        (= opc 2) (do (relatorio-notas alunos) (recur alunos))
        (= opc 3) (do (estatisticas-gerais alunos) (recur alunos))
        (= opc 4) (do (let [nome (ler-string "Nome para buscar: ")]
                        (buscar-aluno alunos nome))
                      (recur alunos))
        :else (do (println "Opção inválida.") (recur alunos))))))
