;;;; Colonetti Fabio 
;;;; Coldani Andrea 

;;;; -*- Mode: Lisp -*-
;;;; ool.lisp

;;; Funzioni per gestione delle hashtable
(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun class-spec (name)
  (gethash name *classes-specs*))

;;; Primitiva def-class
;;; definisce la struttura della classe
(defun def-class (class-name parents &rest part)
  (cond ((not (symbolp class-name))
         (error "la classe non è un simbolo"))
        ((is-class class-name)
         (error "la classe è già stata definita"))
        ((not (listp parents))
         (error "parents non è una lista"))
        ((not (parents-is-correct parents))
         (error "parents non è nella giusta forma"))
        ((not (part-is-correct part parents))
         (error "part non è nella giusta forma")))
  (add-class-spec class-name 
                  (final-class class-name 
                               parents 
                               (return-part part) 
                               (create-new-part part)))
  class-name)

;; ritorna il field della classe
(defun return-part (part)
  (cond ((null part) '())
        ((equal (first (first part)) 'fields)
         (first part))
        (t (return-part (rest part)))))

;; prima di add-class-spec creo già la lista
;; da aggiungere all'hash table
(defun final-class (class-name parents part1 part2)
  (append (list class-name)
          (list parents
                (append 
                 (list part1) 
                 (list part2)))))
        
;;; controllo se class-name è una classe
(defun is-class (class-name)
  (if (class-spec class-name)
      T
      nil))

;;; controllo che i genitori siano simboli e classi esistenti 
(defun parents-is-correct (parents)
  (cond ((null parents)
         T)
        ((not (symbolp (first parents))) 
         (error "La lista parents non è composta da simboli"))
        ((not (is-class (first parents)))
         (error "Parents non è composta da classi esistenti"))
        (T (parents-is-correct (rest parents)))))

;;; controllo che part sia scritto nel modo corretto
;;; cioè che sia un insieme di liste che iniziano con
;;; fields oppure methods
(defun part-is-correct (part parents)
  (cond ((null part) T)
        ((equal (first (first part)) 'fields) 
          (and (field-is-correct (rest (first part)) parents)
               (part-is-correct (rest part) parents)))
        ((equal (first (first part)) 'methods) 
          (and (method-is-correct (rest (first part)))
               (part-is-correct (rest part) parents)))
        (T (part-is-correct (rest part) parents))))

;;; controllo che field sia scritto nel modo corretto,
;;; cioè che contenga triple del tipo 
;;; (field-name, value, type) e verifico che il 
;;; type corrisponda a quello delle superclassi
(defun field-is-correct (field parents)
  (cond ((null field) T)
        ((not (symbolp (first (first field)))) 
         (error "Errore in field-name"))
        ((not (atom (second (first field))))
         (error "Errore in value"))
        ((not (equal (third (first field)) nil)) 
         (and (type-is-correct
               (all-attributes parents)
               (first (first field))
               (third (first field)))
              (field-is-correct (rest field) parents)))
        (T (field-is-correct (rest field) parents))))

;; verifica che i tipi degli attributi siano corretti
;; con la funzione subtypep        
(defun type-is-correct (att-parents att type)
  (cond ((null att-parents) T)
        ((equal (first (first att-parents)) att)
         (if (null (third (first att-parents)))
             T)
         (subtypep type (third (first att-parents))))
        (T (type-is-correct (rest att-parents) att type))))

;;; methods deve essere del tipo (method-name, arglist, form)
(defun method-is-correct (method)
  (cond ((null method) T)
        ((not (symbolp (first (first method))))
         (error "Errore in method-name"))
        ((not (listp (second (first method))))
         (error "Errore in arglist"))
        (T (method-is-correct (rest method)))))


;;; Primitiva make
;;; controllo che class-name sià una classe già definita
;;; controllo che make fields sia corretto
;;; creo la lista da usare con defparameter
(defun make (class-name &rest make-fields)
  (cond ((not (is-class class-name))
         (error "class-name non è una classe"))
        ((not (make-fields-is-correct class-name make-fields))
         (error "i parametri non sono corretti"))
        ((not (control-type class-name make-fields))
         (error "i tipi sono errati"))
        (T (append (list 'oolinst) 
                   (append (list class-name) 
                           make-fields)))))

;;; controllo che i field esistano nella classe o nelle superclassi
(defun make-fields-is-correct (class-name make-fields)
  (cond ((null make-fields)
         T)
        (T (and (make-field-is-correct 
                 class-name (first make-fields))
                (make-fields-is-correct 
                 class-name (cddr make-fields))))))

;; controllo che i tipi siano corretti, 
;; cioè che equivalgano a quelli della classe
(defun control-type (class-name make-fields)
  (cond ((null make-fields)
         T)
        (T (and (type-control 
                 (rest (first (third (class-spec class-name))))
                 (first make-fields) 
                 (second make-fields)
                 class-name)
                (control-type class-name (cddr make-fields))))))
  
;; implementato per semplificare control-type 
;; è il metodo che effettivamente controlla
;; che i tipi siano equivalenti                   
(defun type-control (class adj type class-name)
    (cond ((null class)
           (type-control-sup 
            (all-class-superclasses class-name) adj type))
          ((is-instance type) t) ; controllo se è  un'istanza
          ((equal adj (first (first class)))
           (equal (type-of type) 
                  (type-of (second (first class)))))
          (T (type-control (rest class) adj type class-name))))

;; funzione che recupera il fields di una superclasse
(defun sup-part (superclass)
  (cond ((null superclass) '())
        ((equal (first (first superclass)) 'fields)
         (rest (first superclass)))
        (t (sup-class (rest superclass)))))

;; se non trova l'attributo lo cerca anche nelle superclassi 
(defun type-control-sup (superclasses adj type)
  (cond ((null superclasses)
         nil)
        (T (cond ((sup-type-control
                   (sup-part 
                    (third (class-spec (first superclasses))))
                   adj
                   type)
                  T)
                 (T (type-control-sup 
                     (rest superclasses) adj type))))))

;; confronta i tipi delle superclassi
(defun sup-type-control (class adj type)
  (cond ((null class)
           nil)
        ((equal adj (first (first class)))
         (equal (type-of type) (type-of (second (first class)))))
        (T (sup-type-control (rest class) adj type))))
        
;; controlla che gli attributi esistano nella classe
(defun make-field-is-correct (class-name field)
  (let ((attributi-classe (flatten
                           (third (class-spec class-name)))))
    (cond ((not (equal (member field attributi-classe) nil))
           T)
          ((control-in-superclasses (all-class-superclasses class-name)
                                    field)))))

;; se non esistono nella classe controlla se esistono
;; nelle superclassi
(defun control-in-superclasses (superclasses field)
  (let ((lista (flatten (lista-att superclasses))))
    (cond ((not (equal (member field lista) nil))
           T))))

;; prende tutti gli attributi delle superclassi
(defun lista-att (superclasses)
  (cond ((null superclasses)
         nil)
        ((append (third (class-spec (first superclasses)))
                 (lista-att (rest superclasses))))))
         

;; is-instance controlla se l'istanza passata 
;; è un'istanza esistente, e se come parametri 
;; opzionali può avere T oppure una superclasse
(defun is-instance (value &optional (class-name T))
  (cond ((not (listp value)) nil)
        ((and (equal class-name T)
              (equal (first value) 'oolinst))
         T)
        ((and (superclass class-name (second value))
              (equal (first value) 'oolinst))
         T)
        (T nil)))

;; verifica che la prima classe passata per
;; parametri sia una superclasse
(defun superclass (superclass subclass)
  (let ((superclasses (second (class-spec subclass))))
    (cond ((not (equal (member superclass superclasses) nil))
           T)
          (T nil))))

;; crea una lista con tutte le superclassi di una classe
(defun all-class-superclasses (class-name)
  (cond ((null class-name) nil)
        (T (let ((superclasses (second (class-spec class-name))))
             (superclasses-in-depth-order superclasses)))))

;; ordina le superclassi per la ricerca in profondità
(defun superclasses-in-depth-order (superclasses)
  (cond ((null superclasses)
        nil)
        (T (append (list (first superclasses)) 
                   (append 
                    (superclasses-in-depth-order 
                     (second (class-spec (first superclasses))))
                    (superclasses-in-depth-order 
                     (rest superclasses)))))))

                        
;;; field: prima cerco l'attributo nell'istanza 
;;; poi lo cerco nella classe e infine se non 
;;; l'ho trovato lo ricerco nelle superclassi
;;; in ordine di profondità
(defun field (instance field-name)
  (if (and (is-instance instance)
           (symbolp field-name))
      (let ((attributi instance))
        (find-value attributi field-name (second attributi)))))

;; funzione cerca nell'istanza l'attributo e 
;; in caso non lo trovi chiama value-find
(defun find-value (attributi field-name class-name)
  (cond ((null attributi)
         (value-find field-name 
                        class-name
                        (third (class-spec class-name))))
        ((equal (first attributi) field-name)
         (first (rest attributi)))
        ((find-value (rest attributi) field-name class-name))))

;; dagli attributi seleziona solo i fields per
;; chiamare find-in-class cioè la ricerca nella classe
(defun value-find (field-name class-name class)
  (cond ((null class)
         (find-in-class field-name class-name nil))
        ((equal (first (first class)) 'fields)
         (find-in-class field-name 
                        class-name
                        (rest (first class))))
        (T (value-find field-name class-name (rest class)))))

;; cerca il field-name nella classe e se lo trova
;; ritorna il suo valore
(defun find-in-class (field-name class-name att-class)
  (cond ((null att-class)
         (find-in-superclasses 
          field-name                      
          (all-class-superclasses class-name)))
        ((equal (first (first att-class)) field-name)
         (second (first att-class)))
        ((find-in-class field-name class-name (rest att-class)))))

;; se non ha trovato il valore nella classe lo
;; ricerca nelle superclassi       
(defun find-in-superclasses (field-name superclasses)
  (cond ((null superclasses)
         nil)
        ((find-att field-name (all-attributes superclasses)))
  ))

;; funzione di supporto che restituisce tutti gli
;; attributi delle classi passate
(defun all-attributes (classes)
  (cond ((null classes)
         nil)
        ((append (rest (first (third (class-spec (first classes)))))
                 (all-attributes (rest classes))))))

;; funzione che effettua realmente la
;; ricerca l'attributo nelle superclassi
(defun find-att (field-name attributi) 
  (cond ((null attributi)
         nil)
        ((equal (first (first attributi)) field-name) 
         (second (first attributi)))
        ((find-att field-name (rest attributi)))))

;;; FIELD* percorre una serie di attributi 
;;; e estrae il valore dall'istanza finale
;;; se passa i controlli chiama apply-field*
(defun field* (instance field-names)
  (cond ((not (is-instance instance))
         (error "Instance non è un'istanza"))
         ((not (listp field-names))
          (error "Field-names non è una lista"))
         (t (apply-field* instance field-names))))

;;; se la lista è di lunghezza 1 chiama field 
;;; sull'attributo, invece se il valore dell'attributo 
;;; cercato è un'istanza applica ricorsivamente la ricerca 
;;; a tale istanza sul resto del field-names
(defun apply-field* (instance field-names)
  (cond ((equal (length field-names) 1) 
         (field instance (first field-names))) 
        ((is-instance (field instance (first field-names))) 
         (apply-field* (field instance (first field-names)) 
                       (rest field-names))) 
         (t (field instance (first field-names)))))


;; funzione che "appiattisce" una lista
(defun flatten (lista)
   (cond ((null lista) nil)
         ((atom lista) (list lista))
         (t (append (flatten (first lista))
                    (flatten (rest lista))))))

;; METODI
;; funzione che seleziona da part 
;; solamente il campo methods e lo
;; passa a modify methods
(defun create-new-part (part)
  (cond ((null part)
         nil)
        ((equal (first (first part)) 'methods)
         (append (list 'methods) 
                 (modify-methods (rest (first part)))   
                 (create-new-part (rest part))))
        (t (create-new-part (rest part)))))

;; chiama process method e restituisce
;; la ridefinizione del metodo con la lambda
(defun modify-methods (methods)
  (if (null methods)
      '()
      (append (list (append 
                     (list (first (first methods)))
                     (list (process-method (first (first methods))
                                           (rest (first methods))))))
	      (modify-methods (rest methods)))))


;; funzione per gestione metodi
;; prima creo la lambda che recupera il metodo dall'istanza
;; poi la associo al nome del metodo 
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
	(lambda (this &rest args)
	  (apply (find-methods this method-name) 
                 (append (list this) args))))
  (eval (rewrite-method-code method-spec)))

;; prende in input una s-expression e la riscrive 
;; per ricevere in input anche un parametro this
(defun rewrite-method-code (method-spec)
  (cons 'lambda (cons (append (list 'this) (first method-spec))
                       (cdr method-spec))))

;; funzione che restituisce il corpo del metodo cercato
;; con il nome dello stesso                                          
(defun find-methods (instance method-name)
  (if (is-instance instance)
      (trova-methods 
       (second instance)
       (third (class-spec (second instance))) 
       method-name)))

;; riscrittura dei parametri per la ricerca
(defun trova-methods (class-name attributi-classe method-name)
  (cond ((null attributi-classe)
         (find-methods-in-class class-name nil method-name))
        ((equal (first (first attributi-classe)) 'methods)
         (find-methods-in-class 
          class-name
          (rest (first attributi-classe))
          method-name))
        (T (trova-methods 
            class-name (rest attributi-classe) method-name))))

;; ricerca nella classe, se non trova il metodo 
;; lo ricerca nelle superclassi
(defun find-methods-in-class (class-name class method-name)
  (cond ((null class)
         (methods-in-superclass 
          (all-class-superclasses class-name)
          method-name))
        ((equal (first (first class)) method-name)
         (first (rest (first class))))
        (T (find-methods-in-class class-name 
                                  (rest class) method-name))))

;; funzione che recupera il campo methods di una superclasse
(defun sup-method (superclass)
  (cond ((null superclass) '())
        ((equal (first (first superclass)) 'methods)
         (rest (first superclass)))
        (t (sup-method (rest superclass)))))

;; ricerca del metodo nelle superclassi
;; se non lo trova restituisce nil
(defun methods-in-superclass (superclasses method-name)
  (cond ((null superclasses)
         nil)
        ((equal (find-method-in-superclass 
                 (sup-method (third (class-spec (first superclasses))))
                 method-name) nil)
         (methods-in-superclass (rest superclasses) (method-name)))
        (t (find-method-in-superclass
            (sup-method (third (class-spec (first superclasses))))
            method-name))))

;; restituisce il corpo del metodo cercato
(defun find-method-in-superclass (class method-name)
  (cond ((null class)
         nil)
        ((equal (first (first class)) method-name)
         (first (rest (first class))))
        (T (methods-in-class (rest class) method-name))))


;;;; end of file -- ool.lisp --
