COMPONENTI DEL GRUPPO:

- Colonetti Fabio 
- Coldani Andrea


***______________________________PROGETTO LISP______________________________***



UTILIZZO DEI PREDICATI:

- def_class: 	(def-class <class-name> <parents> <parts>*)

	utilizzo:  definisce la struttura della classe e la aggiunge ad 
		   un'hash-table, in particolare la rappresenta con il nome 
		   della classe stessa, la lista delle sue superclassi ed 
		   eventualmente la lista di metodi ed attributi.
		   Sono state implementate tutte le regole di ereditarietà e 
		   di overloading tra classi e superclassi con i relativi 
		   controlli di tipo delle variabili.

	esempio:     	
			CL-USER 1 > (def-class 'person nil '(fields 
				     (name "Eve") (age 21 integer)))

			PERSON

			CL-USER 2 > (def-class 'student '(person) 
				    '(fields (name "Eva Lu Ator") 
				    (university "Berkeley" string)) '(methods 
			 	     (talk (&optional (out *standard-output*)) 
				     (format out "My name is ~A~%My age 
				     is ~D~%" (field this 'name) 
				     (field this 'age)))))
			
			STUDENT
		

- make:		(make <class-name> [<field-name> <value>]*)

	utilizzo:  crea una nuova istanza di classe, se passa i controlli da 
		   specifica ritorna come valore una lista del tipo ('oolinst 
		   (nome classe) (attributi)), inoltre per salvare l'istanza 
		   sull'ambiente globale viene usata assieme a defparameter 
		   (come da esempio sottostante).
			

	esempio:
			CL-USER 1 > (defparameter eve (make 'person))
			
			EVE

			CL-USER 2 > (defparameter s1 (make 'student 'name 
				     "Eduardo De Filippo" 'age 108))
			
			S1	


- is_class:	(is-class <class-name>)

	utilizzo:  controlla se l'atomo passatogli corrisponde al nome di 
		   una classe presente nella hash-table e in tal caso 
		   restituisce true.
	
	esempio:
			CL-USER 1 > (is-class 'person)
			
			T


- is_instance:	(is-instance <instance> &optional <class-name>)

	utilizzo:  verifica se il nome dell'istanza passatagli corrisponda 
		   con il nome di una istanza preesistente.
		   Qualora fosse inserito opzionalmente il nome di una 
		   classe verrà verificata inoltre che il nome della classe 
		   sia una superclasse alla quale appartiene l'istanza.

	esempio:
			CL-USER 1 > (is-instance eve)

			T

			CL-USER 2 > (is-instance s1 'person)

			T


- field:	(field <instance> <field-name>)

	utilizzo:  estrae e ritorna il valore di un campo da una classe 
		   attraverso il nome dell'istanza e il nome dell'attributo.
		   Inoltre controlla che gli attributi esistano nella classe 
		   e che siano del tipo corretto.	
	
	esempio: 
			CL-USER 1 > (field eve 'name)
			
			"Eve"


- field*:	(field* <instance> <field-name>+)

	utilizzo:  percorre elemento per elemento la lista degli attributi 
		   cercando il primo nell'istanza passatagli. Se nell'attributo 
		   trova un'altra istanza esistente rieffettua la ricerca su 
		   tale istanza con il secondo elemento della lista degli 
		   attributi e così via.
		   Il risultato è il valore dell'attributo associato 
		   all'ultima istanza. Se uno degli elementi di <field-name>+ 
		   non esiste ritorna un'errore. 
	
	esempio: 	
			Sia: c1 un'istanza con un field "other" = c2,
			     c2 un'istanza con un field "other" = c3,
			     c3 un'istanza con un field "name" = "Adam".

			CL-USER 1 > (field* c1 '(other other name))

			"Adam" 



***_______________________________END OF FILE_______________________________***
