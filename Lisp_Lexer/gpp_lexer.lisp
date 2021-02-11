(setf word_list '())		
(setf is_letter 0)
(setf is_number 0)

(defun lexer(input)
	
	(with-open-file (stream "parsed_lisp.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)

		(setq i 0)
		(setq is_oc 1)
		(setq is_first 0)
		(setq flagValue 0)
		(setq control_value 0)
		(setq control_ident 0)


		(setq input (string-downcase input))

		(loop for i from 0 to (-(length input)1) 

			do 

			(setq is_char (char input i))

			(setq is_val 0)


			(if (and (equal is_char #\;) (equal (char input (+ i 1)) #\;))
				(progn
					(format stream "COMMENT ~%")
					(format t "COMMENT ~%")
				)
			)

			(if (equal (letter_control is_char) t) 
				(setf is_letter (+ is_letter 1) ) 
			)

			(cond
				((and (letter_control is_char) (>= is_letter 1) ) (push (string is_char) word_list)) 
				((and (equal (letter_control is_char) nil) (>= is_letter 1) )
					(setf is_letter 0)
					(if(and (equal (control_is_kw (string_concatenation ( setf word_list_reverse (reverse word_list)))) t) (equal (control_number (char input i) ) nil))
						(progn
							(setf is_key (string_concatenation (setf word_list_reverse (reverse word_list))))

							(if (equal is_key "and")
								(progn
									(format stream "KW_AND ~%" )
									(format t "KW_AND ~%" )

								)
							)
							(if (equal is_key "or")
								(progn
									(format stream "KW_OR ~%" )
									(format t "KW_OR ~%" )
								)
							)
							(if (equal is_key "not")
								(progn
									(format stream "KW_NOT ~%" )
									(format t "KW_NOT ~%" )
								)
							)
							(if (equal is_key "equal")
								(progn
									(format stream "KW_EQUAL ~%" )
									(format t "KW_EQUAL ~%" )
								)
							)
							(if (equal is_key "less")
								(progn
									(format stream "KW_LESS ~%" )
									(format t "KW_LESS ~%" )
								)
							)
							(if (equal is_key "nil")
								(progn
									(format stream "KW_NIL ~%" )
									(format t "KW_NIL ~%" )
								)
							)
							(if (equal is_key "list")
								(progn
									(format stream "KW_LIST ~%" )
									(format t "KW_LIST ~%" )
								)
							)
							(if (equal is_key "append")
								(progn
									(format stream "KW_APPEND ~%" )
									(format t "KW_APPEND ~%" )
								)
							)
							(if (equal is_key "concat")
								(progn
									(format stream "KW_CONCAT ~%" )
									(format t "KW_CONCAT ~%" )
								)
							)
							(if (equal is_key "set")
								(progn
									(format stream "KW_SET ~%" )
									(format t "KW_SET ~%" )
								)
							)
							(if (equal is_key "deffun")
								(progn
									(format stream "KW_DEFFUN ~%" )
									(format t "KW_DEFFUN ~%" )
								)
							)
							(if (equal is_key "for")
								(progn
									(format stream "KW_FOR ~%" )
									(format t "KW_FOR ~%" )
								)
							)
							(if (equal is_key "if")
								(progn
									(format stream "KW_IF ~%" )
									(format t "KW_IF ~%" )
								)
							)
							(if (equal is_key "exit")
								(progn
									(format stream "KW_EXIT ~%" )
									(format t "KW_EXIT ~%" )
								)
							)
							(if (equal is_key "load")
								(progn
									(format stream "KW_LOAD ~%" )
									(format t "KW_LOAD ~%" )
								)
							)
							(if (equal is_key "disp")
								(progn
									(format stream "KW_DISP ~%" )
									(format t "KW_DISP ~%" )
								)
							)
							(if (equal is_key "true")
								(progn
									(format stream "KW_TRUE ~%" )
									(format t "KW_TRUE ~%" )
								)
							)
							(if (equal is_key "false")
								(progn
									(format stream "KW_FALSE ~%" )
									(format t "KW_FALSE ~%" )
								)
							)
						)
						(progn
					        (format stream "IDENTIFIER ~%")
					        (format t "IDENTIFIER ~%")
					        (setq control_ident 1)
						)
					)
					(setf word_list_reverse '())
					(setf word_list '())
				)
			)

			(if (equal (control_number is_char) t)
				(progn
					(if (and (not (equal is_char #\0)) (equal (letter_control (char input (- i 1)) ) nil) (equal (control_number (char input (- i 1)) ) nil ))
						(progn
							(setq flagValue 1)
							(setq control_value 1)
						)
					)
					(if (and  (equal flagValue 1 ) (equal (control_number (char input (+ i 1)) ) nil ) (< (+ i 1) (length input) ))
						(progn
							(format stream "VALUE ~%")
							(format t "VALUE ~%")
							(setq control_value 1)
							(setq flagValue 0)
						)
					)
					(if (and (equal is_char #\0) (equal (control_number (char input (+ i 1)) ) nil ) (< (+ i 1) (length input) ) (equal (control_number (char input (- i 1)) ) nil ))
						(progn
							(format stream "VALUE ~%")
							(format t "VALUE ~%")
							(setq control_value 1)
						)
					)
						
					(if (and (equal control_value 0) (equal control_ident 0))
						(progn
							(format stream "SYNTAX ERROR ~a can not be tokenized. ~%" is_char)
							(format t "SYNTAX ERROR ~a can not be tokenized. ~%" is_char)
						)
					)
				)
			)
		
			(if (equal is_char #\")
				(progn
					(if (equal is_oc 1)
						(progn
							(format stream "OP_OC ~%")
							(format t "OP_OC ~%")
						 	(setf is_oc 0)
						)
						(progn
							(format stream "OP_CC ~%")
							(format t "OP_CC ~%")
						  	(setf is_oc 1)
						)
					)
				)
			) 

			(if (equal is_char #\*)
				(progn
					(if(equal (char input (+ i 1)) #\*)
						(progn
							(format stream "OP_DBLMULT ~%")
							(format t "OP_DBLMULT ~%")
							(setq i (+ i 1))
						)
						(progn
							(format stream "OP_MULT ~%")
							(format t "OP_MULT ~%")
						)
					)
				)
			) 
			(if (equal is_char #\+)
				(progn
					(format stream "OP_PLUS ~%" )
					(format t "OP_PLUS ~%" )
				)
			)
			(if (equal is_char #\-)
				(progn
					(format stream "OP_MINUS ~%" )
					(format t "OP_MINUS ~%" )
				)
			)
			(if (equal is_char #\/)
				(progn
					(format stream "OP_DIV ~%" )
					(format t "OP_DIV ~%" )
				)
			)
			(if (equal is_char #\()
				(progn
					(format stream "OP_OP ~%" )
					(format t "OP_OP ~%" )
				)
			)
			(if (equal is_char #\))
				(progn
					(format stream "OP_CP ~%" )
					(format t "OP_CP ~%" )
				)
			)
			(if(equal is_char #\, )
				(progn
					(format stream "OP_COMMA ~%" )
					(format stream "OP_COMMA ~%" )
				)
			)
		)
  	)
)

;;return true if it is keyword
(defun control_is_kw(my_key)
	(if (or 
		(equal my_key "and")
		(equal my_key "or")
		(equal my_key "not")
		(equal my_key "equal")
		(equal my_key "less")
		(equal my_key "nil")
		(equal my_key "list")
		(equal my_key "append")
		(equal my_key "concat")
		(equal my_key "set")
		(equal my_key "deffun")
		(equal my_key "for") 
		(equal my_key "if") 
		(equal my_key "exit") 
		(equal my_key "load") 
		(equal my_key "disp") 
		(equal my_key "true") 
		(equal my_key "false") )
		t
		nil
	)
)

;;controll letter for checking identifiers
(defun letter_control(my_ch)
	(if (or 
		(equal my_ch #\A)
	    (equal my_ch #\B)
	    (equal my_ch #\C)
	    (equal my_ch #\D)
	    (equal my_ch #\E)
	    (equal my_ch #\F)
	    (equal my_ch #\G)
	    (equal my_ch #\H)
	    (equal my_ch #\I)
	    (equal my_ch #\J)
	    (equal my_ch #\K)
	    (equal my_ch #\L)
	    (equal my_ch #\M)
	    (equal my_ch #\N)
	    (equal my_ch #\O)
	    (equal my_ch #\P)
	    (equal my_ch #\R)
	    (equal my_ch #\S)
	    (equal my_ch #\T)
	    (equal my_ch #\U)
	    (equal my_ch #\V)
	    (equal my_ch #\W)
	    (equal my_ch #\Y)
	    (equal my_ch #\Z)
	    (equal my_ch #\X)
	    (equal my_ch #\Q)
	    (equal my_ch #\a)
	    (equal my_ch #\b)
	    (equal my_ch #\c)
	    (equal my_ch #\d)
	    (equal my_ch #\e)
	    (equal my_ch #\f)
	    (equal my_ch #\g)
	    (equal my_ch #\h)
	    (equal my_ch #\i)
	    (equal my_ch #\j)
	    (equal my_ch #\k)
	    (equal my_ch #\l)
	    (equal my_ch #\m)
	    (equal my_ch #\n)
	    (equal my_ch #\o)
	    (equal my_ch #\p)
	    (equal my_ch #\r)
	    (equal my_ch #\s)
	    (equal my_ch #\t)
	    (equal my_ch #\u)
	    (equal my_ch #\v)
	    (equal my_ch #\w)
	    (equal my_ch #\y)
	    (equal my_ch #\z)
	    (equal my_ch #\x)
	    (equal my_ch #\q)
	    )	
		t
		nil
	)
)
;;controll numbers for values using ascii table.
(defun control_number(is_number)
	(if(and (<= (- (char-code is_number) 48) 9) (>= (- (char-code is_number) 48) 0) )
		t
		nil
	)
)
;;concantane string function
(defun string_concatenation (list)
	(if (listp list)
		(let ((output ""))
			(dolist (str list)
				(if (stringp str)
					(setf output (concatenate 'string output str))
				)
			)
			output
		)
	)
)

(defun read_file(filename)
	
	(let ((in (open filename :if-does-not-exist nil))(str (string "")) ) 
		(when in
			(loop for line = (read-line in nil) 
			    while line
					do 
					(if (and (equal(char line 0 ) #\; ) (equal(char line 1 ) #\; )) 
						(setq str (concatenate 'string str ";;" ) ) 
						(setq str (concatenate 'string str line)) 
					)	
			)
			(close in)
		)
		(setq str (concatenate 'string str " "))
		str
	)
)


(defun gppinterpreter()

	(setq command *args*)

	(setq isFile (length command))
	(setq filename (first command));

	(if (> isFile 0)
		(lexer (read_file filename ))
		(progn
			(loop 
				(setf repl (read-line)) 
				(if (= (length (string repl)) 0)
					(return)
					(lexer repl) 
				)
			)
		)
	)
)

(gppinterpreter)

