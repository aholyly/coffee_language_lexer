; *************************************************************************************
; *  341 Programming Languages                										  *
; *  Fall 2017                                										  *
; *  Author: Ahmet Mert Gülbahçe              										  *
; *************************************************************************************
; *  Bu program Coffee dilini, lexical analiz yaparak değişkenlerin           		  *
; *  keywordlerin, operatörlerin vb. dilin kurallarına göre parçalarına               *
; *  ayırma ve parçaları tanımlama işlemi yapmaktadır.             					  *
; *************************************************************************************

(ns lexer.core
	(:import [java.util LinkedList]) ;;Java Linked List
)

;; This function checks string is number or not
(defn numeric? [num]
	(if-let [num (seq num)]
		(let [
			num (if (= (first num) \.) (next num) num)
			num (drop-while #(Character/isDigit %) num)]
			(empty? num)
		)
	)
)
(def TokenList (new LinkedList)) ;;A new LinkedList variable

(defn lexer[filename] ;;Lexer funciton
	(def slurpFromfile (slurp filename)) ;;Read from file
	(def splitIntoTokens (clojure.string/split slurpFromfile #"\s")) ;;Split string

	(doseq [temp splitIntoTokens]
		(def number (numeric? temp))

		(cond
			(= temp "(" ) (.add TokenList (.concat "OPERATOR__" (.toString temp)))
			(= temp ")" ) (.add TokenList (.concat "OPERATOR__" (.toString temp)))
			(= temp "-" ) (.add TokenList (.concat "OPERATOR__" (.toString temp)))
			(= temp "+" ) (.add TokenList (.concat "OPERATOR__" (.toString temp)))
			(= temp "/" ) (.add TokenList (.concat "OPERATOR__" (.toString temp)))
			(= temp "*" ) (.add TokenList (.concat "OPERATOR__" (.toString temp)))
			(= temp "deffun" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "and" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "or" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "not" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "equal" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "append" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "concat" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "set" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "for" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "then" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "if" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "else" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "true" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "false" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= temp "while" ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
			(= number 1 ) (.add TokenList (.concat "KEYWORD_" (.toString temp)))
		:else (.add TokenList (.concat "IDENTIFIER_" (.toString temp))) 
		)
	)

	(let [tempToken TokenList]TokenList) ;;return value
)

(lexer "CoffeeSample.coffee") ;;function call

;;Print tokens to screen
(loop [x 0]
	(when (< x (count TokenList))
		(println (.get TokenList x))
		(recur(+ x 1)))
)