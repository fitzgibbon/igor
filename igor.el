;;; igor.el --- A package that depends on the request library -*- lexical-binding: t -*-

;;; Commentary:

;; This package requires the "request" library to be installed.

;;; Code:
(require 'cl-lib)
(require 'eldoc)
(require 'xref)

(require 'request)

(defvar igor-lru-cache-capacity 10000
  "The maximum capacity of the LRU cache.")
(defvar igor-lru-cache (make-hash-table :test 'equal)
  "The LRU cache implemented as a hash table.")
(defvar igor-lru-cache-queue '()
  "The LRU cache queue implemented as a list of key hashes.")

(defun igor-lru-cache-hash-key (key)
  "Computes the HASH of the given KEY."
  (sxhash-equal key))

(defun igor-lru-cache-put (key value)
  "Puts a KEY - VALUE pair into the LRU cache."
  (let ((key-hash (igor-lru-cache-hash-key key)))
    (when (>= (hash-table-count igor-lru-cache) igor-lru-cache-capacity)
      (let ((oldest-hash (car igor-lru-cache-queue)))
        (remhash (car (rassoc oldest-hash igor-lru-cache)) igor-lru-cache)
        (setq igor-lru-cache-queue (cdr igor-lru-cache-queue))))
    (setq igor-lru-cache-queue (append igor-lru-cache-queue (list key-hash)))
    (puthash key-hash value igor-lru-cache)))

(defun igor-lru-cache-get (key)
  "Gets the VALUE associated with the given KEY from the LRU cache."
  (let* ((key-hash (igor-lru-cache-hash-key key))
         (value (gethash key-hash igor-lru-cache)))
    (when value
      (setq igor-lru-cache-queue (delete key-hash igor-lru-cache-queue))
      (setq igor-lru-cache-queue (append igor-lru-cache-queue (list key-hash))))
    value))

(defun igor-lru-cache-remove (key)
  "Remove the item with the given KEY from the LRU cache."
  (let ((key-hash (igor-lru-cache-hash-key key)))
    (remhash key-hash igor-lru-cache)
    (setq igor-lru-cache-queue (delete key-hash igor-lru-cache-queue))))

(defun igor-lru-cache-count (predicate)
  "Counts the number of entries in the LRU cache that match a specific PREDICATE."
  (cl-count-if predicate igor-lru-cache))

(defgroup igor nil
  "Customizations for the igor package."
  :prefix "igor-"
  :group 'convenience)

(defcustom igor-openai-token ""
  "Your OpenAI API token."
  :type 'string
  :group 'igor)

(defcustom igor-openai-model "gpt-3.5-turbo"
  "The OpenAI model to use."
  :type 'string
  :group 'igor)

(defcustom igor-openai-temperature 1.0
  "The temperature used in OpenAI requests."
  :type 'float
  :group 'igor)

(defcustom igor-openai-response-max-tokens 1024
  "The max tokens wanted in OpenAI responses."
  :type 'integer
  :group 'igor)

(defvar-local json-read-plist
    (lambda ()
      (let ((json-object-type 'plist))
        (json-read))))

(defun igor-retrieve-openai-models ()
  "Retrieve all available OpenAI models."
  (let* ((response
          (request
           "https://api.openai.com/v1/models"
           :type "GET"
           :headers `(("Authorization" . ,(concat "Bearer " igor-openai-token)))
           :parser json-read-plist
           :sync t))
         (models (plist-get (request-response-data response) :data)))
    (mapcar (lambda (model) (plist-get model :id)) models)))

;;;###autoload
(defun igor-select-model (&optional initial-model)
  "Prompt the user to select an OpenAI model and store it in `igor-openai-model'.
If INITIAL-MODEL is provided, set the model to that without prompting the user."
  (interactive)
  (if initial-model
      (setq igor-openai-model initial-model)
    (let* ((models (igor-retrieve-openai-models))
           (selected-model (completing-read "Select an OpenAI model: " models)))
      (setq igor-openai-model selected-model))))

(defun igor-chat-custom (chat-turns model temperature max-tokens callback)
  "Request an OpenAI chat for the given CHAT-TURNS, MODEL, TEMPERATURE and MAX-TOKENS and call CALLBACK function on completion."
  ;; (message "OpenAI request with chat turns: %s" chat-turns)
  (let* ((cache-key (list chat-turns model temperature max-tokens))
         (cached (igor-lru-cache-get cache-key)))
    (if cached
        (cl-case
         (car cached)
         (pending (igor-lru-cache-put cache-key (list 'pending (cons callback (cdr cached)))))
         (present (funcall callback (car (cdr cached)))))
      (progn
        (igor-lru-cache-put cache-key (list 'pending (list callback)))
        (request
         "https://api.openai.com/v1/chat/completions"
         :type "POST"
         :headers `(("Content-Type" . "application/json") ("Authorization" . ,(concat "Bearer " igor-openai-token)))
         :encoding 'utf-8
         :data (json-encode `(:model ,model :messages ,chat-turns :temperature ,temperature :max_tokens ,max-tokens))
         :parser json-read-plist
         :success
         (cl-function
          (lambda (&key data &allow-other-keys)
            (let ((choices (plist-get data :choices)))
              (when choices
                (let* ((choice (aref choices 0))
                       (text (plist-get (plist-get choice :message) :content)))
                  (progn
                    (mapc (lambda (watcher) (funcall (car watcher) text)) (cdr (igor-lru-cache-get cache-key)))
                    (igor-lru-cache-put cache-key (list 'present text))))))))
         :error
         (cl-function
          (lambda (&key data &allow-other-keys)
            (igor-lru-cache-remove cache-key)
            (let ((error (plist-get data :error)))
              (when error
                (message "OpenAI API error: %s" error))))))))))

(defun igor-chat (chat-turns callback)
  "Request an OpenAI chat for the given CHAT-TURNS and call CALLBACK function on completion."
  (igor-chat-custom chat-turns igor-openai-model igor-openai-temperature igor-openai-response-max-tokens callback))

(defun igor--pandoc-convert (input-format output-format buffer)
  "Convert the content of BUFFER from INPUT-FORMAT to OUTPUT-FORMAT using pandoc.
Return the output of the conversion."
  (with-current-buffer buffer
    (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" input-format "-t" output-format)))

(defun igor--markdown-to-org (buffer)
  "Convert the markdown content of BUFFER to org format using pandoc.
Return the output of the conversion."
  (igor--pandoc-convert "markdown" "org" buffer))

(defun igor--org-to-markdown (buffer)
  "Convert the org content of BUFFER to markdown format using pandoc.
Return the output of the conversion."
  (igor--pandoc-convert "org" "markdown" buffer))

(defun igor--response-as-markdown (response)
  "Convert the RESPONSE to markdown and return it as a string."
  (with-temp-buffer
    (insert response)
    (markdown-mode)
    (font-lock-ensure)
    (buffer-string)))

(defun igor--response-as-org (response)
  "Convert the RESPONSE to org format, hide properties and emphasis markers, and return it as a string."
  (with-temp-buffer
    (insert response)
    (org-mode)
    (set (make-local-variable 'org-hide-properties-when-displaying-subtree) t)
    (set (make-local-variable 'org-hide-emphasis-markers) t)
    (igor--markdown-to-org (current-buffer))
    (font-lock-ensure)
    (buffer-string)))

(defun igor--org-response-to-code (response)
  "Convert the org-formatted RESPONSE to executable code in `org-mode`, and return it as a string."
  (with-temp-buffer
    (insert response)
    (org-mode)
    (let* ((elements (org-element-parse-buffer))
           (srcs
            (org-element-map elements '(src-block example-block) (lambda (e) (substring-no-properties (org-element-property :value e)))))
           (inlines (org-element-map elements '(code verbatim) (lambda (e) (substring-no-properties (org-element-property :value e))))))
      (if (null srcs)
          (car (sort inlines (lambda (s1 s2) (> (length s1) (length s2)))))
        (mapconcat 'identity srcs "\n")))))

(defun igor--get-xref-definition-markers (identifier)
  "Return a list of markers of the definition of IDENTIFIER."
  (if (read identifier)
      (save-mark-and-excursion
        (let* ((xref-backend-functions (cl-remove #'etags--xref-backend xref-backend-functions))
               (xrefs
                (if xref-backend-functions
                    (or (funcall (xref--create-fetcher identifier 'definitions identifier)) ())
                  ())))
          (mapcar (lambda (xref) (xref-location-marker (xref-item-location xref))) xrefs)))
    ()))

(defun igor--get-definition-from-marker (marker)
  "Return the definition at the position of MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-mark-and-excursion
      (goto-char (marker-position marker))
      (deactivate-mark)
      (or (thing-at-point 'defun)
          (progn
            (mark-defun)
            (let ((d (buffer-substring-no-properties (point) (mark))))
              (when (not (string-blank-p d))
                d)))
          (thing-at-point 'sexp)
          (progn
            (goto-char (+ 1 (marker-position marker)))
            (or (thing-at-point 'defun) (thing-at-point 'sexp)))))))

(defun igor--get-definition (identifier)
  "Return the definition of IDENTIFIER."
  (let* ((markers (igor--get-xref-definition-markers identifier))
         (def (mapconcat #'igor--get-definition-from-marker markers "\n")))
    (if (string= "" def)
        nil
      def)))

(defun igor--major-mode-name (major-mode)
  "Return the name of the mode with which MAJOR-MODE is associated."
  (let ((mode-name (symbol-name major-mode)))
    (if (string-suffix-p "-mode" mode-name)
        (substring mode-name 0 (- (length mode-name) 5))
      mode-name)))

(defun igor--src-lang-lookup (major-mode-name)
  "Return the `org-mode` format that corresponds to the major-mode with the name MAJOR-MODE-NAME, if one exists."
  (car (rassoc (intern major-mode-name) org-src-lang-modes)))

(defun igor--get-src-lang (major-mode)
  "Return the `org-mode` format corresponding to the mode of the buffer associated with MAJOR-MODE, if one exists."
  (let* ((major-mode-name (igor--major-mode-name major-mode))
         (lookup (igor--src-lang-lookup major-mode-name)))
    (or lookup major-mode-name)))

;;;###autoload
(defun igor-eldoc-function (callback &rest _)
  "Display a summary and example code for the symbol at point.
Invoke CALLBACK with those values.

The function obtains the summary and code by using
the xref-backends to find the definition of the symbol at point
and converting the definition into `org-mode` format."
  (let ((identifier (symbol-name (symbol-at-point))))
    (let ((def (igor--get-definition identifier)))
      (if def
          (igor-chat
           `[(:role
              "system"
              :content "You are an assistant that receives a symbol name and code that defines that symbol. Respond with a summary of the symbol and some example code that uses it.")
             (:role "user" :content ,(format "`%s`\n``` %s\n%s\n```" identifier (igor--get-src-lang major-mode) def))]
           (lambda (response) (funcall callback (format "🧟💬\n%s" (igor--response-as-org response)) :thing identifier)))
        t))))

;;;###autoload
(defun igor-install-eldoc-backend ()
  "Install the igor eldoc backend."
  (add-to-list 'eldoc-documentation-functions #'igor-eldoc-function t))

(defun igor-code-query (language query code callback)
  "Perform QUERY on CODE in LANGUAGE and send response to CALLBACK."
  (igor-chat
   `[(:role "system" :content "You are an assistant that answers queries about code.")
     (:role "user" :content ,(format "Query: %s\n``` %s\n%s\n```" query language code))]
   (lambda (response) (funcall callback (igor--response-as-org response)))))

;;;###autoload
(defun igor-code-query-region (start end query)
  "Perform QUERY on region from START to END and display response in help buffer."
  (interactive "r\nMWhat doth marthter wish to know about thith code? ")
  (igor-code-query
   (igor--get-src-lang major-mode) query (buffer-substring-no-properties start end)
   (lambda (response)
     (with-help-window "*igor-query*"
       (insert response)))))

;;;###autoload
(defun igor-code-explain-region (start end)
  "Explain region from START to END and display response in buffer."
  (interactive "r")
  (igor-code-query-region start end "What does this code do?"))

(defun igor-emacs-action (mode instruction callback)
  "Generate and evaluate an elisp expression to achieve instruction.

The function takes three arguments: MODE, INSTRUCTION and CALLBACK.
- MODE: the name of the Emacs mode.
- INSTRUCTION: the instruction that the user wants to execute.
- CALLBACK: a function to be called with the code generated.

The function calls `igor-chat` with a list of messages, generates
the expression from the instruction and calls the callback function."
  (igor-chat
   `[(:role
      "system"
      :content
      ,(format
        "You are an assistant that lives within Emacs and generates elisp code that achieves the user's instruction when evaluated in `%s`. Explain what the elisp will do and how it works. If you need to make an HTTP request, the `request` library is available."
        mode))
     (:role "user" :content ,(format "%s" instruction))]
   (lambda (response)
     (let ((org-response (igor--response-as-org response)))
       (funcall callback (igor--org-response-to-code org-response) org-response)))))


;;;###autoload
(defun igor-do (instruction)
  "Generate an elisp expression from INSTRUCTION and ask the user whether to evaluate it."
  (interactive "MYeth marthter? ")
  (igor-emacs-action
   major-mode instruction
   (lambda (code-response org-response)
     (with-help-window "*igor-do*"
       (insert org-response))
     (if (yes-or-no-p (format "%s\n\nEvaluate?" code-response))
         (message "Here ith the rethult: %s" (eval (read (format "(progn %s)" code-response))))
       (message "Withe choith, marthter.")))))


(defun igor-code-write (language instruction callback)
  "Write code in a given LANGUAGE according to an INSTRUCTION.

LANGUAGE: The language of the code to be written.
INSTRUCTION: The instruction to be followed while writing the code.
CALLBACK: A function that will be executed after the code is written, with the written code as its argument."
  (igor-chat
   `[(:role "system" :content "You are an assistant that writes code in a given language according to an instruction.")
     (:role "user" :content ,(format "Language: `%s`\nInstruction: `%s`" language instruction))]
   (lambda (response) (funcall callback (igor--org-response-to-code (igor--response-as-org response))))))

;;;###autoload
(defun igor-code-write-at-point (instruction)
  "Write code at point according to an INSTRUCTION provided by the user.

INSTRUCTION: The instruction to be followed while writing the code."
  (interactive "MWhat doth marthter wish me to write? ")
  (let ((m (point-marker)))
    (igor-code-write
     (igor--get-src-lang major-mode) instruction
     (lambda (response)
       (with-current-buffer (marker-buffer m)
         (save-excursion
           (goto-char m)
           (insert response)
           (pulse-momentary-highlight-region (marker-position m) (+ (length response) (marker-position m)))))))))

(defun igor-code-rewrite (language instruction code callback)
  "Rewrite the provided CODE according to an INSTRUCTION.

LANGUAGE: The language of the code to be rewritten.
INSTRUCTION: The instruction to be followed while rewriting the code.
CODE: The code to be rewritten.
CALLBACK: A function that will be executed after the code is rewritten, with the rewritten code as its argument."
  (igor-chat
   `[(:role "system" :content "You are an assistant that rewrites code according to an instruction.")
     (:role "user" :content ,(format "Instruction: `%s`\nCode: ``` %s\n%s\n```" instruction language code))]
   (lambda (response) (funcall callback (igor--org-response-to-code (igor--response-as-org response))))))

;;;###autoload
(defun igor-code-rewrite-region (start end instruction)
  "Rewrite the code in the region between START and END according to INSTRUCTION.

START: The beginning of the region to be rewritten.
END: The end of the region to be rewritten.
INSTRUCTION: The instruction to be followed while rewriting the code."
  (interactive "r\nMWhat doth marthter wish me to do with thith? ")
  (let ((ma (set-marker (make-marker) start))
        (mb (set-marker (make-marker) end)))
    (igor-code-rewrite
     (igor--get-src-lang major-mode) instruction (buffer-substring-no-properties start end)
     (lambda (response)
       (with-current-buffer (marker-buffer ma)
         (save-excursion
           (delete-region (marker-position ma) (marker-position mb))
           (goto-char ma)
           (insert response)
           (pulse-momentary-highlight-region (marker-position ma) (+ (length response) (marker-position ma)))))))))

(defvar igor-mode-map (make-sparse-keymap))
(defvar igor-prefix-map (make-sparse-keymap))

(keymap-set igor-prefix-map "r" #'igor-code-rewrite-region)
(keymap-set igor-prefix-map "w" #'igor-code-write-at-point)
(keymap-set igor-prefix-map "d" #'igor-do)
(keymap-set igor-prefix-map "x" #'igor-code-explain-region)
(keymap-set igor-prefix-map "q" #'igor-code-query-region)

(keymap-set igor-mode-map "C-c i" igor-prefix-map)

(define-minor-mode igor-mode
  "Igor mode."
  :lighter "🧟")


(provide 'igor)

;;; igor.el ends here
