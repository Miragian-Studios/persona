;;; persona.el --- Fictional Character Generator for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Theena Kumaragurunathan
;; Author: Theena Kumaragurunathan <theenat.k@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, characters, writing, fiction
;; URL: https://github.com/Miragian-Studios/persona
;; License: MIT

;;; Commentary:

;; Persona is an Emacs package for writers and actors to build fictional
;; characters with psychological depth.  It provides:
;;
;; - Character Management with detailed profiles
;; - Psychological Index tracking (Self Esteem vs Social Reputation)
;; - Visual Arc Charts showing character journeys
;; - Timeline Events for tracking milestones
;; - Multi-project support (projects stored in current directory)
;; - Import/Export (JSON/CSV)
;; - PDF Character Bible generation
;;
;; Quick Start:
;;   M-x persona-open RET to open the current project
;;   M-x persona-new-project RET to create a new project
;;
;; Keybindings (in persona buffer):
;;   n - New character
;;   e - Edit character
;;   d - Delete character
;;   s - Save project
;;   i - Import characters
;;   x - Export characters
;;   g - Arc chart
;;   t - Timeline view
;;   q - Quit

;;; Code:

(require 'sqlite)
(require 'json)
(require 'seq)
(require 'org)
(require 'ox-hatex)
(require 'ox-latex)

;;;; Customization

(defgroup persona nil
  "Fictional Character Generator"
  :group 'convenience
  :prefix "persona-")

(defcustom persona-project-file "persona-project.el"
  "Name of the project data file."
  :type 'string
  :group 'persona)

(defcustom persona-default-se '(3 3 3)
  "Default Self Esteem values for new characters (SE1 SE2 SE3)."
  :type '(list (integer :tag "Act 1") (integer :tag "Act 2") (integer :tag "Act 3"))
  :group 'persona)

(defcustom persona-default-sr '(3 3 3)
  "Default Social Reputation values for new characters (SR1 SR2 SR3)."
  :type '(list (integer :tag "Act 1") (integer :tag "Act 2") (integer :tag "Act 3"))
  :group 'persona)

(defcustom persona-theme 'dark
  "Color theme for arc charts."
  :type '(choice (const dark) (const light))
  :group 'persona)

;;;; Data Structures

(cl-defstruct (persona-character
                (:constructor persona-character-create)
                (:copier persona-character-copy))
  "Character structure."
  (id nil :read-only t)
  (name "")
  (backstory "")
  (self-esteem '(3 3 3))
  (social-reputation '(3 3 3))
  (relationships nil)
  (timeline nil)
  (notes nil)
  (created-at nil)
  (updated-at nil))

(cl-defstruct (persona-project
                (:constructor persona-project-create)
                (:copier persona-project-copy))
  "Project structure."
  (name "Untitled")
  (author "")
  (description "")
  (characters nil)
  (created-at nil)
  (updated-at nil))

;;;; Variables

(defvar persona-current-project nil
  "Current open project.")

(defvar persona-buffer nil
  "Main persona buffer.")

(defvar persona-char-buffer nil
  "Character edit buffer.")

(defvar persona-chart-buffer nil
  "Arc chart buffer.")

(defvar persona-timeline-buffer nil
  "Timeline buffer.")

(defvar persona-current-view 'characters
  "Current view: 'characters, 'chart, 'timeline, 'settings.")

(defvar persona-selected-character-id nil
  "ID of currently selected character.")

(defvar persona-db nil
  "SQLite database connection.")

;;;; Utilities

(defun persona--generate-id ()
  "Generate unique ID for character."
  (format "char-%s" (format-time-string "%s%N")))

(defun persona--timestamp ()
  "Get current timestamp."
  (time-convert (current-time) 'integer))

(defun persona--calc-arc-points (se sr)
  "Calculate arc points from SE and SR lists.
SE and SR are lists of 3 values each.  Returns list of arc points."
  (cl-mapcar '- se sr))

(defun persona--calc-psych-index (arc-points)
  "Calculate total psychological index from ARC-POINTS."
  (cl-reduce '+ arc-points))

(defun persona--read-file (file)
  "Read FILE content as string."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun persona--write-file (file content)
  "Write CONTENT to FILE."
  (with-temp-buffer
    (insert content)
    (write-file file)))

(defun persona--ensure-directory (dir)
  "Ensure DIR exists."
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;; Database Functions

(defun persona--init-db (db-file)
  "Initialize SQLite DB at DB-FILE."
  (setq persona-db (sqlite-open db-file))
  (sqlite-execute persona-db
    "CREATE TABLE IF NOT EXISTS project (
       id INTEGER PRIMARY KEY,
       name TEXT NOT NULL,
       author TEXT,
       description TEXT,
       created_at INTEGER,
       updated_at INTEGER)")
  (sqlite-execute persona-db
    "CREATE TABLE IF NOT EXISTS characters (
       id TEXT PRIMARY KEY,
       project_id INTEGER,
       name TEXT NOT NULL,
       backstory TEXT,
       se1 INTEGER, se2 INTEGER, se3 INTEGER,
       sr1 INTEGER, sr2 INTEGER, sr3 INTEGER,
       relationships TEXT,
       timeline TEXT,
       notes TEXT,
       created_at INTEGER,
       updated_at INTEGER,
       FOREIGN KEY(project_id) REFERENCES project(id))"))

(defun persona--save-to-db ()
  "Save current project to SQLite database."
  (when persona-current-project
    (let* ((db-file (expand-file-name "persona.db" default-directory))
           (ts (persona--timestamp)))
      (persona--ensure-directory (file-name-directory db-file))
      (persona--init-db db-file)
      (setq persona-current-project
            (persona-project-copy persona-current-project
                                  :updated-at ts))
      (sqlite-execute persona-db (format "DELETE FROM characters WHERE project_id = 1"))
      (dolist (char (persona-project-characters persona-current-project))
        (let ((arc (persona--calc-arc-points
                    (persona-character-self-esteem char)
                    (persona-character-social-reputation char))))
          (sqlite-execute persona-db
            (format "INSERT INTO characters (id, project_id, name, backstory,
             se1, se2, se3, sr1, sr2, sr3, relationships, timeline, notes, created_at, updated_at)
             VALUES ('%s', 1, '%s', '%s', %d, %d, %d, %d, %d, %d, '%s', '%s', '%s', %d, %d)"
              (persona-character-id char)
              (persona-character-name char)
              (persona-character-backstory char)
              (cl-first (persona-character-self-esteem char))
              (cl-second (persona-character-self-esteem char))
              (cl-third (persona-character-self-esteem char))
              (cl-first (persona-character-social-reputation char))
              (cl-second (persona-character-social-reputation char))
              (cl-third (persona-character-social-reputation char))
              (json-serialize (persona-character-relationships char))
              (json-serialize (persona-character-timeline char))
              (json-serialize (persona-character-notes char))
              (or (persona-character-created-at char) ts)
              ts)))))))

(defun persona--load-from-db ()
  "Load project from SQLite database."
  (let* ((db-file (expand-file-name "persona.db" default-directory))
         (chars nil))
    (when (file-exists-p db-file)
      (persona--init-db db-file)
      (setq persona-current-project (persona-project-create :name "Loaded Project"))
      (let* ((rows (sqlite-select persona-db "SELECT name, author, description, created_at, updated_at FROM project WHERE id = 1")))
        (when rows
          (setq persona-current-project
                (persona-project-copy persona-current-project
                                      :name (cl-first (cl-first rows))
                                      :author (cl-second (cl-first rows))
                                      :description (cl-third (cl-first rows))
                                      :created-at (cl-fourth (cl-first rows))
                                      :updated-at (cl-fifth (cl-first rows))))))
      (let* ((rows (sqlite-select persona-db "SELECT * FROM characters")))
        (dolist (row rows)
          (let* ((id (elt row 0))
                 (name (elt row 2))
                 (backstory (elt row 3))
                 (se (list (elt row 4) (elt row 5) (elt row 6)))
                 (sr (list (elt row 7) (elt row 8) (elt row 9)))
                 (relationships (json-read-from-string (or (elt row 10) "[]")))
                 (timeline (json-read-from-string (or (elt row 11) "[]")))
                 (notes (json-read-from-string (or (elt row 12) "[]")))
                 (created-at (elt row 13))
                 (updated-at (elt row 14))
                 (char (persona-character-create
                        :id id
                        :name name
                        :backstory backstory
                        :self-esteem se
                        :social-reputation sr
                        :relationships relationships
                        :timeline timeline
                        :notes notes
                        :created-at created-at
                        :updated-at updated-at)))
            (push char chars))))
      (setq persona-current-project
            (persona-project-copy persona-current-project
                                  :characters (nreverse chars))))))

;;;; Project Functions

(defun persona-new-project (name &optional author description)
  "Create new project NAME with AUTHOR and DESCRIPTION."
  (interactive "sProject Name: ")
  (let ((proj (persona-project-create
               :name name
               :author (or author (user-full-name))
               :description (or description "")
               :characters nil
               :created-at (persona--timestamp)
               :updated-at (persona--timestamp))))
    (setq persona-current-project proj)
    (persona--save-to-db)
    (message "Created project: %s" name)
    (persona-open)))

(defun persona-open ()
  "Open or create project in current directory."
  (interactive)
  (let ((db-file (expand-file-name "persona.db" default-directory)))
    (if (file-exists-p db-file)
        (progn
          (persona--load-from-db)
          (message "Opened project: %s" (persona-project-name persona-current-project)))
      (setq persona-current-project
            (persona-project-create
             :name (file-name-base (directory-file-name default-directory))
             :author (user-full-name)
             :created-at (persona--timestamp)
             :updated-at (persona--timestamp)))
      (persona--save-to-db)
      (message "Created new project in: %s" default-directory))
    (persona-render)))

(defun persona-save ()
  "Save current project."
  (interactive)
  (persona--save-to-db)
  (message "Project saved!"))

(defun persona-add-character (name &optional backstory)
  "Add new character with NAME and BACKSTORY."
  (interactive "sCharacter Name: ")
  (let* ((ts (persona--timestamp))
         (char (persona-character-create
                :id (persona--generate-id)
                :name name
                :backstory (or backstory "")
                :self-esteem persona-default-se
                :social-reputation persona-default-sr
                :relationships nil
                :timeline nil
                :notes nil
                :created-at ts
                :updated-at ts)))
    (push char (persona-project-characters persona-current-project))
    (setq persona-current-project
          (persona-project-copy persona-current-project
                                :updated-at ts))
    (persona--save-to-db)
    (persona-render)
    (message "Added character: %s" name)))

(defun persona-delete-character (char-id)
  "Delete character with CHAR-ID."
  (interactive (list persona-selected-character-id))
  (when char-id
    (let ((chars (cl-delete-if (lambda (c)
                                 (string= (persona-character-id c) char-id))
                               (persona-project-characters persona-current-project))))
      (setq persona-current-project
            (persona-project-copy persona-current-project
                                  :characters chars
                                  :updated-at (persona--timestamp)))
      (persona--save-to-db)
      (persona-render)
      (message "Character deleted"))))

(defun persona-edit-character (char-id)
  "Edit character with CHAR-ID in a buffer."
  (interactive (list persona-selected-character-id))
  (when char-id
    (let* ((char (cl-find char-id
                          (persona-project-characters persona-current-project)
                          :key 'persona-character-id
                          :test 'string=))
           (buf (get-buffer-create "*Persona Edit Character*")))
      (with-current-buffer buf
        (persona-edit-character-mode)
        (setq-local persona-editing-character char)
        (persona-render-edit-buffer char))
      (pop-to-buffer buf))))

(defun persona-refresh ()
  "Refresh the persona buffer."
  (interactive)
  (persona-render))

;;;; Buffer Rendering

(defun persona-render ()
  "Render main persona buffer."
  (unless persona-buffer
    (setq persona-buffer (get-buffer-create "*Persona*")))
  (with-current-buffer persona-buffer
    (erase-buffer)
    (persona-mode)
    (insert (persona--render-header))
    (insert (persona--render-content))
    (goto-char (point-min)))
  (pop-to-buffer persona-buffer))

(defun persona--render-header ()
  "Render header."
  (let ((proj persona-current-project)
        (view persona-current-view))
    (format "
═══════════════════════════════════════════════════════════════
  PERSONA - Fictional Character Generator
═══════════════════════════════════════════════════════════════
  Project: %s | Author: %s
  Characters: %d | View: %s
═══════════════════════════════════════════════════════════════

"
            (or (persona-project-name proj) "Untitled")
            (or (persona-project-author proj) "")
            (length (persona-project-characters proj))
            (capitalize (symbol-name view)))))

(defun persona--render-content ()
  "Render main content based on current view."
  (pcase persona-current-view
    ('characters (persona--render-characters))
    ('chart (persona--render-chart))
    ('timeline (persona--render-timeline))
    ('settings (persona--render-settings))
    (_ (persona--render-characters))))

(defun persona--render-characters ()
  "Render characters list."
  (let ((chars (persona-project-characters persona-current-project)))
    (if (null chars)
        "
  No characters yet. Press 'n' to add one.

"
      (concat "
  #  Name                  SE     SR     Arc    Notes
  ─────────────────────────────────────────────────────────
"
              (mapconcat (lambda (char)
                           (let* ((se (persona-character-self-esteem char))
                                  (sr (persona-character-social-reputation char))
                                  (arc (persona--calc-arc-points se sr))
                                  (total (persona--calc-psych-index arc))
                                  (notes-count (length (persona-character-notes char))))
                             (format "  %-2s %-21s %s    %s    %+3d  %d notes"
                                     (substring (persona-character-id char) -2)
                                     (persona-character-name char)
                                     (mapconcat 'number-to-string se "/")
                                     (mapconcat 'number-to-string sr "/")
                                     total
                                     notes-count)))
                         chars "
")))))

(defun persona--render-chart ()
  "Render arc chart view."
  (if (null (persona-project-characters persona-current-project))
      "
  No characters to chart. Add some characters first.

"
    (let* ((chars (persona-project-characters persona-current-project))
           (svg (persona--generate-arc-svg chars)))
      (concat "
  Arc Chart - Psychological Index Over Time

"
              svg
              "

  Press 's' to select different characters for comparison
")))

(defun persona--render-timeline ()
  "Render timeline view."
  (let ((chars (persona-project-characters persona-current-project)))
    (if (null chars)
        "
  No characters with timeline events.

"
      (concat "
  Timeline - Character Events

"
              (mapconcat (lambda (char)
                           (let ((events (persona-character-timeline char)))
                             (when events
                               (format "  ┌─ %s\n%s"
                                       (persona-character-name char)
                                       (mapconcat (lambda (e)
                                                    (format "  │ %s: %s"
                                                            (alist-get 'date e)
                                                            (alist-get 'event e)))
                                                  events "\n")))))
                         chars "\n")))))

(defun persona--render-settings ()
  "Render settings view."
  (format "
  Project Settings

  Name: %s
  Author: %s
  Description: %s

  Press 'u' to update project details
"
          (or (persona-project-name persona-current-project) "")
          (or (persona-project-author persona-current-project) "")
          (or (persona-project-description persona-current-project) "")))

;;;; SVG Chart Generation

(defun persona--generate-arc-svg (chars)
  "Generate SVG arc chart for CHARS."
  (let* ((colors '("#f38ba8" "#a6e3a1" "#89b4fa" "#fab387" "#cba6f7" "#94e2d5" "#74c7ec" "#f9e2af"))
         (width 600)
         (height 300)
         (padding 50)
         (chart-width (- width (* padding 2)))
         (chart-height (- height (* padding 2)))
         (x-step (/ chart-width 2))
         (y-scale (/ chart-height 10)))
    (format "<svg width=\"%d\" height=\"%d\" xmlns=\"http://www.w3.org/2000/svg\">
  <style>
    .axis { stroke: #6c7086; stroke-width: 1; }
    .label { fill: #cdd6f4; font-family: monospace; font-size: 10px; }
    .title { fill: #cdd6f4; font-family: sans-serif; font-size: 14px; font-weight: bold; }
  </style>
  <!-- Background -->
  <rect width=\"%d\" height=\"%d\" fill=\"#1e1e2e\"/>
  <!-- Grid lines -->
  %s
  <!-- Y-axis labels -->
  %s
  <!-- X-axis labels -->
  <text x=\"%d\" y=\"%d\" class=\"label\" text-anchor=\"middle\">Act 1</text>
  <text x=\"%d\" y=\"%d\" class=\"label\" text-anchor=\"middle\">Act 2</text>
  <text x=\"%d\" y=\"%d\" class=\"label\" text-anchor=\"middle\">Act 3</text>
  <!-- Title -->
  <text x=\"%d\" y=\"20\" class=\"title\" text-anchor=\"middle\">Psychological Index Journey</text>
  <text x=\"20\" y=\"%d\" class=\"label\" text-anchor=\"middle\" transform=\"rotate(-90, 20, %d)\">Psych Index</text>
  <text x=\"%d\" y=\"%d\" class=\"label\" text-anchor=\"middle\">Narrative Time</text>
  <!-- Character arcs -->
  %s
</svg>"
            width height width height
            (mapconcat (lambda (i)
                        (let ((y (+ padding (* i y-scale))))
                          (format "<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" stroke=\"#313244\" stroke-width=\"1\"/>"
                                  padding y (- width padding) y)))
                      (number-sequence 0 10) "\n")
            (mapconcat (lambda (i)
                        (let ((val (- 5 i))
                              (y (+ padding (* i y-scale))))
                          (format "<text x=\"%d\" y=\"%d\" class=\"label\" text-anchor=\"end\">%+d</text>"
                                  (- padding 5) (+ y 3) val)))
                      (number-sequence 0 10) "\n")
            (+ padding x-step) (- height 10)
            (+ padding (* 2 x-step)) (- height 10)
            width 30 30
            (- width padding) (- height 5)
            (mapconcat (lambda (pair)
                        (let ((char (car pair))
                              (color (cdr pair)))
                          (persona--render-arc-path char color padding x-step y-scale)))
                      (cl-mapcar 'cons chars (seq-take colors (length chars))) "\n"))))

(defun persona--render-arc-path (char color padding x-step y-scale)
  "Render arc path for CHAR with COLOR."
  (let* ((se (persona-character-self-esteem char))
         (sr (persona-character-social-reputation char))
         (arc (persona--calc-arc-points se sr))
         (points (cl-mapcar (lambda (i val)
                              (cons (+ padding (* (1+ i) x-step))
                                    (- (+ padding (* 5 y-scale)) (* val y-scale))))
                            (number-sequence 0 2) arc)))
    (format "<path d=\"M %s %s Q %s %s %s %s\" fill=\"none\" stroke=\"%s\" stroke-width=\"2\"/>
    <text x=\"%s\" y=\"%s\" fill=\"%s\" font-size=\"10\">%s</text>"
            (caar points) (cdar points)
            (cadr points) (cdadr points)
            (caddr points) (cddr points)
            color
            (cddr points) (- (cddr points) 8) color
            (persona-character-name char))))

;;;; Import/Export

(defun persona-export-json ()
  "Export project to JSON file."
  (interactive)
  (let* ((file (expand-file-name
                (format "%s-characters.json"
                        (persona-project-name persona-current-project))
                default-directory))
         (data (list (cons 'project (persona-project-name persona-current-project))
                     (cons 'author (persona-project-author persona-current-project))
                     (cons 'description (persona-project-description persona-current-project))
                     (cons 'characters
                           (mapcar (lambda (c)
                                     (list (cons 'id (persona-character-id c))
                                           (cons 'name (persona-character-name c))
                                           (cons 'backstory (persona-character-backstory c))
                                           (cons 'selfEsteem (persona-character-self-esteem c))
                                           (cons 'socialReputation (persona-character-social-reputation c))
                                           (cons 'relationships (persona-character-relationships c))
                                           (cons 'timeline (persona-character-timeline c))
                                           (cons 'notes (persona-character-notes c))))
                                   (persona-project-characters persona-current-project))))))
    (persona--write-file file (json-encode data))
    (message "Exported to: %s" file)))

(defun persona-import-json ()
  "Import characters from JSON file."
  (interactive)
  (let* ((file (car (directory-files default-directory t ".*-characters\\.json$")))
         (data (when file (json-read-from-string (persona--read-file file)))))
    (when data
      (dolist (c (alist-get 'characters data))
        (let ((char (persona-character-create
                     :id (alist-get 'id c)
                     :name (alist-get 'name c)
                     :backstory (alist-get 'backstory c)
                     :self-esteem (or (alist-get 'selfEsteem c) '(3 3 3))
                     :social-reputation (or (alist-get 'socialReputation c) '(3 3 3))
                     :relationships (or (alist-get 'relationships c) nil)
                     :timeline (or (alist-get 'timeline c) nil)
                     :notes (or (alist-get 'notes c) nil)
                     :created-at (persona--timestamp)
                     :updated-at (persona--timestamp))))
          (push char (persona-project-characters persona-current-project))))
      (persona--save-to-db)
      (persona-render)
      (message "Imported characters from: %s" file))))

(defun persona-export-csv ()
  "Export characters to CSV file."
  (interactive)
  (let* ((file (expand-file-name
                (format "%s-characters.csv"
                        (persona-project-name persona-current-project))
                default-directory))
         (lines (cons "name,backstory,se1,se2,se3,sr1,sr2,sr3,notes"
                      (mapcar (lambda (c)
                                (format "\"%s\",\"%s\",%d,%d,%d,%d,%d,%d,\"%s\""
                                        (persona-character-name c)
                                        (persona-character-backstory c)
                                        (cl-first (persona-character-self-esteem c))
                                        (cl-second (persona-character-self-esteem c))
                                        (cl-third (persona-character-self-esteem c))
                                        (cl-first (persona-character-social-reputation c))
                                        (cl-second (persona-character-social-reputation c))
                                        (cl-third (persona-character-social-reputation c))
                                        (mapconcat 'identity (persona-character-notes c) "; ")))
                              (persona-project-characters persona-current-project)))))
    (persona--write-file file (string-join lines "\n"))
    (message "Exported to: %s" file)))

;;;; PDF Export

(defun persona-export-pdf ()
  "Export character bible to PDF via Org-mode."
  (interactive)
  (let* ((file (expand-file-name
                (format "%s-character-bible.org"
                        (persona-project-name persona-current-project))
                default-directory))
         (org-buffer (get-buffer-create "*Persona PDF*")))
    (with-current-buffer org-buffer
      (erase-buffer)
      (insert (format "#+TITLE: Character Bible - %s\n#+AUTHOR: %s\n\n"
                      (persona-project-name persona-current-project)
                      (persona-project-author persona-current-project)))
      (dolist (char (persona-project-characters persona-current-project))
        (insert (format "* %s\n" (persona-character-name char)))
        (insert (format "** Backstory\n%s\n\n" (persona-character-backstory char)))
        (let ((se (persona-character-self-esteem char))
              (sr (persona-character-social-reputation char)))
          (insert (format "** Psychological Profile\n- Self Esteem: %s\n- Social Reputation: %s\n- Arc Points: %s\n- Total Index: %+d\n\n"
                          (mapconcat 'number-to-string se " -> ")
                          (mapconcat 'number-to-string sr " -> ")
                          (persona--calc-arc-points se sr)
                          (persona--calc-psych-index (persona--calc-arc-points se sr)))))
        (when (persona-character-relationships char)
          (insert "** Relationships\n")
          (dolist (r (persona-character-relationships char))
            (insert (format "- %s: %s\n"
                            (alist-get 'targetId r)
                            (alist-get 'type r))))
          (insert "\n"))
        (when (persona-character-timeline char)
          (insert "** Timeline\n")
          (dolist (e (persona-character-timeline char))
            (insert (format "- %s: %s\n"
                            (alist-get 'date e)
                            (alist-get 'event e))))
          (insert "\n"))
        (when (persona-character-notes char)
          (insert "** Notes\n")
          (dolist (n (persona-character-notes char))
            (insert (format "- %s\n" n)))
          (insert "\n")))
      (write-file file)
      (org-latex-export-to-pdf nil nil nil nil
                               (list "-shell-escape" "persona")))
    (message "Character bible exported to: %s" file)))

;;;; Major Mode

(define-derived-mode persona-mode special-mode "Persona"
  "Major mode for Persona character generator."
  (setq buffer-read-only t)
  (setq-local persona-current-project persona-current-project)
  (setq-local persona-current-view 'characters))

(define-key persona-mode-map (kbd "n") 'persona-add-character)
(define-key persona-mode-map (kbd "e") 'persona-edit-character)
(define-key persona-mode-map (kbd "d") 'persona-delete-character)
(define-key persona-mode-map (kbd "s") 'persona-save)
(define-key persona-mode-map (kbd "g") (lambda () (interactive) (setq persona-current-view 'chart) (persona-render)))
(define-key persona-mode-map (kbd "t") (lambda () (interactive) (setq persona-current-view 'timeline) (persona-render)))
(define-key persona-mode-map (kbd "c") (lambda () (interactive) (setq persona-current-view 'characters) (persona-render)))
(define-key persona-mode-map (kbd ",") (lambda () (interactive) (setq persona-current-view 'settings) (persona-render)))
(define-key persona-mode-map (kbd "i") 'persona-import-json)
(define-key persona-mode-map (kbd "x") 'persona-export-json)
(define-key persona-mode-map (kbd "X") 'persona-export-csv)
(define-key persona-mode-map (kbd "p") 'persona-export-pdf)
(define-key persona-mode-map (kbd "r") 'persona-refresh)
(define-key persona-mode-map (kbd "q") 'persona-quit)

;;;; Edit Character Mode

(defvar persona-editing-character nil
  "Character being edited.")

(define-derived-mode persona-edit-character-mode text-mode "Persona Edit"
  "Mode for editing character details."
  (setq buffer-read-only nil))

(defun persona-render-edit-buffer (char)
  "Render edit buffer for CHAR."
  (let* ((buf (current-buffer))
         (name (persona-character-name char))
         (backstory (persona-character-backstory char))
         (se (persona-character-self-esteem char))
         (sr (persona-character-social-reputation char))
         (notes (persona-character-notes char)))
    (erase-buffer)
    (insert (format "
═══════════════════════════════════════════════════════════════
  Edit Character: %s
═══════════════════════════════════════════════════════════════

Name: %s

Backstory:
%s

Self Esteem (Act 1-3, -5 to +5):
  Act 1: %d
  Act 2: %d
  Act 3: %d

Social Reputation (Act 1-3, -5 to +5):
  Act 1: %d
  Act 2: %d
  Act 3: %d

Psychological Index: %+d

Notes (one per line):
%s

═══════════════════════════════════════════════════════════════
Press C-c C-c to save and close
"
                     name name backstory
                     (cl-first se) (cl-second se) (cl-third se)
                     (cl-first sr) (cl-second sr) (cl-third sr)
                     (persona--calc-psych-index (persona--calc-arc-points se sr))
                     (string-join notes "\n")))
    (setq-local persona-editing-character char)))

(defun persona-save-character ()
  "Save character edits."
  (interactive)
  (when persona-editing-character
    (goto-char (point-min))
    (search-forward "Name: ")
    (let ((name (buffer-substring (point) (line-end-point))))
      (search-forward "Backstory:")
      (forward-line)
      (let ((backstory (buffer-substring (point) (re-search-forward "^Self Esteem" nil t))))
        (search-forward "Act 1: ")
        (let ((se1 (string-to-number (buffer-substring (point) (line-end-point)))))
          (forward-line)
          (let ((se2 (string-to-number (buffer-substring (point) (line-end-point)))))
            (forward-line)
            (let ((se3 (string-to-number (buffer-substring (point) (re-search-forward "^Social Reputation" nil t))))))
              (search-forward "Act 1: ")
              (let ((sr1 (string-to-number (buffer-substring (point) (line-end-point)))))
                (forward-line)
                (let ((sr2 (string-to-number (buffer-substring (point) (line-end-point)))))
                  (forward-line)
                  (let ((sr3 (string-to-number (buffer-substring (point) (re-search-forward "^Psychological Index" nil t)))))
                    (search-forward "Notes")
                    (forward-line)
                    (let ((notes (split-string (buffer-substring (point) (point-max)) "\n" t)))
                      (setf (persona-character-name persona-editing-character) name)
                      (setf (persona-character-backstory persona-editing-character) backstory)
                      (setf (persona-character-self-esteem persona-editing-character) (list se1 se2 se3))
                      (setf (persona-character-social-reputation persona-editing-character) (list sr1 sr2 sr3))
                      (setf (persona-character-notes persona-editing-character) notes)
                      (setf (persona-character-updated-at persona-editing-character) (persona--timestamp))
                      (persona--save-to-db)
                      (persona-render)
                      (kill-buffer)))))))))))))
  (message "Character saved"))

(define-key persona-edit-character-mode-map (kbd "C-c C-c") 'persona-save-character)

;;;; Commands

;;;###autoload
(defun persona ()
  "Open Persona character generator."
  (interactive)
  (persona-open))

;;;###autoload
(defun persona-new ()
  "Create new Persona project."
  (interactive)
  (call-interactively 'persona-new-project))

(provide 'persona)

;;; persona.el ends here
