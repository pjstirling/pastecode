(defun slime-pastecode-create-paste (description)
  (interactive "MEnter a description: ")
  (let ((package (slime-search-buffer-package))
	(region (slime-region-for-defun-at-point)))
    (slime-eval-async `(pastecode:insert-paste-for-slime ,description
							 ,(apply #'buffer-substring-no-properties
								 region)
							 ,package))))
