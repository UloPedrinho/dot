;; net
(defun pedro/net-get-mp3-uris-from-url (url)
  "return a list o mp3 uris from a web page"
  (let ((data-buffer (url-retrieve-synchronously url ))
	(url-list '()))
    (switch-to-buffer data-buffer)
    (goto-char (point-min))
    (while (search-forward-regexp "\\(\\(?:http://\\)?www\\(?:[./#+-]\\w*\\)+mp3\\)" nil t)
	   (push (match-string 1) url-list))
    (kill-buffer data-buffer)
    url-list))

(defun pedro/save-hiragana-katakana-sounds-to-files (url path)
  (dolist (uri (pedro/net-get-mp3-uris-from-url url))
    (url-copy-file uri (concat path "/" (car (last (split-string uri "/")))))))

