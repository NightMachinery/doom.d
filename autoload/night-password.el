;;; autoload/night-password.el -*- lexical-binding: t; -*-

(after! (password-cache eredis)
  (setq password-cache t)
  (setq password-cache-expiry nil)
;;;
  (defcustom password-use-redis (featurep 'eredis)
    "Use Redis for caching passwords if eredis is available."
    :group 'password
    :type 'boolean)

  (defconst password-redis-hash "emacs-password-cache-215870"
    "Name of the Redis hash table used to store passwords.")
  ;; `redis-cli hgetall emacs-password-cache-215870`

  (defun password-read-from-cache-redis (key)
    "Obtain passphrase for KEY from Redis if available, otherwise from local cache."
    (if password-use-redis
        (let ((password (eredis-hget password-redis-hash key)))
          (if (and password (not (string= password "")))
              password
            (password-read-from-cache key)))
      (password-read-from-cache key)))

  (defun password-in-cache-p-redis (key)
    "Check if KEY is in the Redis cache or local cache."
    (if password-use-redis
        (not (string= (eredis-hget password-redis-hash key) ""))
      (password-in-cache-p key)))

  (defun password-cache-add-redis (key password)
    "Add password to Redis cache if available, otherwise to local cache."
    (if password-use-redis
        (progn
          (eredis-hset password-redis-hash key password)
          nil)
      (password-cache-add key password)))

  (defun password-cache-remove-redis (key)
    "Remove password indexed by KEY from Redis cache or local cache."
    (if password-use-redis
        (eredis-hdel password-redis-hash key)
      (password-cache-remove key)))

  (defun password-reset-redis ()
    "Clear both the Redis and local password cache."
    (interactive)
    (if password-use-redis
        (eredis-del password-redis-hash))
    (password-reset))

  (defun password-read-redis (prompt &optional key)
    "Read password, for use with KEY, from user, or from Redis cache if available and wanted.
KEY indicates the purpose of the password, so the cache can
separate passwords. The cache is not used if KEY is nil.
KEY is typically a string but can be anything (compared via `equal').
The variable `password-cache' controls whether the cache is used."
    (let ((password
           (or (password-read-from-cache-redis key)
               (read-passwd prompt))))
      (password-cache-add-redis key password)
      password))

  (provide 'password-cache-redis)
;;; password-cache-redis.el ends here

  )
