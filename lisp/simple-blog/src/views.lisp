(in-package :simple-blog)

(defview user-table-view (:type table :inherit-from '(:scaffold user)))
(defview user-data-view (:type data :inherit-from '(:scaffold user)))
(defview user-form-view (:type form :inherit-from '(:scaffold user)))

(defview post-table-view (:type table :inherit-from '(:scaffold post)))
(defview post-data-view (:type data :inherit-from '(:scaffold post)))
(defview post-form-view (:type form :inherit-from '(:scaffold post))
  (time :hidep t)
  (author :reader #'post-author-id
          :present-as (dropdown :choices #'all-users
                                :label-key #'user-name)
          :parse-as (object-id :class-name 'user)
          :requiredp t)
  (short-text :present-as (textarea :cols 30 :rows 1)
              :requiredp t)
  (text :present-as (textarea :cols 30)
        :requiredp t))
