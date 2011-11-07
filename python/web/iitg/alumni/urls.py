from django.conf.urls.defaults import *

from django.contrib import admin
admin.autodiscover()

from iitg.alumni.models import BlogPost

blogPostList = {
    'queryset': BlogPost.objects.all(),
    'date_field': 'dateOfPublishing',
    'template_name': 'alumni/blogPostList.html',
}
blogPostDetail = {
    'queryset': BlogPost.objects.all(),
    'date_field': 'dateOfPublishing',
    'template_name': 'alumni/blogPostDetail.html',
    'slug_field': 'titleSlug',
}

urlpatterns = patterns('django.views.generic.date_based',
    (r'^(?P<year>\d{4})/(?P<month>[a-z]{3})/(?P<day>\d{1,2})/(?P<slug>[-\w]{5,50})/$', 'object_detail', blogPostDetail),
    (r'^(?P<year>\d{4})/(?P<month>[a-z]{3})/(?P<day>\d{1,2})/$', 'archive_day', blogPostList),
    (r'^(?P<year>\d{4})/(?P<month>[a-z]{3})/$', 'archive_month', blogPostList),
    (r'^(?P<year>\d{4})/$', 'archive_year', blogPostList),
    (r'^$', 'archive_index', blogPostList),
)
urlpatterns += patterns('iitg.alumni.views',
    (r'user/register/$', 'registerUser'),
    (r'user/verify/(?P<userName>\w{1,20})/(?P<nonce>\w{32})/$', 'verifyUser'),
    #(r'user/modify/(?P<userName>\w{7,20})/$', 'modifyUser'),
    #(r'user/all/$', 'allUsers'),
    #(r'user/page/(?P<pageId>\d{1,3})/$', 'userPage'),
    #(r'user/(?P<userId>)\w{7,20}/$', 'userDetail'),
)
urlpatterns += patterns('django.views.generic.simple',
    (r'user/register/thanks/$', 'direct_to_template', {'template': 'alumni/registerUserThanks.html'}),
)
