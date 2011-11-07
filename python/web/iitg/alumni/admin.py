from iitg.alumni.models import BlogPost
from django.contrib import admin

class BlogPostAdmin(admin.ModelAdmin):
    fields = ['title', 'titleSlug', 'fullText', 'dateOfPublishing']
    prepopulated_fields = {'titleSlug': ('title',)}

admin.site.register(BlogPost, BlogPostAdmin)
