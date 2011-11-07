from django.db import models
from django import forms

from iitg.alumni.signals import *

###Enums
degreeTypes = (
    (1, 'BSc'),
    (2, 'BTech'),
    (3, 'MTech'),
    (4, 'MSc'),
)
departments = (
    (1, 'CSE'),
    (2, 'Mech'),
    (3, 'Math'),
)
yearOfPassing = (
    ('1995-05-01', '1995'),
    ('1996-05-01', '1996'),
    ('1997-05-01', '1997'),
    ('1998-05-01', '1998'),
    ('1999-05-01', '1999'),
    ('2000-05-01', '2000'),
    ('2001-05-01', '2001'),
    ('2002-05-01', '2002'),
    ('2003-05-01', '2003'),
    ('2004-05-01', '2004'),
    ('2005-05-01', '2005'),
    ('2006-05-01', '2006'),
    ('2007-05-01', '2007'),
    ('2008-05-01', '2008'),
    ('2009-05-01', '2009'),
    ('2010-05-01', '2010'),
)

###Models
class AlumniUser(models.Model):
    username = models.CharField(max_length = 20, unique = True)
    name = models.CharField(max_length = 50)
    email = models.EmailField()
    passwd = models.CharField('Password', max_length = 25)
    degreeType = models.PositiveSmallIntegerField('Degree', choices = degreeTypes)
    department = models.PositiveSmallIntegerField('Department', choices = departments)
    yearOfPassing = models.DateField('Year Of Passing', choices = yearOfPassing)
    isEmailVerified = models.BooleanField(default = False)
    nonce = models.CharField(max_length = 32)

class BlogPost(models.Model):
    title = models.CharField(max_length = 50)
    titleSlug = models.SlugField(max_length = 50)
    shortText = models.CharField(max_length = 200)
    fullText = models.TextField()
    dateOfPublishing = models.DateField('Publish On')
    def __unicode__(self):
        return self.title

###Forms
class AlumniUserForm(forms.ModelForm):
    username = forms.CharField(min_length = 7, max_length = 20)
    class Meta:
        model = AlumniUser
        #fields = ['username', 'name', 'email', 'degreeType', 'department', 'yearOfPassing']
        exclude = ['isEmailVerified', 'nonce']

###Signals
pre_save.connect(populateShortText, sender = BlogPost)
