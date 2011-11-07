from django.shortcuts import render_to_response, get_object_or_404
from django.http import Http404, HttpResponseRedirect

from iitg.alumni.models import AlumniUser, BlogPost, AlumniUserForm

def registerUser(request):
    if request.method == 'POST':
        form = AlumniUserForm(request.POST)
        if form.is_valid():
            username = form.cleaned_data['username']
            name = form.cleaned_data['name']
            email = form.cleaned_data['email']
            passwd = form.cleaned_data['passwd']
            degreeType = form.cleaned_data['degreeType']
            department = form.cleaned_data['department']
            yearOfPassing = form.cleaned_data['yearOfPassing']

            #generate nonce
            import md5
            nonceBase = username + name + email + passwd
            nonce = md5.new(nonceBase).hexdigest()

            #compose message and send mail
            def prepareMessage():
                domain = 'http://localhost:8000'
                message = 'Dear ' + name.capitalize() + ',\n\n\n'
                message += 'Please click on link given below to complete your registration with the IITG alumni club!\n\n'
                url = domain + '/user/verify/' + username + '/' + nonce + '/'
                message += '<a href="' + url + '">' + url + '</a>\n\n'
                message += 'If you cannot click on the link above, please copy paste the following into your browser\n\n'
                message += url
                return message

            message = prepareMessage()
            subject = 'Please complete registration to IITG alumni club'
            sender = 'alumni@iitg.ernet.in'
            recipients = [email,]

            from django.core.mail import send_mail
            #send_mail(subject, message, sender, recipients)

            #save form contents to db
            user = form.save(commit = False)
            user.isEmailVerified = False
            user.nonce = nonce
            user.save()

            return HttpResponseRedirect('/user/register/thanks/')
    else:
        form = AlumniUserForm()

    return render_to_response('alumni/registerUser.html', {'form': form,})
def verifyUser(request, userName, nonce = None):
    user = AlumniUser.objects.get(username = userName)
    if user.nonce == nonce:
        user.isEmailVerified = True
        user.save()
        return render_to_response('alumni/verifiedUser.html', {'verified': True})
    return render_to_response('alumni/verifiedUser.html', {'verified': False})
'''def modifyUser(request, userName):
    pass
def allUsers(request):
    pass
def userPage(request, pageId):
    pass
def userDetail(request, pageId):
    pass'''
