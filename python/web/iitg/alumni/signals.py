from django.db.models.signals import pre_save, post_save, post_delete

def populateShortText(sender, instance, **kwargs):
    instance.shortText = instance.fullText[:200]

