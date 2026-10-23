from django.db import models


class Order(models.Model):
    name = models.TextField()


def list_orders(request):
    return Order.objects.all()
