#!/usr/bin/env python
import os
import pika

connection = pika.BlockingConnection(pika.ConnectionParameters(
               'localhost'))
channel = connection.channel()

os.system("sleep 10s")
