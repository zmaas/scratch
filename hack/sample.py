def respnod(msg):
    if msg in responses:
        bot_message = responses[msg]
    else:
        bot_message = responses["default"]
    return bot_message

