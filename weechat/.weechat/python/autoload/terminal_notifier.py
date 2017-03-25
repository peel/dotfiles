# This weechat plugin sends OS X notifications for weechat messages
#
# Install terminal-notifier, no other configuration needed.
#
# History:
#
# Version 1.0.0: initial release
# Version 1.0.1: fix escape characters which broke terminal-notifier
# Version 1.0.2: set the nick as the title of the notification
# Version 1.0.3: throttle notification calls
# Version 1.1.0: switch to applescript for notifications
# Version 1.1.1: handle script not existing

import datetime
import distutils.spawn
import functools
import subprocess
import weechat


# https://gist.github.com/ChrisTM/5834503
class throttle(object):
    """
    Decorator that prevents a function from being called more than once every
    time period.

    To create a function that cannot be called more than once a minute:

        @throttle(minutes=1)
        def my_fun():
            pass
    """
    def __init__(self, seconds=0, minutes=0, hours=0):
        self.throttle_period = datetime.timedelta(
            seconds=seconds, minutes=minutes, hours=hours
        )
        self.time_of_last_call = datetime.datetime.min

    def __call__(self, fn):
        @functools.wraps(fn)
        def wrapper(*args, **kwargs):
            now = datetime.datetime.now()
            time_since_last_call = now - self.time_of_last_call
            if time_since_last_call > self.throttle_period:
                self.time_of_last_call = now
                return fn(*args, **kwargs)
        return wrapper


@throttle(seconds=1)
def notify(data, signal, signal_data):
    separated = signal_data.split("\t")
    try:
        title = separated[0]
    except IndexError:
        title = "WeeChat"

    message = "\t".join(separated[1:])
    notification = """display notification "%s" with title "%s" """ % (message,
                                                                       title)
    process = subprocess.Popen("osascript -".split(), stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    process.communicate(notification)
    exit_code = process.wait()
    if exit_code == 0:
        return weechat.WEECHAT_RC_OK
    else:
        return weechat.WEECHAT_RC_ERROR


def main():
    if not weechat.register("terminal_notifier", "Keith Smiley", "1.1.0", "MIT",
                            "Get OS X notifications for messages", "", ""):
        return weechat.WEECHAT_RC_ERROR

    if distutils.spawn.find_executable("osascript") is None:
        return weechat.WEECHAT_RC_OK

    weechat.hook_signal("weechat_pv", "notify", "")
    weechat.hook_signal("weechat_highlight", "notify", "")

    return weechat.WEECHAT_RC_OK

if __name__ == "__main__":
    main()
