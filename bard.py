import queue
import threading
import traceback
import sys
from functools import wraps
from Bard import Chatbot
from epc.server import ThreadingEPCServer
from utils import (close_epc_client, eval_in_emacs, get_emacs_var,
                   message_emacs, init_epc_client, logger)


def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper


class Bard:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        self.server.allow_reuse_address = True

        self.server.register_instance(self)  # register instance functions let elisp side call

        self.token = get_emacs_var("bard-cookie-token")

        self.chatbot = Chatbot(self.token)

        # Start EPC server with sub-thread, avoid block main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # Pass epc port and webengine codec information to Emacs when first start bard.
        eval_in_emacs('bard--first-start', self.server.server_address[1])

        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                self.event_queue.get(True)
                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

    def bard_chat(self, prompt, buffer):
        content = self.chatbot.ask(prompt)
        self.answers = list()

        responses = content['choices']
        for response in responses:
            self.answers.append(response['content'][0])

        serial_number = 1
        for answer in self.answers:
            eval_in_emacs("bard-insert-answer", serial_number, answer, buffer)
            serial_number += 1

        eval_in_emacs("bard-finish-answer", buffer)


if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Bard(sys.argv[1:])")
    else:
        Bard(sys.argv[1:])
