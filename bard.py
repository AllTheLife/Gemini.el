#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2023 AllTheLife
#
# Author:     AllTheLife <xjn208930@gmail.com>
# Maintainer: AllTheLife <xjn208930@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import queue
import threading
import traceback
import sys
import os
from functools import wraps
from Bard import Chatbot
from epc.server import ThreadingEPCServer
from utils import (close_epc_client, eval_in_emacs, get_emacs_var,
                   message_emacs, init_epc_client, logger, get_command_result)


def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper

def browser_cookies():
    import browser_cookie3
    cookiejar = None
    cookie_token = None
    cookie_token_ts = None
    google_com = "google.com"

    try:
        cookiejar = browser_cookie3.chrome(domain_name=google_com)
    except Exception as e:
        print(f"get cookie from Chrome failed, {str(e)}", file=sys.stderr)

    if not cookiejar:
        try:
            cookiejar = browser_cookie3.chromium(domain_name=google_com)
        except Exception as e:
            print(f"get cookie from Chromium failed, {str(e)}", file=sys.stderr)

    if not cookiejar:
        try:
            cookiejar = browser_cookie3.brave(domain_name=google_com)
        except Exception as e:
            print(f"get cookie from Brave failed, {str(e)}", file=sys.stderr)

    if not cookiejar:
        try:
            cookiejar = browser_cookie3.firefox(domain_name=google_com)
        except Exception as e:
            print(f"get cookie from Firefox failed, {str(e)}", file=sys.stderr)

    if not cookiejar:
        try:
            cookiejar = browser_cookie3.edge(domain_name=google_com)
        except Exception as e:
            print(f"get cookie from Microsoft Edge failed, {str(e)}", file=sys.stderr)
            return None, None

    for cookie in cookiejar:

        if cookie_token and cookie_token_ts:
            break

        if cookie.name == "__Secure-1PSID":
            cookie_token = cookie.value

        if cookie.name == "__Secure-1PSIDCC":
            cookie_token_ts = cookie.value

    return cookie_token, cookie_token_ts

class Bard:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        self.server.allow_reuse_address = True

        self.server.register_instance(self)  # register instance functions let elisp side call

        # self.token = get_emacs_var("bard-cookie-token")
        self.token, self.token_ts = self.get_cookie_token()

        self.proxy = self.get_emacs_proxy_config()

        self.chatbot = Chatbot(self.token, self.token_ts, proxy=self.proxy)

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

    def get_emacs_proxy_config(self):
        proxy = get_emacs_var("bard-http-proxy")
        return proxy if proxy != "" else None

    def get_cookie_token(self):
        bard_cookie_token_file_path = os.path.expanduser(get_emacs_var("bard-cookie-token-path"))
        cookie_token = None
        cookie_token_ts = None
        if os.path.exists(bard_cookie_token_file_path):
            with open(bard_cookie_token_file_path, "r") as f:
                try:
                    token, ts = f.read().strip().split(",", 2)
                    if token != "" and ts != "":
                        cookie_token = token
                        cookie_token_ts = ts
                except Exception as e:
                    message_emacs(f"Bard cookie token and token_ts not found: {str(e)}")
        else:
            cookie_token = os.environ.get("BARD_TOKEN")
            cookie_token_ts = os.environ.get("BARD_TOKEN_TS")

        if cookie_token is None or cookie_token_ts is None:
            try:
                import browser_cookie3
                message_emacs("No available cookie value read from configuration. Trying to automatically extract cookies from the browser")
                cookie_token, cookie_token_ts = browser_cookies()
            except ImportError:
                pass

        if cookie_token is None or cookie_token_ts is None:
            message_emacs("Bard cookie token or token_ts not found, please check the README.")

        return cookie_token, cookie_token_ts

    @threaded
    def bard_chat(self, prompt, buffer):
        content = self.chatbot.ask(prompt)
        answers = list()

        responses = content['choices']
        for response in responses:
            answers.append(response['content'][0])

        serial_number = 1
        for answer in answers:
            eval_in_emacs("bard-response", serial_number, answer, buffer)
            serial_number += 1

        eval_in_emacs("bard-finish-answer", buffer)

    @threaded
    def bard_text(self, prompt, buffer, text,
                  notify_start, notify_end, begin=0, end=0, func=""):
        message_emacs(notify_start)
        answers = list()

        if text == "":
            content = f"{prompt}"
        else:
            content = f"{prompt}:\n{text}"

        responses = self.chatbot.ask(content)

        responses = responses['choices']

        for response in responses:
            answers.append(response['content'][0])

        serial_number = 1
        for answer in answers:
            if func == "":
                eval_in_emacs("bard-response", serial_number, answer, buffer)
            else:
                eval_in_emacs(func, serial_number, answer, buffer, begin, end)
            serial_number += 1

        message_emacs(notify_end)

    @threaded
    def git_commit(self, dir, buffer, begin, end):
        message_emacs("Generating...")

        diff_string = get_command_result(f"cd {dir} ; git diff")
        answers = list()
        prompt = f"""
Please generate a patch title for the following diff content, mainly analyze the content starting with - or + at the beginning of the line, with a concise and informative summary instead of a mechanical list. The title should not exceed 100 characters in length, and the format of the words in the title should be: the first word capitalized, all other words lowercase, unless they are proper nouns, if the diff content starts with 'Subproject commit', you extract the submodule name 'xxx', and reply 'Update xxx modules'. Please just put the commit message in code block and don't give any explanations or instructions.
\n
{ diff_string }
"""
        responses = self.chatbot.ask(prompt)
        responses = responses['choices']
        for response in responses:
            answers.append(response['content'][0])

        serial_number = 1
        for answer in answers:
            eval_in_emacs("bard-return-code", serial_number, answer, buffer, begin, end)
            serial_number += 1

        message_emacs("Generate messages done.")

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()


if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Bard(sys.argv[1:])")
    else:
        Bard(sys.argv[1:])
