#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2024 Enze Chi
#
# Author:     Enze Chi <Enze.Chi@gmailcom>
# Maintainer: Enze Chi <Enze.Chi@gmailcom>
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
from epc.server import ThreadingEPCServer
from utils import (close_epc_client, eval_in_emacs, get_emacs_var,
                   message_emacs, init_epc_client, logger, get_command_result)
import google.generativeai as genai


def threaded(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        thread = threading.Thread(target=func, args=args, kwargs=kwargs)
        thread.start()
        if hasattr(args[0], 'thread_queue'):
            args[0].thread_queue.append(thread)
    return wrapper


class Gemini:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('localhost', 0), log_traceback=True)
        self.server.allow_reuse_address = True

        self.server.register_instance(self)  # register instance functions let elisp side call

        self.token = self.get_token()

        # self.proxy = self.get_emacs_proxy_config()

        self.model = genai.GenerativeModel('gemini-pro')

        self.chatbot = self.model.start_chat(history=[])

        # Start EPC server with sub-thread, avoid block main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()

        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        # Pass epc port and webengine codec information to Emacs when first start Gemini.
        eval_in_emacs('gemini--first-start', self.server.server_address[1])

        self.event_loop.join()

    def event_dispatcher(self):
        try:
            while True:
                self.event_queue.get(True)
                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def get_emacs_proxy_config(self):
        proxy = get_emacs_var("gemini-http-proxy")
        return proxy if proxy != "" else None

    def get_token(self):
        token = get_emacs_var("gemini-api-token")
        if token is None:
            message_emacs("Gemini API token is not found, please check the README.")

        return token

    @threaded
    def gemini_chat(self, prompt, buffer):
        response = self.chatbot.send_message(prompt)
        answers = list()

        answers.append(response.text)

        serial_number = 1
        for answer in answers:
            eval_in_emacs("gemini-response", serial_number, answer, buffer)
            serial_number += 1

        eval_in_emacs("gemini-finish-answer", buffer)

    @threaded
    def gemini_text(self, prompt, buffer, text,
                  notify_start, notify_end, begin=0, end=0, func=""):
        message_emacs(notify_start)
        answers = list()

        if text == "":
            content = f"{prompt}"
        else:
            content = f"{prompt}:\n{text}"

        response = self.chatbot.send_message(content)

        answers.append(response.text)

        serial_number = 1
        for answer in answers:
            if func == "":
                eval_in_emacs("gemini-response", serial_number, answer, buffer)
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
        response = self.chatbot.send_message(prompt)
        answers.append(response.text)

        serial_number = 1
        for answer in answers:
            eval_in_emacs("gemini-return-code", serial_number, answer, buffer, begin, end)
            serial_number += 1

        message_emacs("Generate messages done.")

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()


if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("Gemini(sys.argv[1:])")
    else:
        Gemini(sys.argv[1:])
